/*
 * Copyright © 2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.model;

import java.util.concurrent.*;

import com.globalmentor.collections.AbstractRunnableBlockingQueueConsumer;
import com.globalmentor.log.Log;

/**
 * A class for managing and executing operations.
 * 
 * <p>
 * This is a lightweight manager of task-like objects that are to be executed serially.
 * </p>
 * 
 * <p>
 * An operation manager is similar to the Java AWT thread; it allows multiple operations to be queued and executed serially. An operation can be queued by
 * calling {@link #schedule(Operation)}. The operation, if not canceled, will eventually be started using {@link Operation#run()}.
 * </p>
 * 
 @author Garret Wilson
 */
public class OperationManager
{

	/** The queue for scheduling operations. */
	private final BlockingQueue<Operation> operationQueue;

	/** @return The queue for scheduling operations. */
	protected BlockingQueue<Operation> getOperationQueue()
	{
		return operationQueue;
	}

	/** The consumer that executes pending operations. */
	private final AbstractRunnableBlockingQueueConsumer<Operation> operationConsumer;

	/** @return The consumer that executes pending operations. */
	protected Consumer<Operation> getOperationConsumer()
	{
		return operationConsumer;
	}

	/** The thread for asynchronous execution. */
	private final Thread executionThread;

	/** The thread for asynchronous execution. */
	protected Thread getExecutionThread()
	{
		return executionThread;
	}

	/** Default constructor. */
	public OperationManager()
	{
		operationQueue = new LinkedBlockingQueue<Operation>(); //create a new queue for scheduling operations
		operationConsumer = new OperationConsumer(operationQueue); //create a consumer of operations
		executionThread = new Thread(operationConsumer, getClass().getName()); //create a new send thread
		executionThread.setDaemon(true); //make the execution thread a daemon so that it won't hold up the application when the system shuts down
		executionThread.start(); //start the execution thread
	}

	/**
	 * Schedules an operation for later, serial execution.
	 * @param operation The operation to schedule.
	 * @throws NullPointerException if the given operation is <code>null</code>.
	 * @throws IllegalStateException if the operation cannot be scheduled at this time due to queue capacity restrictions.
	 */
	public void schedule(final Operation operation)
	{
		getOperationQueue().add(operation);
	}

	/**
	 * Consumer for executing operations. This implementation ignores canceled operations and logs any errors.
	 * 
	 * @author Garret Wilson
	 * @see Log#error(Object...)
	 */
	public static class OperationConsumer extends AbstractRunnableBlockingQueueConsumer<Operation>
	{

		/**
		 * Blocking queue constructor.
		 * @param blockingQueue The blocking queue from which elements will be consumed.
		 * @throws NullPointerException if the given blocking queue is <code>null</code>.
		 */
		public OperationConsumer(final BlockingQueue<Operation> blockingQueue)
		{
			super(blockingQueue);
		}

		@Override
		public void consume(final Operation operation)
		{
			if(!operation.isCanceled()) //if the operation isn't canceled
			{
				try
				{
					operation.run(); //run the operation
				}
				catch(final Throwable throwable)
				{
					Log.error(throwable);
				}
			}
		}

	}

}
