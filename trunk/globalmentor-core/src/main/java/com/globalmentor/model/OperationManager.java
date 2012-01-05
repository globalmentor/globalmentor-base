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

import static com.globalmentor.java.Objects.*;

import java.util.*;
import java.util.concurrent.*;

import com.globalmentor.java.Objects;
import com.globalmentor.log.Log;
import com.globalmentor.time.*;

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
	private final BlockingQueue<ScheduledOperation> scheduledOperationQueue;

	/** @return The queue for scheduling operations. */
	protected BlockingQueue<ScheduledOperation> getScheduledOperationQueue()
	{
		return scheduledOperationQueue;
	}

	/** The worker that executes scheduled operations. */
	private final Runnable scheduledOperationWorker;

	/** @return The worker that executes scheduled operations. */
	protected Runnable getScheduledOperationWorker()
	{
		return scheduledOperationWorker;
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
		scheduledOperationQueue = new LinkedBlockingQueue<ScheduledOperation>(); //create a new queue for scheduling operations
		scheduledOperationWorker = new ScheduledOperationWorker(scheduledOperationQueue); //create a consumer of operations
		executionThread = new Thread(scheduledOperationWorker, getClass().getName()); //create a new send thread
		executionThread.setDaemon(true); //make the execution thread a daemon so that it won't hold up the application when the system shuts down
		executionThread.start(); //start the execution thread
	}

	/**
	 * Schedules an operation for later, serial execution once with no delay.
	 * @param operation The operation to schedule.
	 * @throws NullPointerException if the given operation is <code>null</code>.
	 * @throws IllegalStateException if the operation cannot be scheduled at this time due to queue capacity restrictions.
	 */
	public void schedule(final Operation operation)
	{
		schedule(operation, false);
	}

	/**
	 * Schedules an operation for later, serial execution once with the given delay.
	 * @param operation The operation to schedule.
	 * @param delayTime The delay past the scheduled time before the operation should begin (which may be 0).
	 * @throws NullPointerException if the given operation and/or delay time is <code>null</code>.
	 * @throws IllegalStateException if the operation cannot be scheduled at this time due to queue capacity restrictions.
	 */
	public void schedule(final Operation operation, final Duration delayTime)
	{
		schedule(operation, delayTime, false);
	}

	/**
	 * Schedules an operation for later, serial execution with no delay.
	 * @param operation The operation to schedule.
	 * @param repeated Whether this operation should be repeated.
	 * @throws NullPointerException if the given operation is <code>null</code>.
	 * @throws IllegalStateException if the operation cannot be scheduled at this time due to queue capacity restrictions.
	 */
	public void schedule(final Operation operation, final boolean repeated)
	{
		schedule(operation, Duration.NO_DURATION, repeated);
	}

	/**
	 * Schedules an operation for later, serial execution.
	 * @param operation The operation to schedule.
	 * @param delayTime The delay past the scheduled time before the operation should begin (which may be 0).
	 * @param repeated Whether this operation should be repeated.
	 * @throws NullPointerException if the given operation and/or delay time is <code>null</code>.
	 * @throws IllegalStateException if the operation cannot be scheduled at this time due to queue capacity restrictions.
	 */
	public void schedule(final Operation operation, final Duration delayTime, final boolean repeated)
	{
		getScheduledOperationQueue().add(new ScheduledOperation(operation, delayTime, repeated));
	}

	/**
	 * Encapsulates an operation that has been scheduled, along with any delay or repeat.
	 * @author Garret Wilson
	 */
	protected static class ScheduledOperation
	{
		/** The scheduled operation. */
		private final Operation operation;

		/** @return The scheduled operation. */
		public Operation getOperation()
		{
			return operation;
		}

		/** The time the operation was scheduled. */
		private Time scheduledTime;

		/** @return The time the operation was scheduled. */
		public Time getScheduledTime()
		{
			return scheduledTime;
		}

		/**
		 * Resets the scheduled time to the current time.
		 * @return The new scheduled time.
		 */
		public Time resetScheduledTime()
		{
			scheduledTime = new Time();
			return scheduledTime;
		}

		/** The delay past the scheduled time before the operation should begin (which may be 0). */
		private final Duration delayDuration;

		/** @return The delay past the scheduled time before the operation should begin (which may be 0). */
		public Duration getDelayDuration()
		{
			return delayDuration;
		}

		/** Whether this operation should be repeated. */
		private final boolean repeated;

		/** @return Whether this operation should be repeated. */
		public boolean isRepeated()
		{
			return repeated;
		}

		/**
		 * Constructor.
		 * @param operation The scheduled operation.
		 * @param delayTime The delay past the scheduled time before the operation should begin (which may be 0).
		 * @param repeated Whether this operation should be repeated.
		 * @throws NullPointerException if the given operation and/or delay time is <code>null</code>.
		 */
		public ScheduledOperation(final Operation operation, final Duration delayTime, final boolean repeated)
		{
			this.operation = checkInstance(operation);
			this.scheduledTime = new Time();
			this.delayDuration = checkInstance(delayTime);
			this.repeated = repeated;
		}
	}

	/**
	 * Worker for processing scheduled operations. This implementation ignores canceled operations and logs any errors.
	 * @author Garret Wilson
	 * @see Log#error(Object...)
	 */
	public static class ScheduledOperationWorker implements Runnable
	{

		/** The blocking queue holding the incoming scheduled operations. */
		private final BlockingQueue<ScheduledOperation> blockingQueue;

		/**
		 * Blocking queue constructor.
		 * @param blockingQueue The blocking queue from which scheduled operations will be processed.
		 * @throws NullPointerException if the given blocking queue is <code>null</code>.
		 */
		public ScheduledOperationWorker(final BlockingQueue<ScheduledOperation> blockingQueue)
		{
			this.blockingQueue = Objects.checkInstance(blockingQueue);
		}

		@Override
		public void run()
		{
			final List<ScheduledOperation> scheduledOperations = new LinkedList<ScheduledOperation>(); //The list of currently known scheduled operations.
			while(!Thread.interrupted()) //keep polling until interrupted
			{
				try
				{
					Duration timeoutDuration = Duration.MAX_DURATION; //we'll time out based upon the soonest delayed operation
					final Time now = new Time(); //get the current time
					final Iterator<ScheduledOperation> scheduledOperationIterator = scheduledOperations.iterator();
					while(scheduledOperationIterator.hasNext()) //process the current scheduled operations
					{
						final ScheduledOperation scheduledOperation = scheduledOperationIterator.next();
						final Duration duration = now.subtract(scheduledOperation.getScheduledTime()); //see how long it's been since this operation was scheduled
						final Duration delayDuration = scheduledOperation.getDelayDuration(); //find out how long the operation should be delayed
						final Duration remainingDuration = delayDuration.subtract(duration); //see how much time is remaining before this operation should be executed
						if(remainingDuration.getTime() <= 0) //if the required amount of time has passed
						{
							final Operation operation = scheduledOperation.getOperation(); //get the operation
							if(!operation.isCanceled()) //if the operation isn't canceled
							{
								try
								{
									operation.run(); //run the operation
								}
								catch(final Throwable throwable) //if the operation causes an error
								{
									Log.error(throwable);
								}
								if(scheduledOperation.isRepeated() && !operation.isCanceled()) //if the scheduled operation should be repeated (if the operation has already been canceled, don't bother---remove it already)
								{
									scheduledOperation.resetScheduledTime(); //reset the scheduled time of the operation and leave it for next time
									if(delayDuration.compareTo(timeoutDuration) < 0) //if the time we should wait before the next time this operation is executed is shorter than what we have on record
									{
										timeoutDuration = delayDuration; //lower our minimum poll timeout so we can come back and execute this operation in time
									}
								}
								else
								//if the scheduled operation should not be repeated
								{
									scheduledOperationIterator.remove(); //remove the operation from our list
								}
							}
							else
							//if the operation is canceled
							{
								scheduledOperationIterator.remove(); //remove the operation from our list
							}
						}
						else
						//if there is still time before this operation should be executed
						{
							if(remainingDuration.compareTo(timeoutDuration) < 0) //if this is a shorter duration time than for the other operations
							{
								timeoutDuration = remainingDuration; //lower our minimum poll timeout so we can come back and execute this operation in time
							}
						}
					}
					//poll for a new scheduled operation, timing out at the minimum duration we found
					final ScheduledOperation scheduledOperation = blockingQueue.poll(timeoutDuration.getTime(), TimeUnit.MILLISECONDS);
					if(scheduledOperation != null) //if we got a new scheduled operation before timeout
					{
						scheduledOperations.add(scheduledOperation); //add this operation to our list of current scheduled operations; the next time around we'll process all of them, including the new one
					}
				}
				catch(final InterruptedException interruptedException) //if we're interrupted while waiting
				{
					break; //break out of the loop
				}
				catch(final Throwable throwable) //if any other exception occurs
				{
					Log.error(throwable); //log the error and continue
				}
			}
		}
	}

}
