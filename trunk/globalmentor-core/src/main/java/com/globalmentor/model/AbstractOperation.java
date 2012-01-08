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

import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.java.Threads.*;
import static com.globalmentor.model.TaskState.*;

/**
 * Abstract implementation of some operation that can be executed. As a task, this implementation also indicates via {@link #getState()} whether it has been
 * started and/or canceled.
 * 
 * <p>
 * An operation is a {@link Runnable}, but this implementation's {@link #run()} method delegates to {@link #execute()}, the method that should be overridden by
 * implementations and in which the actual execution logic should be placed.
 * </p>
 * 
 * <p>
 * This implementation's state is not governed by any thread locks.
 * </p>
 * 
 * @author Garret Wilson
 */
public abstract class AbstractOperation extends AbstractTask implements Operation, Runnable, Task
{

	/**
	 * {@inheritDoc} This implementation sets the state to {@link TaskState#CANCELED}.
	 * @see #setState(TaskState)
	 */
	@Override
	public void cancel()
	{
		setState(CANCELED);
	}

	/**
	 * {@inheritDoc} This implementation returns whether the state is {@link TaskState#CANCELED}.
	 * @see #getState()
	 */
	@Override
	public boolean isCanceled()
	{
		return getState() == CANCELED;
	}

	/** Method that can be overridden to initialize before execution. */
	protected void initialize()
	{
	}

	/**
	 * {@inheritDoc} This version sets the state to {@link TaskState#INITIALIZE} and then calls {@link #initialize()}. The method then sets the state to
	 * {@link TaskState#INCOMPLETE} and delegates to {@link #execute()}. After successful execution, the task state is set to {@link TaskState#COMPLETE}. If an
	 * error occurs, the task state is set to {@link TaskState#ERROR}.
	 * @throws IllegalStateException if the state is {@link TaskState#INCOMPLETE}.
	 * @see #initialize()
	 */
	@Override
	public final void run()
	{
		checkState(getState() != TaskState.INCOMPLETE, "Task is running.");
		try
		{
			setState(INITIALIZE);
			initialize();
			setState(INCOMPLETE);
			execute();
			setState(COMPLETE);
		}
		catch(final CancelException cancelException) //if the operation was canceled
		{
			setState(CANCELED);
		}
		catch(final Throwable throwable) //if any error occurred
		{
			setState(ERROR);
			rethrow(throwable);
		}
	}

	/**
	 * Executes the operation.
	 * <p>
	 * This method should <em>not</em> change its state by calling {@link #setState(TaskState)}. If cancellation is needed (to prevent recurrence, for example), a
	 * {@link CancelException} should be thrown.
	 * </p>
	 * @throws CancelException if the current operation should be canceled.
	 */
	protected abstract void execute() throws CancelException;

}
