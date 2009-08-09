/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.java;

import java.lang.reflect.UndeclaredThrowableException;

/**Utility methods for threads.
@author Garret Wilson
*/
public class Threads
{

	/**Joins a thread, ignoring any interruptions.
	If any interruptions occur, they will be ignored and joining will be attempted again.
	@param thread The thread to join.
	*/
	public static void join(final Thread thread)
	{
		boolean joined=false;	//we'll note when joining actually finishes
		do
		{
			try
			{
				thread.join();	//join the thread
				joined=true;	//joining was successful
			}
			catch(final InterruptedException interruptedException)	//if joining was interrupted, ignore the interruption and try again
			{
			}
		}
		while(!joined);	//keep joining until successful
	}

	/**Calls a thread serially by first starting the thread and waiting until the thread is finished.
	@param thread The thread to call.
	@see Thread#start()
	@see #join(Thread)
	*/
	public static void call(final Thread thread)
	{
		thread.start();	//start the thread
		join(thread);	//join the thread
	}

	/**Creates a thread in the given thread group and calls the given runnable in the thread, waiting until the thread is finished.
	If an {@link UndeclaredThrowableException} is thrown from the thread, the stack traces of that exception and that exception's cause are filled in and then that exception is rethrown. 
	This version catches any uncaught {@link Error} or {@link RuntimeException} in the thread and rethrows it from this thread after filling in the stack trace.
	If any other {@link Throwable} is thrown in the thread, this method throws an {@link UndeclaredThrowableException} with that throwable as the cause after filling in the throwable's stack trace.
	This method delegates to {@link #call(ThreadGroup, Runnable, UncaughtExceptionHandler)}
	@param threadGroup The thread group in which the thread is to be run.
	@param runnable The runnable interface to run in the thread.
	@return The thread that has been run in the thread group.
	@exception UndeclaredThrowableException if the thread throws any {@link Throwable} that is not an {@link Error} or a {@link RuntimeException}.
	@see #call(Thread)
	*/
/*TODO del; this would be nice for supplementing the stack trace, but fillInStackTrace() erases the current stack trace
	public static Thread call(final ThreadGroup threadGroup, final Runnable runnable)
	{
		final UncaughtExceptionHandler uncaughtExceptionHandler=new UncaughtExceptionHandler();	//create an uncaught exception handler that will store any error
		final Thread thread=call(threadGroup, runnable, uncaughtExceptionHandler);	//call the runnable with our local exception handler
		final Throwable throwable=uncaughtExceptionHandler.getThrowable();	//get the error, if any
		if(throwable!=null)	//if there was an error
		{
			if(throwable instanceof UndeclaredThrowableException)	//if this was an undeclared throwable exception thrown, we can throw it in this thread, but we need to update the decorated throwable's stack trace
			{
				throwable.getCause().fillInStackTrace();	//fill in the stack trace of the cause
				throw (UndeclaredThrowableException)throwable.fillInStackTrace();	//fill in the stack track of the exception itself and send it on
			}
			else if(throwable instanceof Error)	//if this was an Error, we can throw it in this thread
			{
				throw (Error)throwable.fillInStackTrace();	//fill in the stack track of the error and send it on
			}
			if(throwable instanceof RuntimeException)	//if this was a RuntimeException, we can throw that in this thread as well
			{
				throw (RuntimeException)throwable.fillInStackTrace();	//fill in the stack track of the runtime exception and send it on
			}
			else	//if this was any other Exception
			{
				throw new UndeclaredThrowableException(throwable.fillInStackTrace());	//throw a wrapper exception after filling in the throwable's stack trace
			}
		}
		return thread;
	}
*/

	/**Creates a thread in the given thread group and calls the given runnable in the thread, waiting until the thread is finished.
	This version catches any uncaught {@link Error} or {@link RuntimeException} in the thread and rethrows it from this thread.
	If any other {@link Throwable} is thrown in the thread, this method throws an {@link UndeclaredThrowableException} with that throwable as the cause.
	This method delegates to {@link #call(ThreadGroup, Runnable, UncaughtExceptionHandler)}
	@param threadGroup The thread group in which the thread is to be run.
	@param runnable The runnable interface to run in the thread.
	@return The thread that has been run in the thread group.
	@exception UndeclaredThrowableException if the thread throws any {@link Throwable} that is not an {@link Error} or a {@link RuntimeException}.
	@see #call(Thread)
	*/
	public static Thread call(final ThreadGroup threadGroup, final Runnable runnable)
	{
		final UncaughtExceptionHandler uncaughtExceptionHandler=new UncaughtExceptionHandler();	//create an uncaught exception handler that will store any error
		final Thread thread=call(threadGroup, runnable, uncaughtExceptionHandler);	//call the runnable with our local exception handler
		final Throwable throwable=uncaughtExceptionHandler.getThrowable();	//get the error, if any
		if(throwable!=null)	//if there was an error
		{
			if(throwable instanceof Error)	//if this was an Error, we can throw it in this thread
			{
				throw (Error)throwable;
			}
			if(throwable instanceof RuntimeException)	//if this was a RuntimeException, we can throw that in this thread as well
			{
				throw (RuntimeException)throwable;
			}
			else	//if this was any other Exception
			{
				throw new UndeclaredThrowableException(throwable);	//throw a wrapper exception
			}
		}
		return thread;
	}

	/**Creates a thread in the given thread group and calls the given runnable in the thread, waiting until the thread is finished.
	@param threadGroup The thread group in which the thread is to be run.
	@param runnable The runnable interface to run in the thread.
	@param uncaughtExceptionHandler The exception handler for the thread, or <code>null</code> if no exception handler should be installed.
	@return The thread that has been run in the thread group.
	@see #call(Thread)
	*/
	public static Thread call(final ThreadGroup threadGroup, final Runnable runnable, final Thread.UncaughtExceptionHandler uncaughtExceptionHandler)
	{
		final Thread thread=new Thread(threadGroup, runnable);	//create a new thread in the thread group for the runnable
		if(uncaughtExceptionHandler!=null)	//if an exception handler was given
		{
			thread.setUncaughtExceptionHandler(uncaughtExceptionHandler);	//install the exception handler
		}
		call(thread);	//call the thread
		return thread;	//return the thread
	}

	/**Walks up the thread group chain of the current thread to find the thread group of the given type.
	 * @param <TG> The type of thread group to find.
	 * @param threadGroupClass The class of the type of thread group to find.
	 * @return The first thread group of the given type, or <code>null</code> if no thread group of the given type could be found.
	 * @throws NullPointerException if the given thread group class is <code>null</code>.
	 */
	public static <TG extends ThreadGroup> TG getThreadGroup(final Class<TG> threadGroupClass)
	{
		return getThreadGroup(Thread.currentThread(), threadGroupClass);
	}

	/**Walks up the thread group chain of the given thread to find the thread group of the given type.
	 * @param <TG> The type of thread group to find.
	 * @param thread The thread at which the thread group search should begin.
	 * @param threadGroupClass The class of the type of thread group to find.
	 * @return The first thread group of the given type, or <code>null</code> if no thread group of the given type could be found.
	 * @throws NullPointerException if the given thread and/or thread group class is <code>null</code>.
	 */
	public static <TG> TG getThreadGroup(final Thread thread, final Class<TG> threadGroupClass)
	{
		final ThreadGroup threadGroup=thread.getThreadGroup();	//get the thread's thread group
		return threadGroup!=null ? getThreadGroup(threadGroup, threadGroupClass) : null;	//if the thread has a thread group, look for the thread group of the requested type 
	}

	/**Walks up the thread group chain of the given thread group to find the thread group of the given type
	 * or that implements the given interface.
	 * @param <TG> The type of thread group to find.
	 * @param threadGroup The thread group at which the search should begin.
	 * @param threadGroupClass The class of the type of thread group to find.
	 * @return The first thread group of the given type, or <code>null</code> if no thread group of the given type could be found.
	 * @throws NullPointerException if the given thread group and/or thread group class is <code>null</code>.
	 */
	public static <TG> TG getThreadGroup(ThreadGroup threadGroup, final Class<TG> threadGroupClass)
	{
		do
		{
			if(threadGroupClass.isInstance(threadGroup))	//if this is the required thread group type
			{
				return threadGroupClass.cast(threadGroup);	//return the thread group as the type
			}
			threadGroup=threadGroup.getParent();	//check this thread group's parent thread group
		}
		while(threadGroup!=null);	//stop looking if we run out of thread groups
		return null;	//we were unable to find the required thread group
	}

	/**The uncaught exception handler that stores the error for later access.
	@author Garret Wilson
	*/
	private static class UncaughtExceptionHandler implements Thread.UncaughtExceptionHandler
	{
		/**The error that occurred, or <code>null</code> if no error occurred.*/
		private Throwable throwable=null;

			/**@return The error that occurred, or <code>null</code> if no error occurred.*/
			public Throwable getThrowable() {return throwable;}

		/**Invoked when the given thread terminates due to an given uncaught exception.
		@param thread The thread in which the exception occurred.
		@param throwable The exception that occurrred.
		*/
		public void uncaughtException(final Thread thread, final Throwable throwable)
		{
			this.throwable=throwable;	//save the throwable for later
		}
	};

}
