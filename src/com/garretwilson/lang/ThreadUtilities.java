package com.garretwilson.lang;

/**Utility methods for threads.
@author Garret Wilson
*/
public class ThreadUtilities
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
	@param threadGroup The thread group in which the thread is to be run.
	@param runnable The runnable interface to run in the thread.
	@return The thread that has been run in the thread group.
	@see #call(Thread)
	*/
	public static Thread call(final ThreadGroup threadGroup, final Runnable runnable)
	{
		final Thread thread=new Thread(threadGroup, runnable);	//create a new thread in the thread group for the runnable
		call(thread);	//call the thread
		return thread;	//return the thread
	}
}
