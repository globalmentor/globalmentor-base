package com.garretwilson.util;

import java.util.*;

/**A queue of objects that, once an object is added to a queue, will process
	the queue contents in a separate thread. Once all objects in the queue have
	been processed, the separate thread will block until more objects are added
	to the queue.
	<p>The queue does not start processing automatically unless constructed with
	the autostart constructor. Otherwise, it must first be started
	by calling its <code>start()</code> method.</p>
	<p>A subclass may override <code>process(Object)</code>, or use the
	<code>ObjectProcessor</code> constructor, specifying itself or another class
	as the object processor.</p>
@author Garret Wilson
*/
public class ThreadedQueue implements Runnable, ObjectProcessor
{

	/**The object that will process objects as they are added to the queue.*/
	private final ObjectProcessor objectProcessor;

	/**The list of objects. This list is not synchronized, so every method that
		accesses it must syncrhonize on the list itself.
	*/
	private final LinkedList queueList=new LinkedList();

	/**The thread used for processing the objects. Also used as an indication
		of whether the queue is started or stopped.
	*/
	private Thread processThread=null;

	/**Default constructor that does not start the processing thread and uses
		this class as an object processor.
	*/
	public ThreadedQueue()
	{
		this(false);  //construct the queue without starting the processing thread
	}

	/**Object processor that does not automatically start the processing thread.
	@param newObjectProcessor The object responsible for processing objects as
		they are added to the queue, or <code>null</code> if this class should
		be used as the object processor.
	*/
	public ThreadedQueue(final ObjectProcessor newObjectProcessor)
	{
		this(newObjectProcessor, false);  //don't start automatically, but set the object processor
	}

	/**Autostart constructor that uses this class as the object processor.
	@param autoStart <code>true</code> if the processing thread should start
		automatically.
	*/
	public ThreadedQueue(final boolean autoStart)
	{
		this(null, autoStart);  //use ourselves as the object processor
	}

	/**Object processor autostart constructor.
	@param newObjectProcessor The object responsible for processing objects as
		they are added to the queue, or <code>null</code> if this class should
		be used as the object processor.
	@param autoStart <code>true</code> if the processing thread should start
		automatically.
	*/
	public ThreadedQueue(final ObjectProcessor newObjectProcessor, final boolean autoStart)
	{
//G***maybe assert the object processor isn't null
		objectProcessor=newObjectProcessor!=null ? newObjectProcessor : this; //save the object processor, or use ourselves if the object processor is null
		if(autoStart) //if we should start the thread automatically
			start();  //start the thread
	}

	/**Cleans up the queue when it is garbage collected.
		Allows the queue processing thread to stop.
	@exception Throwable Thrown if any error occurs during the method.
	*/
	protected void finalize() throws Throwable
	{
		try
		{
			final Thread thread=processThread;  //get the processing thread
			processThread=null; //set the processing thread to null to notify the thread to stop
			thread.interrupt();   //interrupt the thread in case it's waiting on new messages; when it awakes, it will process the remaining messages, see that the process thread is set to null, and end
		}
		finally
		{
			super.finalize(); //always do the default finalizing
		}
	}

	/**Causes this queue to begin processing its contents, if any, in a separate
		thread of execution. If the queue is already started, no action is performed.
//G***del	@exception IllegalThreadStateException Thrown if the processing thread was
//G***del		already started.
	@see java.lang.Thread#start()
	*/
	public synchronized void start()
	{
		if(processThread==null) //if we haven't started
		{
		  processThread=new Thread(this);  //create a new processing thread that will call our run() method
		  processThread.start();  //start the processing thread
		}
	}

	/**Stops the thread that is processing this queue's contents. This method
		blocks until the processing thread has processed all remaining objects
		in the queue. If the queue is already stopped, no action occurs.
	*/
	public synchronized void stop()
	{
		if(processThread!=null) //if we have a processing thread
		{
			final Thread thread=processThread;  //get the processing thread
			processThread=null; //set the processing thread to null to notify the thread to stop
			thread.interrupt();   //interrupt the thread in case it's waiting on new messages; when it awakes, it will process the remaining messages, see that the process thread is set to null, and end
			try
			{
			  thread.join();  //wait for the thread to end
			}
			catch(InterruptedException interruptedException)
			{
				Debug.warn(interruptedException); //G***fix; do we need to loop until we've really joined the thread?
			}
		}
	}

	/**Adds an object to the end of the queue and notifies the processing thread.
	Nothing can be added to the queue while the queue is being started or stopped.
	@param object The object to add to the queue.
	*/
	public synchronized void add(final Object object)
	{
		synchronized(queueList) //don't allow any other threads to modify the list while we access it
		{
			queueList.add(object);  //add the object to the end of the queue
			queueList.notify(); //notify the processing thread that there is a new object waiting
		}
	}

	/**Gets the next object from the queue in first in, first out sequence.
	@return The first object waiting in the queue.
	*/
	public Object get()
	{
		synchronized(queueList) //don't allow any other threads to modify the list while we access it
		{
			return queueList.removeFirst();  //remove and return the first object in the queue
		}
	}

	/**@return The number of objects in the queue.*/
	public int size()
	{
		synchronized(queueList) //don't allow any other threads to modify the list while we access it
		{
			return queueList.size();  //return the size of the queue
		}
	}

	/**The method which executes as the processing thread.*/
	public void run() //G***will this thread be garbage collected when the ThreadedQueue is garbage collected?
	{
		while(processThread!=null) //process objects until the processing thread is set to null, meaning that we should stop
		{
			synchronized(queueList) //don't allow any other threads to modify the list while we access it; this prevents our finding that there are no objects in the quee, but objects being added and our being notified *before* we wait
			{
//G***del				while(queueList.size()==0) //if there are no objects in the queue
				{
					try
					{
					  queueList.wait(); //wait for another object to be added to the queue
					}
					catch(InterruptedException interruptedException) {} //if we're interrupted while waiting, ignore it and keep waiting, unless we have objects in the queue or the thread should end
				}
			}
			try
			{
			  process();  //process the objects
			}
			catch(Throwable throwable)  //don't let any exceptions or anything else shut down the thread; only allow it to be stopped normally
			{
				Debug.warn(throwable);  //there's not much we can do here, but this shouldn't happen unless there is a logic error, so create a warning
			}
		}
	}

	/**Processes any objects in the queue.
		Called from the processing thread when objects are added to the queue.
		It is recommended that the <code>process(Object)</code> method be
		overridden instead of this method.
	*/
	public void process()
	{
		while(size()>0) //while there are more objects in the queue
		{
			objectProcessor.process(get()); //get the next object from the queue and process it, using the registered object processor (which may be this class)
		}
	}

	/**Processes a single object in the queue.
		Called from the processing thread when objects are added to the queue.
		The default constructor specifies this class as the object processor, so
		child classes can override this method to process each object if a separate
		object processor is not specified.
	@param object The object to process.
	*/
	public void process(final Object object)
	{
	}

}