package com.garretwilson.awt;

import java.awt.EventQueue;
import java.lang.reflect.InvocationTargetException;

/**Utility methods for working with the AWT event queue.
@author Garret Wilson
*/
public class EventQueueUtilities
{
	/**Causes the object to have its <code>run()</code> method called in the 
		dispatch thread of the EventQueue. This will happen after all pending
		events are processed. The call blocks until this has happened.
	<p>If the calling thread is the event queue, the objects <code>run()</code>
		method will be called in the calling thread. Contrast this with the
		<code>EventQueue</code> version, which throws an error under this
		condition.</p>
	@see EventQueue#invokeAndWait
	*/
	public static void invokeInEventQueueAndWait(final Runnable runnable) throws InterruptedException, InvocationTargetException
	{
		if(EventQueue.isDispatchThread())	//if we're in the event dispatch thread already
		{
			runnable.run();	//just run the object in our thread, since we're already in the event thread
		}
		else	//if we're not in the AWT event thread
		{
			EventQueue.invokeAndWait(runnable);	//make sure the object is invoked in the event thread
		}
	}
}
