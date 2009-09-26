/*
 * Copyright © 1996-2009 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.garretwilson.awt;

import java.awt.EventQueue;
import java.lang.reflect.InvocationTargetException;

/**Utility methods for working with the AWT event queue.
@author Garret Wilson
*/
public class EventQueues
{
	/**Causes the object to have its {@link Runnable#run()} method called in the 
		dispatch thread of the EventQueue. This will happen after all pending
		events are processed. The call does not block and wait for the execution
		to occur.
	@see EventQueue#invokeLater(Runnable)
	*/
	public static void invokeInEventQueue(final Runnable runnable)
	{
		if(EventQueue.isDispatchThread())	//if we're in the event dispatch thread already
		{
			runnable.run();	//just run the object in our thread, since we're already in the event thread
		}
		else	//if we're not in the AWT event thread
		{
			EventQueue.invokeLater(runnable);	//schedule the object to be invoked later in the event thread
		}
	}

	/**Causes the object to have its <code>run()</code> method called in the 
		dispatch thread of the EventQueue. This will happen after all pending
		events are processed. The call blocks until this has happened.
	<p>If the calling thread is the event queue, the objects <code>run()</code>
		method will be called in the calling thread. Contrast this with the
		<code>EventQueue</code> version, which throws an error under this
		condition.</p>
	@see EventQueue#invokeAndWait(Runnable)
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
