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

import java.util.*;

/**A timer task that ensures its activity runs in the AWT event queue.
<p>Child classes should override {@link #runInEventQueue()}.</p>
@author Garret Wilson
@see Timer
@see #runInEventQueue()
*/
public abstract class AbstractEventQueueTimerTask extends TimerTask
{

	/**The class that is executed in the AWT event queue.*/
	private final Runnable runnable;

	/**Default constructor.*/
	public AbstractEventQueueTimerTask()
	{
		runnable=new Runnable()	//create a new runnable class that will simply call runInEventQueue()
				{
					public void run()	//when this runnable is ran
					{
						runInEventQueue();	//run the code in runInEventQueue()
					}
				};
	}

	/**Executes the task in the AWT event queue.
	@see #runInEventQueue()
	*/
	public final void run()
	{
		EventQueues.invokeInEventQueue(runnable);	//invoke our code in the event queue
	}

	/**The action to be performed by this timer task in the AWT event queue.*/
	protected abstract void runInEventQueue();

}
