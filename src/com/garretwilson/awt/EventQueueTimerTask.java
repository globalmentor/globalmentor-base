package com.garretwilson.awt;

import java.util.TimerTask;

/**A timer task that ensures its activity runs in the AWT event queue.
<p>Child classes should override <code>runInEventQueue()</code>.</p>
@author Garret Wilson
@see java.util.Timer
@see #runInEventQueue()
*/
public abstract class EventQueueTimerTask extends TimerTask
{

	/**The class that is executed in the AWT event queue.*/
	private final Runnable runnable;

	/**Default constructor.*/
	public EventQueueTimerTask()
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
		EventQueueUtilities.invokeInEventQueue(runnable);	//invoke our code in the event queue
	}

	/**The action to be performed by this timer task in the AWT event queue.*/
	protected abstract void runInEventQueue();

}
