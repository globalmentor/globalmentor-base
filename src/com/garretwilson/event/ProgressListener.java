package com.garretwilson.event;

import java.util.EventListener;

/**Represents an object which monitors the progress of a particular task.
@author Garret Wilson
*/
public interface ProgressListener extends EventListener
{
	/**Invoked when progress has been made.
	@param progressEvent The event object representing the progress made.
	*/
	public void progressed(final ProgressEvent progressEvent);
}
