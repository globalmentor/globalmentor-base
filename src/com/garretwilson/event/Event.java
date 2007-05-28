package com.garretwilson.event;

import java.util.EventObject;

/**An event interface.
This interface is useful because Java's included {@link EventObject} is an abstract class, not an interface.
@author Garret Wilson
*/
public interface Event
{

	/**The object on which the event initially occurred.
	@return The object on which the event initially occurred.
	*/
	public Object getSource();

}
