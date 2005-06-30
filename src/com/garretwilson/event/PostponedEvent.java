package com.garretwilson.event;

import java.util.EventObject;

/**Encapsulation of an event and the means to fire it at a later time.
@param <E> The type of postponed event.
@author Garret Wilson
*/
public interface PostponedEvent<E extends EventObject>
{

	/**@return Whether the postponed event has been fired.*/
	public boolean isFired();

	/**@return The event that has been postponed.*/
	public E getEvent();

	/**Fires the postponed event.
	@exception IllegalStateException if the postponed event has already been fired.
	*/
	public void fireEvent();
}
