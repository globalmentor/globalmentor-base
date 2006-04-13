package com.garretwilson.beans;

/**An interface that an event knows its target, or the object to which the event applies (which may be different than the object that fired the event).
@author Garret Wilson
*/
public interface TargetedEvent
{

	/**Returns the object to which the event applies.
	This may be a different than <dfn>source</dfn>, which is the object that generated this event instance.
	@return The target of the event, or <code>null</code> if the event target is not known.
	*/
	public Object getTarget();
}
