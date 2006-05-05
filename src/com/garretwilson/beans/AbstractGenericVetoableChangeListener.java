package com.garretwilson.beans;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;

import static com.garretwilson.beans.AbstractGenericPropertyChangeListener.getGenericPropertyChangeEvent;

/**A Java Beans vetoable change listener retrofitted to use generics to cast to proper value type.
@param <V> The type of property value.
@author Garret Wilson
*/
public abstract class AbstractGenericVetoableChangeListener<V> implements GenericVetoableChangeListener<V>
{

	/**Called when a constrained property is changed.
	This not-generics version calls the generic version, creating a new event if necessary.
	@param propertyChangeEvent An event object describing the event source, the property that is changing, and its old and new values.
	@exception PropertyVetoException if the recipient wishes the property change to be rolled back.
	@see GenericPropertyChangeListener#propertyChange(GenericPropertyChangeEvent)
	*/
	@SuppressWarnings("unchecked")
	public void vetoableChange(final PropertyChangeEvent propertyChangeEvent) throws PropertyVetoException
	{
		vetoableChange((GenericPropertyChangeEvent<V>)getGenericPropertyChangeEvent(propertyChangeEvent));	//call the generic version of the method with the genericized event object
	}
}
