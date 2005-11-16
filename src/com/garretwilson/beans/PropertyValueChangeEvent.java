package com.garretwilson.beans;

import java.beans.PropertyChangeEvent;

/**A propety value change event is a Java Beans property change event retrofitted to use generics to cast to proper value type.
@param <V> The type of property value.
@author Garret Wilson
*/
public class PropertyValueChangeEvent<V> extends PropertyChangeEvent
{

	/**Source, property name, with old and new value constructor.
	@param source The bean that fired the event.
	@param propertyName The programmatic name of the property that was changed.
	@param oldValue The old value of the property, or <code>null</code> if no old value is not available.
	@param newValue The new value of the property, or <code>null</code> if the new value is not available.
	*/
	public PropertyValueChangeEvent(final Object source, final String propertyName, final V oldValue, V newValue)
	{
		super(source, propertyName, oldValue, newValue);	//construct the parent class
	}

	/**Property change event copy constructor
	@param propertyChangeEvent A property change event the values of which will later cast to this class' generic type.
	*/
	public PropertyValueChangeEvent(final PropertyChangeEvent propertyChangeEvent)
	{
		super(propertyChangeEvent.getSource(), propertyChangeEvent.getPropertyName(), propertyChangeEvent.getOldValue(), propertyChangeEvent.getNewValue());	//construct the parent class with identical values
		setPropagationId(propertyChangeEvent.getPropagationId());	//update our propagation ID to match
	}

	/**@return The old value of the property, or <code>null</code> if the old value is not available.*/
	@SuppressWarnings("unchecked")
	public V getOldValue()
	{
		return (V)super.getOldValue();	//cast the value to the appropriate generic type
	}

	/**@return The new value of the property, or <code>null</code> if the new value is not available.*/
	@SuppressWarnings("unchecked")
	public V getNewValue()
	{
		return (V)super.getNewValue();	//cast the value to the appropriate generic type
	}

}
