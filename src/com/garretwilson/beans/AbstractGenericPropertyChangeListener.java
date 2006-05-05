package com.garretwilson.beans;

import java.beans.PropertyChangeEvent;

/**A Java Beans property change listener retrofitted to use generics to cast to proper value type.
@param <V> The type of property value.
@author Garret Wilson
*/
public abstract class AbstractGenericPropertyChangeListener<V> implements GenericPropertyChangeListener<V>
{

	/**Called when a bound property is changed.
	This not-generics version calls the generic version, creating a new event if necessary.
	No checks are made at compile time to ensure the given event actually supports the given generic type.
	@param propertyChangeEvent An event object describing the event source, the property that has changed, and its old and new values.
	@see GenericPropertyChangeListener#propertyChange(GenericPropertyChangeEvent)
	*/
	@SuppressWarnings("unchecked")
	public final void propertyChange(final PropertyChangeEvent propertyChangeEvent)
	{
		propertyChange((GenericPropertyChangeEvent<V>)getGenericPropertyChangeEvent(propertyChangeEvent));	//call the generic version of the method with the genericized event object
	}

	/**Converts a property change event to a generics-aware property value change event.
	This is a utility method to facilitate creation of property value change listeners that cannot subclass this class.
	No checks are made at compile time to ensure the given event actually supports the given generic type.
	@param propertyChangeEvent An event object describing the event source, the property that has changed, and its old and new values.
	@return A generics-aware property change event, either cast from the provided object or created from the provided object's values as appropriate.
	*/
	@SuppressWarnings("unchecked")
	public static <T> GenericPropertyChangeEvent<T> getGenericPropertyChangeEvent(final PropertyChangeEvent propertyChangeEvent)
	{
		if(propertyChangeEvent instanceof GenericPropertyChangeEvent)	//if the event is a property value change event
		{
			return (GenericPropertyChangeEvent<T>)propertyChangeEvent;	//cast the event to a property value change event, assuming that it uses the correct generic type (if not, a class cast exception will be thrown later when the value is attempted to be used)
		}
		else	//if the event is a normal property change event
		{
			return new GenericPropertyChangeEvent<T>(propertyChangeEvent);	//create a copy of the event
		}
	}

}
