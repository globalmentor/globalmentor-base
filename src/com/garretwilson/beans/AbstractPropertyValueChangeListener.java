package com.garretwilson.beans;

import java.beans.PropertyChangeEvent;

/**A Java Beans property change listener retrofitted to use generics to cast to proper value type.
The abstract version of this class, <code>AbstractPropertyValueChangeListener</code>, will be used most often.
@author Garret Wilson
*/
public abstract class AbstractPropertyValueChangeListener<V> implements PropertyValueChangeListener<V>
{

	/**Called when a bound property is changed.
	This not-generics version calls the generic version, creating a new event if necessary.
	No checks are made at compile time to ensure the given event actually supports the given generic type.
	@param propertyValueChangeEvent An event object describing the event source, the property that has changed, and its old and new values.
	@see PropertyValueChangeListener#propertyValueChange(PropertyValueChangeEvent)
	*/
	@SuppressWarnings("unchecked")
	public final void propertyChange(final PropertyChangeEvent propertyChangeEvent)
	{
		final PropertyValueChangeEvent<V> propertyValueChangeEvent;	//we'll create a property value change event of the correct generic type
		if(propertyChangeEvent instanceof PropertyValueChangeEvent)	//if the event is a property value change event
		{
			propertyValueChangeEvent=(PropertyValueChangeEvent<V>)propertyChangeEvent;	//cast the event to a property value change event, assuming that it uses the correct generic type (if not, a class cast exception will be thrown later when the value is attempted to be used)
		}
		else	//if the event is a normal property change event
		{
			propertyValueChangeEvent=new PropertyValueChangeEvent<V>(propertyChangeEvent);	//create a copy of the event
		}
		propertyValueChange(propertyValueChangeEvent);	//call the generic version of the method with the genericized event object
	}

}
