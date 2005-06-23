package com.garretwilson.beans;

import java.beans.PropertyChangeListener;

/**A Java Beans property change listener retrofitted to use generics to cast to proper value type.
An implementing class must override the non-generic property change method and call the generic version,
so that the property change can be noted by either method. 
The abstract version of this class, <code>AbstractPropertyValueChangeListener</code>, will be used most often.
@author Garret Wilson
*/
public interface PropertyValueChangeListener<V> extends PropertyChangeListener
{

	/**Called when a bound property is changed.
	@param propertyValueChangeEvent An event object describing the event source, the property that has changed, and its old and new values.
	*/
	public void propertyValueChange(final PropertyValueChangeEvent<V> propertyValueChangeEvent);

}
