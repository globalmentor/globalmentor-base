package com.garretwilson.beans;

import java.beans.*;

/**A Java Beans vetoable change listener retrofitted to use generics to cast to proper value type.
An implementing class must override the non-generic property change method and call the generic version,
so that the property change can be noted by either method. 
The abstract version of this class, {@link AbstractGenericVetoableChangeListener}, will be used most often.
@param <V> The type of property value.
@author Garret Wilson
*/
public interface GenericVetoableChangeListener<V> extends VetoableChangeListener
{

	/**Called when a constrained property is changed.
	@param genericPropertyChangeEvent An event object describing the event source, the property that is changing, and its old and new values.
	@exception PropertyVetoException if the recipient wishes the property change to be rolled back.
	*/
	public void vetoableChange(final GenericPropertyChangeEvent<V> genericPropertyChangeEvent) throws PropertyVetoException; 
}
