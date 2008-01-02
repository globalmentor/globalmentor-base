package com.garretwilson.util;

import static com.globalmentor.java.Objects.checkInstance;

/**An object that decorates another object, preserving the {@link Object#hashCode()} and {@link Object#equals(Object)} of the decorated object.
Equality is only supported for exact top-level types.
This class does not permit a <code>null</code> decorated object.
@param <T> The type of object being decorated.
@author Garret Wilson
*/
public class ObjectDecorator<T> extends AbstractObjectDecorator<T>
{

	/**Decorated object constructor.
	@param decoratedObject The object to decorate.
	@exception NullPointerException if the given object is <code>null</code>.
	*/
	public ObjectDecorator(final T decoratedObject)
	{
		super(checkInstance(decoratedObject, "Decorated object cannot be null."));	//construct the parent class, making sure the decorated object is not null
	}

}
