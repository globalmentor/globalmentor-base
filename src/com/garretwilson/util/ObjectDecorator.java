package com.garretwilson.util;

import static com.garretwilson.lang.ObjectUtilities.checkInstance;

/**An object that decorates another object,
	preserving the {@link Object#hashCode()} and {@link Object#equals(Object)} of the decorated object.
Equality is only supported for exact top-level types.
@param <T> The type of object being decorated.
@author Garret Wilson
*/
public class ObjectDecorator<T>
{

	/**The decorated object.*/
	private final T decoratedObject;

		/**@return The decorated object.*/
		protected T getDecoratedObject() {return decoratedObject;}

	/**Decorated object constructor.
	@param decoratedObject The object to decorate.
	@exception NullPointerException if the given object is <code>null</code>.
	*/
	public ObjectDecorator(final T decoratedObject)
	{
		this.decoratedObject=checkInstance(decoratedObject, "Decorated object cannot be null.");
	}

	/**Returns the hash code of this object.
	This version returns the hash code of the decorated object.
	@return The hash code of this object.
	*/
	public int hashCode()
	{
		return getDecoratedObject().hashCode();	//return the hash code of the decorated object
	}

	/**Determines if this object equals another object.
	This version considers the given object equal to this object if it is of the same type
		as this object and this object's decorated object's {@link Object#equals(Object)} method also returns <code>true</code>
		for the objects's decorated object.
	@param object The object to compare with this object.
	@return <code>true</code> if the given object is considered equal to this object.
	*/
	public boolean equals(final Object object)
	{
		return getClass().isInstance(object) && getDecoratedObject().equals(((ObjectDecorator<?>)object).getDecoratedObject());	//see if the object is of this class and our decorated object is equal to its decorated object
	}

	/**Returns a string representation of the object.
	This version returns a string version of the decorated object.
	*/
	public String toString()
	{
		return getDecoratedObject().toString();	//delegate to the decorated object
	}
	
}
