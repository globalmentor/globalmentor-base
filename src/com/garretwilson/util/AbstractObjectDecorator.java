package com.garretwilson.util;

import com.globalmentor.java.Objects;

/**An object that decorates another object, preserving the {@link Object#hashCode()} and {@link Object#equals(Object)} of the decorated object.
Equality is only supported for exact top-level types.
@param <T> The type of object being decorated.
@author Garret Wilson
*/
public abstract class AbstractObjectDecorator<T>
{

	/**The decorated object.*/
	private T object;
	
		/**@return The decorated object.*/
		protected T getObject() {return object;}

		/**Sets the decorated object.
		@param decoratedObject The object to decorate.
		*/
		protected void setObject(final T decoratedObject) {this.object=decoratedObject;}

	/**Decorated object constructor.
	@param decoratedObject The object to decorate.
	*/
	public AbstractObjectDecorator(final T decoratedObject)
	{
		this.object=decoratedObject;	//save the decorated object
	}
	
	/**Returns the hash code of this object.
	This version returns the hash code of the decorated object if there is one; otherwise this method delegates to the parent class.
	@return The hash code of this object.
	*/
	public int hashCode()
	{
		final T decoratedObject=getObject();	//get the decorated object
		return decoratedObject!=null ? getObject().hashCode() : super.hashCode();	//return the hash code of the decorated object if possible
	}
	
	/**Determines if this object equals another object.
	This version considers the given object equal to this object if it is of the same type
		as this object, and this object's decorated object's {@link Object#equals(Object)} method also returns <code>true</code>
		for the objects's decorated object or both decorated objects are <code>null</code>.
	@param object The object to compare with this object.
	@return <code>true</code> if the given object is considered equal to this object.
	*/
	public boolean equals(final Object object)
	{
		return getClass().isInstance(object) && Objects.equals(getObject(), ((ObjectDecorator<?>)object).getObject());	//see if the object is of this class and our decorated object is equal to its decorated object
	}
	
	/**Returns a string representation of the object.
	This version returns a string version of the decorated object if there is one; otherwise this method delegates to the parent class.
	*/
	public String toString()
	{
		final T decoratedObject=getObject();	//get the decorated object
		return decoratedObject!=null ? getObject().toString() : super.toString();	//delegate to the decorated object, if possible
	}

}
