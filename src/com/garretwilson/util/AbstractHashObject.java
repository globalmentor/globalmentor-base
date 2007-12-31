package com.garretwilson.util;

import java.util.Arrays;

import static com.garretwilson.lang.Objects.*;
import com.garretwilson.lang.Objects;

/**An object that produces a hash for {@link Object#hashCode()} and implements equality checking for {@link Object#equals(Object)} based upon given objects.
Equality is only supported for exact top-level types.
@author Garret Wilson
*/
public abstract class AbstractHashObject
{

	/**The objects for hashing and equality, any or all of which can be <code>null</code>.*/
	private final Object[] objects;
	
	/**Objects constructor.
	@param objects The objects for hashing and equality, any or all of which can be <code>null</code>.
	@exception NullPointerException if the given objects is <code>null</code>
	*/
	public AbstractHashObject(final Object... objects)
	{
		this.objects=checkInstance(objects, "Objects cannot be null.");	//save the objects
	}
	
	/**Returns the hash code of this object.
	This version returns the hash code of the underlying objects.
	@return The hash code of this object.
	*/
	public int hashCode()
	{
		return Objects.hashCode(objects);	//return the hash code of the objects
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
		return getClass().isInstance(object) && Arrays.equals(objects, ((AbstractHashObject)object).objects);	//see if the object is of this class and our objects are equal to its objects
	}

	/**@return A string representation of this hash object.*/
	public String toString()
	{
		return ArrayUtilities.toString(objects);	//convert the objects to strings
	}
}
