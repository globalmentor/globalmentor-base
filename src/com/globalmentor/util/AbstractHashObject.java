/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.util;

import java.util.Arrays;

import static com.globalmentor.java.Objects.*;

import com.globalmentor.java.Objects;

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
		return getClass().isInstance(object) && java.util.Arrays.equals(objects, ((AbstractHashObject)object).objects);	//see if the object is of this class and our objects are equal to its objects
	}

	/**@return A string representation of this hash object.*/
	public String toString()
	{
		return Arrays.toString(objects);	//convert the objects to strings
	}
}
