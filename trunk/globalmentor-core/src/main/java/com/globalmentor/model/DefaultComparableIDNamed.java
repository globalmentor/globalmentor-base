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

package com.globalmentor.model;

/**A default implementation of an IDed and named object comparable by name.
This implementation does not allow a <code>null</code> name.
Note: Because this class considers equality by ID and order by name, this class has a natural ordering that is inconsistent with equals.
@author Garret Wilson
*/
public class DefaultComparableIDNamed<I, N extends Comparable<N>> extends DefaultComparableNamed<N> implements IDed<I>, Comparable<Named<N>>
{

	/**@The identifier, preferably unique, of the object; or <code>null</code> if the object has no ID.*/
	private final I id;

		/**@return The identifier, preferably unique, of the object; or <code>null</code> if the object has no ID.*/
		public I getID() {return id;}

	/**Constructor specifying the ID and name.
	@param id The ID of the object, or <code>null</code> if the object should have no ID.
	@param name The name of the object.
	@throws NullPointerException if the given name is <code>null</code>.
	*/
	public DefaultComparableIDNamed(final I id, final N name)
	{
		super(name);  //construct the base class
		this.id=id;
	}

	/**Considers another object equal if the other object is an {@link IDed} with the same ID.
	@param object The object with which to compare this object; should be an {@link IDed}.
	@return <code>true<code> if the given object is an {@link IDed} with the same ID.
	@see #getID()
	*/
	public boolean equals(final Object object)
	{
		return this==object || object instanceof IDed && getID().equals(((IDed<?>)object).getID());	//see if the other object is an IDable with the same ID
	}

	/**@return A hashcode value from the ID.*/
	public int hashCode()
	{
		return getID().hashCode();	//return the ID's hashcode
	}

	/**@return A string representation of this object in the format "objectinfo: [ID]".*/
	public String toString()
	{
		return super.toString()+": ["+getID()+"]";  //return a string constructed from the default string and the ID
	}

}
