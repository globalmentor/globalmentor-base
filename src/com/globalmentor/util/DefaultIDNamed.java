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

import static com.globalmentor.java.Objects.*;

/**A convenience class for storing an ID and a name. This class is useful to
	serve as a base class to other classes that are identified by an ID and a
	name. It provides compare functionality that sorts according to the name.
@author Garret Wilson
*/
public class DefaultIDNamed<I, N extends Comparable<N>> extends DefaultIDed<I> implements Named<N>, Comparable<Named<N>>
{

	/**The name of the object.*/
	private N name;

		/**@return The name of the object.*/
		public N getName() {return name;}

		/**Sets the name of the object.
		@param newName The new name of the object.
		@throws NullPointerException if the given name is <code>null</code>.
		*/
		protected void setName(final N newName)
		{
			this.name=checkInstance(newName, "Name cannot be null.");
		}

	/**Constructor specifying the ID and name.
	@param id The ID of the object.
	@param name The name of the object.
	@throws NullPointerException if the given ID and/or name is <code>null</code>.
	*/
	public DefaultIDNamed(final I id, final N name)
	{
		super(id);  //construct the base class
		this.name=checkInstance(name, "Name cannot be null.");
	}

	/**Compares this object to another object.
		This method determines order based upon the name of the object.
	@param object The object with which to compare the component.
	@return A negative integer, zero, or a positive integer as this name is
		less than, equal to, or greater than the name of the specified object,
		respectively.
	@exception ClassCastException Thrown if the specified object's type is not
		an <code>IDNameObject</code>.
	@see #getName
	*/
	public int compareTo(final Named<N> object) throws ClassCastException
	{
		return getName().compareTo(object.getName()); //compare names
	}

	/**@return A string representation of this object in the format "objectinfo: [ID] name".*/
	public String toString()
	{
		return super.toString()+' '+getName();  //return a string constructed from the default string and the name
	}

}
