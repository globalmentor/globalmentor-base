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

/**A default implementation of a named object.
@author Garret Wilson
*/
public class DefaultNamed<N> implements Named<N>
{

	/**The name of the object.*/
	private final N name;

		/**@return The name of the object.*/
		public N getName() {return name;}
		
	/**Constructor specifying the name.
	@param newName The object's new name.
	@throws NullPointerException if the given name is <code>null</code>;
	*/
	public DefaultNamed(final N newName)
	{
		this.name=checkInstance(newName, "Name cannot be null."); //set the name
	}

	/**Compares the names of two objects if the other object is a {@link Named}.
	@param object The object with which to compare this named object; should be another {@link Named}.
	@return <code>true<code> if the other object is a named object with the same name.
	@see #getName()
	*/
	public boolean equals(final Object object)
	{
		return object instanceof Named && getName().equals(((Named<?>)object).getName());	//see if the other object is a named object with the same named
	}

	/**@return A hashcode value composed from the name.*/
	public int hashCode()
	{
		return getName().hashCode();  //return the hash code of the name
	}

	/**@return A string representation of this object's name.*/
	public String toString()
	{
		return super.toString()+": "+getName();	//return a string constructed from the name
	}
}