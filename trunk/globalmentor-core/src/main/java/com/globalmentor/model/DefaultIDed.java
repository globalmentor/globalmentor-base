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

import static com.globalmentor.java.Objects.*;

/**A default implementation of a uniquely identified object.
 	This class is useful to serve as a base class to other classes that are
 	identified by an ID. It provides equality checking based upon the ID.
<p>This implementation provides the capability to provide automatic conversion
	of the ID when the ID is set, such as converting to lowercase.</p>
@author Garret Wilson
@see Object#equals
*/
public class DefaultIDed<I> implements IDed<I>
{

	/**@The identifier, preferably unique, of the object; or <code>null</code> if the object has no ID.*/
	private I id;

		/**@return The identifier, preferably unique, of the object; or <code>null</code> if the object has no ID.*/
		public I getID() {return id;}

		/**Sets the identifier of the object.
		 * @param id The identifier, preferably unique, of the object; or <code>null</code> if the object has no ID. 
		 */
		protected void setID(final I id)
		{
			this.id=id;
		}
		
	/**Constructor specifying the ID.
	@param id The ID of the object.
	*/
	public DefaultIDed(final I id)
	{
		this.id=checkInstance(id, "ID cannot be null.");  //set the ID
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
