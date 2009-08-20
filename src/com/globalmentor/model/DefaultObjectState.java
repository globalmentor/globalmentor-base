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

import java.util.*;

import static com.globalmentor.java.Objects.*;

/**A default implementation of object state information.
<p>Each property changed using <code>setProperty</code> will fire a
	property change event if its key is a string.</p>
@param <T> The type of object the state of which is being stored.
@author Garret Wilson
@see Modifiable#MODIFIED_PROPERTY
@see java.beans.PropertyChangeEvent
@deprecated
*/
public class DefaultObjectState<T> extends DefaultModifiable implements ObjectState<T>	//TODO maybe just store the modified value in the properties rather than having an explicit modified variable; right now, setting a property of "modified" would result in an identical property change firing
{
	
	/**The non-<code>null</code> object being described.*/
	private T object;

		/**@return The non-<code>null</code> object being described.*/
		public T getObject() {return object;}
		
		/**Sets the object being described.
		@param object The new object to describe.
		@exception NullPointerException Thrown if the object is <code>null</code>.
		*/
		public void setObject(final T object)	//TODO determine if we want to make this public or not
		{
			this.object=checkInstance(object, "Object cannot be null");
		}

	/**The map of properties.*/
	private final Map propertyMap=new HashMap();
	
		/**Gets a property of the object state.
		@param key The key to the property.
		@return The value of the object state's property, or <code>null</code> if
			that property does not exist.
		*/
		public Object getProperty(final Object key)
		{
			return propertyMap.get(key);	//return the property from the property map
		}
		
		/**Sets the value of an object state property, and fires a property changed
			event if the key is a string.
		If the property represented by the key already exists, it will be replaced.
		@param key The non-<code>null</code> property key.
		@param value The property value.
		@return The old property value associated with the key, or <code>null</code>
			if no value was associated with the key previously.
		@see PropertyChangeEvent
		*/
		public Object setProperty(final Object key, final Object value)
		{
			final Object oldValue=propertyMap.put(key, value);	//put the value in the map keyed to the key and save the old value
			if(key instanceof String)	//if they key was a string
			{					
				firePropertyChange((String)key, oldValue, value);	//show that the property value has changed
			}
			return oldValue;	//return the old property value, if there was one
		}
	
		/**Removes a property of the object state.
		If the property represented by the key does not exist, no action is taken.
		@param key The non-<code>null</code> property key.
		@return The removed property value, or <code>null</code> if there was no
			property.
		*/
		public Object removeProperty(final Object key)
		{
			return propertyMap.remove(key);	//remove and return the property value keyed to the key
		}

	/**Constructs an object state with an object.
	@param object The non-<code>null</code> object being described
	@exception NullPointerException Thrown if the object is <code>null</code>.
	*/
	public DefaultObjectState(final T object)
	{
		setObject(object);	//save the object
	}


	/**Compares object states by comparing their respective objects.
	@param object The object with which to compare this RDF resource; should be
		another object state.
	@return <code>true<code> if this object state refers to the same object as
		specified in the object state <code>object</code>.
	@see ObjectState
	@see #getObject
	*/
	public boolean equals(final Object object)
	{
		if(object instanceof ObjectState)	//if the other object implements the object state methods
		{
			return getObject().equals(((ObjectState)object).getObject());	//compare our object with that of the object state
		}
		return super.equals(object);	//try to compare the objects normally if the object isn't an object state
	}

	/**@return The hashcode value of the object represented.*/
	public int hashCode()
	{
		return getObject().hashCode();	//return the represented object's hash code
	}

}