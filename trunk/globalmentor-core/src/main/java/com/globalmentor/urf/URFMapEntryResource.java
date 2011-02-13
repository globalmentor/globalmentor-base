/*
 * Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.urf;

import java.util.*;
import java.net.URI;

import com.globalmentor.java.Objects;
import static com.globalmentor.java.Objects.*;
import static com.globalmentor.urf.URF.*;

/**An URF map entry resource that allows convenient access to its key and value.
@param <K> The type of key contained in the entry.
@param <V> The type of mapped value.
@author Garret Wilson
*/
public class URFMapEntryResource<K extends URFResource, V extends URFResource> extends DefaultURFResource implements Map.Entry<K, V>
{

	/**Default constructor with no URI.*/
	public URFMapEntryResource()
	{
		this((URI)null);	//create a resource without a URI
	}

	/**URI and type URIs constructor.
	If no types are specified, the type {@value URF#MAP_ENTRY_CLASS_URI} will be added.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
	@param typeURIs The URIs of the types, if any, to add to the resource.
	*/
	public URFMapEntryResource(final URI uri, final URI... typeURIs)
	{
		super(uri, typeURIs.length>0 ? typeURIs : new URI[]{MAP_ENTRY_CLASS_URI});	//construct the parent class, specifying the map entry class if there are no types given
	}

	
	/**Returns the key corresponding to this entry.
	@return The key corresponding to this entry, or <code>null</code> if this entry has no key.
	*/
	@SuppressWarnings("unchecked")
	public K getKey()
	{
		return (K)getPropertyValue(KEY_PROPERTY_URI);	//return the key, assuming that it is the correct generic type
	}

	/**Returns the value corresponding to this entry.
	@return The value corresponding to this entry, or <code>null</code> if this entry has no value.
	*/
	@SuppressWarnings("unchecked")
	public V getValue()
	{
		return (V)getPropertyValue(VALUE_PROPERTY_URI);	//return the value, assuming that it is the correct generic type
	}
		
	/**Replaces the value corresponding to this entry with the specified value.
	@param value The new value to be stored in this entry.
	@return The old value corresponding to the entry.
	@throws ClassCastException if the class of the specified value prevents it from being stored in the backing map.
	@throws NullPointerException if the specified value is <code>null</code>.
	*/
	@SuppressWarnings("unchecked")
	public V setValue(final V value)
	{
		return (V)setPropertyValue(VALUE_PROPERTY_URI, checkInstance(value, "Map entry value cannot be null."));	//set the value, assuming that the old value is of the correct generic type
	}

	/**Compares the specified object with this entry for equality.
	Returns <code>true</code> if the given object is also a map entry and the two entries represent the same mapping.
	@param object The object to be compared for equality with this map entry.
	@return <code>true</code> if the specified object is equal to this map entry.
	*/
	public boolean equals(final Object object)
	{
		if(object instanceof Map.Entry)	//if the other object is also a map entry
		{
			final Map.Entry<?, ?> entry=(Map.Entry<?, ?>)object;	//get the object as a map entry
			return Objects.equals(getKey(), entry.getKey()) && Objects.equals(getValue(), entry.getValue());	//compare keys and values
		}
		else	//if the other object isn't a map entry
		{
			return false;	//the object's aren't equal
		}
	}

	/**Returns the hash code value for this map entry.
	The hash code of a map entry <tt>e</tt> is defined to be the bitwise OR of the key and value, using zero if either is <code>null</code>. 
	@return The hash code value for this map entry.
	@see Object#hashCode()
	@see Object#equals(Object)
	@see #equals(Object)
	*/
	public int hashCode()
	{
		final K key=getKey();	//get the key, if any
		final V value=getValue();	//get the value, if any
		return (key!=null ? key.hashCode() : 0) ^ (value!=null ? value.hashCode() : 0);	//do a bitwise OR of the key and value
	}

}
