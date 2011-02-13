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
import static java.util.Collections.*;

import com.globalmentor.java.Objects;
import static com.globalmentor.urf.URF.*;

/**An URF map resource that allows convenient access to its entries.
If there are multiple entries with the same key, it is undefined which property value will be considered to be the mapped value.
This implementation does not support <code>null</code> keys or values; if there is an entry that contains no <code>null</code> value, the entry is ignored.
All entry objects that are not instances of {@link URFMapEntryResource} are ignored.
This implementation does not follow the {@link Map#keySet()}, {@link Map#values()}, and {@link Map#entrySet()} contracts in that the values
returned by this implementation are not backed by the map.
This implementation currently performs an inefficient traversal of all entries for each lookup.
@param <K> The type of keys maintained by the map.
@param <V> The type of mapped values.
@author Garret Wilson
*/
public class URFMapResource<K extends URFResource, V extends URFResource> extends DefaultURFResource implements Map<K, V>
{

	/**Default constructor with no URI.*/
	public URFMapResource()
	{
		this((URI)null);	//create a resource without a URI
	}

	/**URI and type URIs constructor.
	If no types are specified, the type {@value URF#MAP_CLASS_URI} will be added.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
	@param typeURIs The URIs of the types, if any, to add to the resource.
	*/
	public URFMapResource(final URI uri, final URI... typeURIs)
	{
		super(uri, typeURIs.length>0 ? typeURIs : new URI[]{MAP_CLASS_URI});	//construct the parent class, specifying the map class if there are no types given
	}

	/**Collection constructor with no URI.
	@param map The map whose mappings are to be placed in this map.
	@exception NullPointerException if the specified map is <code>null</code>.
	*/
	public URFMapResource(final Map<? extends K, ? extends V> map)
	{
		this(null, map);	//construct the class with no URI
	}

	/**URI and collection constructor.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
	@param map The map whose mappings are to be placed in this map.
	@exception NullPointerException if the specified map is <code>null</code>.
	*/
	public URFMapResource(final URI uri, final Map<? extends K, ? extends V> map)
	{
		this(uri);	//construct the class with the URI
		putAll(map);	//add all the map mappings to the map
	}

	/**Returns the number of key-value mappings in this map.
	If this map contains more than {@link Integer#MAX_VALUE} elements, returns {@link Integer#MAX_VALUE}.
	This implementation returns the number of {@value URF#ENTRY_PROPERTY_URI} property values.
	@return The number of key-value mappings in this map.
	*/
	public int size()
	{
		final long count=getPropertyValueCount(ENTRY_PROPERTY_URI);	//see how many entries there are
		return count<Integer.MAX_VALUE ? (int)count : Integer.MAX_VALUE;	//return the value, with a ceiling of Integer.MAX_VALUE
	}

	/**Returns <code>true</code> if this map contains no key-value mappings./
	This implementation determines whether there are no {@value URF#ENTRY_PROPERTY_URI} property values.
	@return <code>true</code> if this map contains no key-value mappings.
	*/
	public boolean isEmpty()
	{
		return getPropertyValueCount(ENTRY_PROPERTY_URI)==0;	//determine if there are no entries
	}

	/**Returns <code>true</code> if this map contains a mapping for the specified key.
	@param key The key whose presence in this map is to be tested.
	@return <code>true</code> if this map contains a mapping for the specified key.
	@throws ClassCastException if the key is of an inappropriate type for this map.
	@throws NullPointerException if the specified key is <code>null</code>.
	*/
	public boolean containsKey(final Object key)
	{
		return get(key)!=null;	//see if there is a value mapped to this key
	}

	/**Returns <code>true</code> if this map maps one or more keys to the specified value.
	@param value The value whose presence in this map is to be tested.
	@return <code>true</code> if this map maps one or more keys to the specified value.
	@throws ClassCastException if the value is of an inappropriate type for this map.
	@throws NullPointerException if the specified value is <code>null</code>.
	*/
	public boolean containsValue(final Object value)
	{
		readLock().lock();	//get a read lock
		try
		{
			for(final URFProperty entryProperty:getProperties(ENTRY_PROPERTY_URI))	//for each entry property
			{
				final URFMapEntryResource<K, V> entry=asMapEntryInstance(entryProperty.getValue());	//get the entry
				if(entry!=null)	//if this is a map entry
				{
					if(value.equals(entry.getValue()))	//if this entry has the correct value
					{
						if(entry.getKey()!=null)	//if this entry has a key
						{
							return true;	//indicate that we found the value mapped to a key
						}
					}
				}
			}
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
		return false;	//indicate that we couldn't find any key mapped to the given value
	}

	/**Returns the value to which the specified key is mapped.
	@param key The key whose associated value is to be returned
	@return The value to which the specified key is mapped, or <code>null</code> if this map contains no mapping for the key.
	@throws ClassCastException if the key is of an inappropriate type for this map.
	@throws NullPointerException if the specified key is <code>null</code>.
	*/
	public V get(final Object key)
	{
		readLock().lock();	//get a read lock
		try
		{
			for(final URFProperty entryProperty:getProperties(ENTRY_PROPERTY_URI))	//for each entry property
			{
				final URFMapEntryResource<K, V> entry=asMapEntryInstance(entryProperty.getValue());	//get the entry
				if(entry!=null)	//if this is a map entry
				{
					if(key.equals(entry.getKey()))	//if this entry has the correct key
					{
						final V value=entry.getValue();	//get the entry value
						if(value!=null)	//if there is a value
						{
							return value;	//return the entry value
						}
					}
				}
			}
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
		return null;	//indicate that we couldn't find a value associated with the given key
	}

  // Modification Operations

	/**Associates the specified value with the specified key in this map.
	If the map previously contained a mapping for the key, the old value is replaced by the specified value.
	@param key The key with which the specified value is to be associated.
	@param value The value to be associated with the specified key.
	@return The previous value associated with the key, or <code>null</code> if there was no mapping for the key.
	@throws ClassCastException if the class of the specified key or value prevents it from being stored in this map.
	@throws NullPointerException if the specified key or value is <code>null</code>.
	@throws IllegalArgumentException if some property of the specified key or value prevents it from being stored in this map.
	*/
	public V put(final K key, final V value)
	{
		writeLock().lock();	//get a write lock
		try
		{
			final V oldValue=remove(key);	//remove the value, if any, associated with the key
			final URFMapEntryResource<K, V> entry=new URFMapEntryResource<K, V>();	//create a new map entry resource
			entry.setPropertyValue(KEY_PROPERTY_URI, key);	//set the key
			entry.setPropertyValue(VALUE_PROPERTY_URI, key);	//set the value
			addPropertyValue(ENTRY_PROPERTY_URI, entry);	//add this entry to the map
			return oldValue;	//return the old value, if any
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

  /**Removes the mapping for a key from this map if it is present.
	@param key The key whose mapping is to be removed from the map.
	@return The previous value associated with the key, or <code>null</code> if there was no mapping for the key.
	@throws ClassCastException if the key is of an inappropriate type for this map.
	@throws NullPointerException if the specified key is <code>null</code>.
	*/
	public V remove(final Object key)
	{
		writeLock().lock();	//get a write lock
		try
		{
			for(final URFProperty entryProperty:getProperties(ENTRY_PROPERTY_URI))	//for each entry property
			{
				final URFMapEntryResource<K, V> entry=asMapEntryInstance(entryProperty.getValue());	//get the entry
				if(entry!=null)	//if this is a map entry
				{
					if(key.equals(entry.getKey()))	//if this entry has the correct key
					{
						final V value=entry.getValue();	//get the entry value
						if(value!=null)	//if there is a value
						{
							removePropertyValue(ENTRY_PROPERTY_URI, entry);	//remove this entry TODO just remove the entry property when the property iterator is live
							return value;	//return the entry value
						}
					}
				}
			}
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
		return null;	//indicate that we couldn't find a value associated with the given key
	}

  // Bulk Operations

	/**Copies all of the mappings from the specified map to this map.
	@param map The mappings to be stored in this map.
	@throws ClassCastException if the class of a key or value in the specified map prevents it from being stored in this map.
	@throws NullPointerException if the specified map is <code>null</code>, or if specified map contains <code>null</code> keys or values.
	@throws IllegalArgumentException if some property of a key or value in the specified map prevents it from being stored in this map.
	*/
	public void putAll(final Map<? extends K, ? extends V> map)
	{
		writeLock().lock();	//get a write lock
		try
		{
			clear();	//clear all entries from the map
			for(final Map.Entry<? extends K, ? extends V> entry:map.entrySet())	//for each entry
			{
				final URFMapEntryResource<K, V> newEntry=new URFMapEntryResource<K, V>();	//create a new map entry resource
				newEntry.setPropertyValue(KEY_PROPERTY_URI, entry.getKey());	//set the key
				newEntry.setPropertyValue(VALUE_PROPERTY_URI, entry.getValue());	//set the value
				addPropertyValue(ENTRY_PROPERTY_URI, newEntry);	//add the new entry to the map
			}
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

	/**Removes all of the mappings from this map.*/
	public void clear()
	{
		removePropertyValues(ENTRY_PROPERTY_URI);	//remove all the entries, if any
	}

  // Views

	/**Returns a {@link Set} view of the keys contained in this map.
	In this implementation, the set is read-only and is not backed by the map
	@return A set view of the keys contained in this map.
	*/
	public Set<K> keySet()
	{
		readLock().lock();	//get a read lock
		try
		{
			final Set<K> keySet=new HashSet<K>(size());	//create a new hash set TODO add a live property URI set accessor to the URF scope
			for(final URFProperty entryProperty:getProperties(ENTRY_PROPERTY_URI))	//for each entry property
			{
				final URFMapEntryResource<K, V> entry=asMapEntryInstance(entryProperty.getValue());	//get the entry
				if(entry!=null)	//if this is a map entry
				{
					final K key=entry.getKey();	//get the key
					if(key!=null && entry.getValue()!=null)	//if there is a key and a value
					{
						keySet.add(key);	//add the key to the set
					}
				}
			}
			return unmodifiableSet(keySet);	//return a read-only set of all the keys
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Returns a {@link Collection} view of the values contained in this map.
	In this implementation, the collection is read-only and is not backed by the map.
	@return A collection view of the values contained in this map
	*/
	public Collection<V> values()
	{
		readLock().lock();	//get a read lock
		try
		{
			final Collection<V> values=new ArrayList<V>(size());	//create a new list TODO add a live value set accessor to the URF scope
			for(final URFProperty entryProperty:getProperties(ENTRY_PROPERTY_URI))	//for each entry property
			{
				final URFMapEntryResource<K, V> entry=asMapEntryInstance(entryProperty.getValue());	//get the entry
				if(entry!=null)	//if this is a map entry
				{
					final V value=entry.getValue();	//get the value
					if(value!=null && entry.getKey()!=null)	//if there is a key and a value
					{
						values.add(value);	//add the value to the collection
					}
				}
			}
			return unmodifiableCollection(values);	//return a read-only collection of the value for each property
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Returns a {@link Set} view of the mappings contained in this map.
	In this implementation, the set is read-only and is not backed by the map.
	@return A set view of the mappings contained in this map.
	*/
	public Set<Map.Entry<K, V>> entrySet()
	{
		readLock().lock();	//get a read lock
		try
		{
			final Set<Map.Entry<K, V>> entries=new HashSet<Map.Entry<K, V>>(size());	//create a new set
			for(final URFProperty entryProperty:getProperties(ENTRY_PROPERTY_URI))	//for each entry property
			{
				final URFMapEntryResource<K, V> entry=asMapEntryInstance(entryProperty.getValue());	//get the entry
				if(entry!=null)	//if this is a map entry
				{
					entries.add(entry);	//add this entry to the set
				}
			}
			return unmodifiableSet(entries);	//return a read-only set of the entries
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	// Comparison and hashing

  /**Compares the specified object with this map for equality.
	Returns <code>true</code> if the given object is also a map and the two maps represent the same mappings.
	@param object The object to be compared for equality with this map.
	@return <code>true</code> if the specified object is equal to this map.
	*/
  @SuppressWarnings("unchecked")
	public boolean equals(final Object object)
  {
  	if(object==this)	//if we're being compared to ourselves
  	{
  		return true;	//this object always equals itself
  	}
  	if(!(object instanceof Map))	//if the given object is not a map
  	{
  		return false;	//the objects aren't equal
  	}
  	final Map<?, ?> map=(Map<?, ?>)object;	//get the other object as a map
		readLock().lock();	//get a read lock
		try
		{
			if(size()!=map.size())	//if the maps don't have the same size
			{
				return false;	//the maps aren't equal
			}
			for(final Map.Entry<?, ?> entry:map.entrySet())	//look at all the entries in the other map
			{
				final Object key=entry.getKey();	//get the other key
				final Object value=entry.getValue();	//get the other value
				if(key==null || !value.equals(get((K)key)))	//this entry is equal if its property value equals our property value (our property value will never be null)
				{
					return false;	//this entry didn't match
				}
			}
			return true;	//all entries were non-null and matched ours, so the maps have the same mappings
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
  }

	/**Returns the hash code value for this map.
	The hash code of a map is defined to be the sum of the hash codes of each entry in the map's #entrySet() view.
	@return The hash code value for this map
	@see Map.Entry#hashCode()
	@see Object#equals(Object)
	@see Map#equals(Object)
	*/
	public int hashCode()
	{
		int hashCode=0;	//start with a hash code of 0, according to the map contract
		readLock().lock();	//get a read lock
		try
		{
			for(final URFProperty entryProperty:getProperties(ENTRY_PROPERTY_URI))	//for each entry property
			{
				final URFMapEntryResource<K, V> entry=asMapEntryInstance(entryProperty.getValue());	//get the entry
				if(entry!=null)	//if this is a map entry
				{
					hashCode+=Objects.hashCode(entry.getKey(), entry.getValue());	//add the property-value hash code to our total
				}
			}
			return hashCode;	//return the calculated hash code
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Converts the given map to an URF map resource.
	If the collection is already a map resource, the map is returned;
	otherwise, a new map resource with the contents of the map is returned.
	@param <K> The type of keys maintained by the map.
	@param <V> The type of mapped values.
	@param map The map to convert to a map resource.
	@return A map resource representing the contents of the given map.
	*/
	public static <K extends URFResource, V extends URFResource> URFMapResource<K, V> toMapResource(final Map<K, V> map)
	{
		return map instanceof URFMapResource ? (URFMapResource<K, V>)map : new URFMapResource<K, V>(map);	//if the map is already a map resource, return the map as a map resource; otherwise create a new map resource with the mappings of the map
	}

}
