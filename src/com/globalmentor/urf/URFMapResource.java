package com.globalmentor.urf;

import java.util.*;
import java.net.URI;
import static java.util.Collections.*;

import com.garretwilson.lang.Objects;
import static com.garretwilson.util.CollectionUtilities.*;

import static com.globalmentor.urf.URF.*;

/**An URF map resource that allows convenient access to its elements.
The keys to the map are the URIs of the resource properties; the associated values are the resource property values of this resource.
If there are multiple property values exist for a property, it is undefined which property value will be considered to be the mapped value.
The property {@value URF#TYPE_PROPERTY_URI} is not considered to be part of the mappings.
This implementation does not follow the {@link Map#keySet()}, {@link Map#values()}, and {@link Map#entrySet()} contracts in that the values
returned by this implementation are not backed by the map.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@param <K> The type of keys maintained by the map.
@param <V> The type of mapped values.
@author Garret Wilson
*/
public class URFMapResource<K extends URI, V extends URFResource> extends DefaultURFResource implements Map<K, V>
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
	This implementation returns the number of property values, ignoring the values of the {@value URF#TYPE_PROPERTY_URI} property.
	@return The number of key-value mappings in this map.
	*/
	public int size()
	{
		long count;
		readLock().lock();	//get a read lock
		try
		{
			count=getPropertyCount();	//get the number of distinct properties
			if(hasProperty(TYPE_PROPERTY_URI))	//if there is a type property
			{
				--count;	//ignore the type property in the total
			}
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
		return count<Integer.MAX_VALUE ? (int)count : Integer.MAX_VALUE;	//return the value, with a ceiling of Integer.MAX_VALUE
	}

	/**Returns <code>true</code> if this map contains no key-value mappings./
	This implementation determines whether there is at least one property that is not the {@value URF#TYPE_PROPERTY_URI} property.
	@return <code>true</code> if this map contains no key-value mappings.
	*/
	public boolean isEmpty()
	{
		return size()>0;	//determine if there is at least one mapping that isn't the type property
	}

	/**Returns <code>true</code> if this map contains a mapping for the specified key.
	This implementation returns <code>false</code> if the given key is the URI {@value URF#TYPE_PROPERTY_URI}.
	@param key The key whose presence in this map is to be tested.
	@return <code>true</code> if this map contains a mapping for the specified key.
	@throws ClassCastException if the key is of an inappropriate type for this map.
	@throws NullPointerException if the specified key is <code>null</code>.
	*/
	public boolean containsKey(final Object key)
	{
		return !key.equals(TYPE_PROPERTY_URI) && hasProperty((URI)key); //see if this property URI isn't the type property URI and has a value
	}

	/**Returns <code>true</code> if this map maps one or more keys to the specified value.
	This implementation ignores the {@value URF#TYPE_PROPERTY_URI} property.
	@param value The value whose presence in this map is to be tested.
	@return <code>true</code> if this map maps one or more keys to the specified value.
	@throws ClassCastException if the value is of an inappropriate type for this map.
	@throws NullPointerException if the specified value is <code>null</code>.
	*/
	public boolean containsValue(final Object value)
	{
		if(!value.equals(TYPE_PROPERTY_URI))	//if this is not the type property URI
		{
			final URI propertyURI=(URI)value;	//get the value as a URI
			readLock().lock();	//get a read lock
			try
			{
				for(final URFProperty property:getProperties())	//for each property
				{
					if(propertyURI.equals(property.getPropertyURI()))	//if this is the requested property
					{
						return true;	//indicate that the property has a value
					}
				}
			}
			finally
			{
				readLock().unlock();	//always release the read lock
			}
		}
		return false;	//indicate that we couldn't find a value associated with the given URI
	}

	/**Returns the value to which the specified key is mapped.
	@param key The key whose associated value is to be returned
	@return The value to which the specified key is mapped, or <code>null</code> if this map contains no mapping for the key.
	@throws ClassCastException if the key is of an inappropriate type for this map.
	@throws NullPointerException if the specified key is <code>null</code>.
	*/
	@SuppressWarnings("unchecked")
	public V get(final Object key)
	{
		return (V)getPropertyValue((URI)key);	//return the property value, assuming that it's the requested generic type
	}

  // Modification Operations

	/**Associates the specified value with the specified key in this map.
	If the map previously contained a mapping for the key, the old value is replaced by the specified value.
	The URI value {@value URF#TYPE_PROPERTY_URI} is not allowed as a key and will cause an {@link IllegalArgumentException}.
	@param key The key with which the specified value is to be associated.
	@param value The value to be associated with the specified key.
	@return The previous value associated with the key, or <code>null</code> if there was no mapping for the key.
	@throws ClassCastException if the class of the specified key or value prevents it from being stored in this map.
	@throws NullPointerException if the specified key or value is <code>null</code>.
	@throws IllegalArgumentException if some property of the specified key or value prevents it from being stored in this map.
	*/
	@SuppressWarnings("unchecked")
	public V put(final K key, final V value)
	{
		if(TYPE_PROPERTY_URI.equals(key))	//if this is the type property URI
		{
			throw new IllegalArgumentException("The key "+TYPE_PROPERTY_URI+" is not allowed as an URF map resource key.");
		}
		return (V)setPropertyValue(key, value);	//set the property value, assuming that the old value, if any, is of the correct value type
	}

  /**Removes the mapping for a key from this map if it is present.
	@param key The key whose mapping is to be removed from the map.
	@return The previous value associated with the key, or <code>null</code> if there was no mapping for the key.
	@throws ClassCastException if the key is of an inappropriate type for this map.
	@throws NullPointerException if the specified key is <code>null</code>.
	*/
	@SuppressWarnings("unchecked")
	public V remove(final Object key)
	{
		writeLock().lock();	//get a write lock
		try
		{
			final URI propertyURI=(URI)key;	//get the property URI indicated by the key
			final V oldValue=(V)getPropertyValue(propertyURI);	//get the current value, if any
			removePropertyValues(propertyURI);	//remove all the value for this property
			return oldValue;	//return the old value, if any
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

  // Bulk Operations

	/**Copies all of the mappings from the specified map to this map.
	The URI value {@value URF#TYPE_PROPERTY_URI} is not allowed as a key and will cause an {@link IllegalArgumentException}.
	@param map The mappings to be stored in this map.
	@throws ClassCastException if the class of a key or value in the specified map prevents it from being stored in this map.
	@throws NullPointerException if the specified map is <code>null</code>, or if specified map contains <code>null</code> keys or values.
	@throws IllegalArgumentException if some property of a key or value in the specified map prevents it from being stored in this map.
	*/
	public void putAll(final Map<? extends K, ? extends V> map)
	{
		for(final Map.Entry<? extends K, ? extends V> entry:map.entrySet())	//for each entry
		{
			put(entry.getKey(), entry.getValue());	//put this value in the map
		}
	}

	/**Removes all of the mappings from this map.*/
	public void clear()
	{
		removeProperties();	//remove all properties
	}


  // Views

	/**Returns a {@link Set} view of the keys contained in this map.
	In this implementation, the set is read-only and is not backed by the map
	@return A set view of the keys contained in this map.
	*/
	@SuppressWarnings("unchecked")
	public Set<K> keySet()
	{
		readLock().lock();	//get a read lock
		try
		{
			final Set<K> keySet=new HashSet<K>(size());	//create a new hash set TODO add a live property URI set accessor to the URF scope
			addAll(keySet, (Iterable<K>)getPropertyURIs());	//add all the property URIs to the set
			return unmodifiableSet(keySet);	//return a read-only set of all the property URIs
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
	@SuppressWarnings("unchecked")
	public Collection<V> values()
	{
		readLock().lock();	//get a read lock
		try
		{
			final Collection<V> values=new ArrayList<V>((int)getPropertyCount());	//create a new list TODO add a live value set accessor to the URF scope
			for(final URI propertyURI:getPropertyURIs())	//look at each property URI; don't iterate the properties, which could return multiple values for each property
			{
				if(!TYPE_PROPERTY_URI.equals(propertyURI))	//if this is not the type property
				{
					values.add((V)getPropertyValue(propertyURI));	//add the property's value to our value collection
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
			final Set<Map.Entry<K, V>> entries=new HashSet<Map.Entry<K, V>>((int)getPropertyCount());	//create a new set
			for(final URI propertyURI:getPropertyURIs())	//look at each property URI; don't iterate the properties, which could return multiple values for each property
			{
				if(!TYPE_PROPERTY_URI.equals(propertyURI))	//if this is not the type property
				{
					entries.add(new AbstractMap.SimpleEntry<K, V>((K)propertyURI, (V)getPropertyValue(propertyURI)));	//add an entry of the property URI and the first value associated with it, assuming they are of the correct types
				}
			}
			return unmodifiableSet(entries);	//return a read-only set of the simple entries
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
				if(!(key instanceof URI) || !getPropertyValue((URI)key).equals(value))	//this entry is equal if its key is a URI and its property value equals our property value (our property value will never be null)
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
			for(final URI propertyURI:getPropertyURIs())	//look at each property URI; don't iterate the properties, which could return multiple values for each property
			{
				if(!TYPE_PROPERTY_URI.equals(propertyURI))	//if this is not the type property
				{
					hashCode+=Objects.hashCode(propertyURI, getPropertyValue(propertyURI));	//add the property-value hash code to our total
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
	public static <K extends URI, V extends URFResource> URFMapResource<K, V> toMapResource(final Map<K, V> map)
	{
		return map instanceof URFMapResource ? (URFMapResource<K, V>)map : new URFMapResource<K, V>(map);	//if the map is already a map resource, return the map as a map resource; otherwise create a new map resource with the mappings of the map
	}

}
