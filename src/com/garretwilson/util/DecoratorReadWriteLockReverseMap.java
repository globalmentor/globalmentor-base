package com.garretwilson.util;

import java.util.*;
import java.util.concurrent.locks.*;

import static com.garretwilson.lang.ObjectUtilities.*;

/**A thread-safe map decorator that allows many readers but only one writer to access a map at a time, and that also allows lookup of the map keys keyed to the values by decorating two maps.
For operations that iterate over live map data, a read or write lock should be acquired before the call to acquire the data and held until the data is consumed.
@param <K> The type of key used in the map.
@param <V> The type of value stored in the map.
@author Garret Wilson
*/
public class DecoratorReadWriteLockReverseMap<K, V> extends DecoratorReadWriteLockMap<K, V> implements ReadWriteLockReverseMap<K, V>
{
	/**The map containing reverse-lookup values.*/
	private final Map<V, K> reverseMap;

	/**Map constructor with a default reentrant read/write lock.
	@param map The map this map should decorate.
	@param reverseMap The map to contain reverse lookup values.
	@exception NullPointerException if the provided map and/or reverse map is <code>null</code>.
	*/
	public DecoratorReadWriteLockReverseMap(final Map<K, V> map, final Map<V, K> reverseMap)
	{
		this(map, reverseMap, new ReentrantReadWriteLock());	//create the map with a default lock
	}

	/**Map and read/write lock constructor.
	@param map The map this map should decorate.
	@param reverseMap The map to contain reverse lookup values.
	@param lock The lock for controlling access to the map.
	@exception NullPointerException if the provided map, reverse map, and/or lock is <code>null</code>.
	*/
	public DecoratorReadWriteLockReverseMap(final Map<K, V> map, final Map<V, K> reverseMap, final ReadWriteLock lock)
	{
		super(map, lock);	//construct the parent class
		this.reverseMap=checkInstance(reverseMap, "Reverse map cannot be null.");
	}

	/**Returns the key that represents the given value. 
	@param value The value whose associated key is to be returned.
	@return The key to which this map reverse maps the specified value, or
		<code>null</code> if the map contains no reverse mapping for this value.
	@exception ClassCastException Thrown if the key is of an inappropriate type
		for this map (optional).
	@exception NullPointerException Thrown if the value is <code>null</code> and
		this map does not not permit <code>null</code> keys (optional).
	@see #containsValue(Object)
	*/
	public K getKey(final V value) {readLock().lock(); try{return reverseMap.get(value);} finally{readLock().unlock();}}	//return the key keyed to the given value in the key map

	/**Returns <code>true</code> if this map maps a key to the specified value.
	<p>This version uses an internal reverse map to provide faster lookups than
		the default linear-time lookup.</p>
	@param value The value whose presence in this map is to be tested.
	@return <code>true</code> if this map maps a key to the specified value.
	*/
	public boolean containsValue(final Object value) {readLock().lock(); try{return reverseMap.containsKey(value);} finally{readLock().unlock();}}	//see if this value is stored in the key map

	/**Associates the specified value with the specified key in this map,
		and associates the specified key with the specified value in the internal
		reverse map.
	<p>If the map previously contained a mapping for this key, the old
		value is replaced.</p>
	@param key The key with which the specified value is to be associated.
	@param value The value to be associated with the specified key.
	@return The previous value associated with specified key, or <code>null</code>
		if there was no mapping for key. A <code>null</code> return can
		also indicate that the map previously associated
		<code>null</code> with the specified key.
	*/
	public V put(final K key, final V value)
	{
		writeLock().lock();	//get a lock for writing
		try
		{
			final V oldValue=super.put(key, value);	//store the value in the map, keyed to the key
			reverseMap.put(value, key);	//store the key in the key map, keyed to the value
			return oldValue;	//return the old value previously mapped to the key, if any
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

	/**Removes all mappings from this map.*/
	public void clear()
	{
		writeLock().lock();	//get a lock for writing
		try
		{
			super.clear();	//do the default clearing
			reverseMap.clear();	//clear the key map as well
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

}
