package com.garretwilson.util;

import java.util.*;
import java.util.concurrent.locks.*;

import static com.garretwilson.lang.ObjectUtilities.*;

/**A thread-safe collection map decorator that allows many readers but only one writer to access a map at a time.
For operations that iterate over live map data, a read or write lock should be acquired before the call to acquire the data and held until the data is consumed.
@param <K> The type of key used in the map.
@param <V> The type of value stored in the map.
@param <C> The type of collection in which to store values in the map.
@author Garret Wilson
*/
public class DecoratorReadWriteLockCollectionMap<K, V, C extends Collection<V>> extends DecoratorReadWriteLockMap<K, C> implements ReadWriteLockCollectionMap<K, V, C>
{

	/**The collection map this class decorates.*/
	protected final CollectionMap<K, V, C> collectionMap;

	/**Collection map constructor with a default reentrant read/write lock.
	@param collectionMap The collection map this map should decorate.
	@exception NullPointerException if the provided collection map is <code>null</code>.
	*/
	public DecoratorReadWriteLockCollectionMap(final CollectionMap<K, V, C> collectionMap)
	{
		this(collectionMap, new ReentrantReadWriteLock());	//create the map with a default lock
	}

	/**Collection map and read/write lock constructor.
	@param collectionMap The collection map this map should decorate.
	@param lock The lock for controlling access to the map.
	@exception NullPointerException if the provided map and/or lock is <code>null</code>.
	*/
	public DecoratorReadWriteLockCollectionMap(final CollectionMap<K, V, C> collectionMap, final ReadWriteLock lock)
	{
		super(collectionMap, lock);	//construct the parent class
		this.collectionMap=checkInstance(collectionMap, "Collection map cannot be null");	//save the collection map
	}

	/**Adds a value to the collection of values associated with the key.
	If no collection of values is associated with the key, one will be created and added to the map.
	@param key The key in the map.
	@param value The value to store in the collection.
	*/
	public void addItem(final K key, final V value) {writeLock().lock(); try{collectionMap.addItem(key, value);} finally{writeLock().unlock();}} 

	/**Retrieves the first value from the collection of values, if any, associated with the key.
	@param key The key in the map.
	@return The first value in the collection, or <code>null</code> if there is no collection associated with the key or no values in the collection.
	*/
	public V getItem(final K key) {readLock().lock(); try{return collectionMap.getItem(key);} finally{readLock().unlock();}}

	/**Retrieves iterable access to all items, if any, associated with the given key
	@param key The key in the map.
	@return An object that will iterate all items, if any, associated with the given key.
	*/
	public Iterable<V> getItems(final K key) {readLock().lock(); try{return collectionMap.getItems(key);} finally{readLock().unlock();}}
	
}