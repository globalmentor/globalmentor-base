package com.garretwilson.util;

import java.util.*;
import java.util.concurrent.locks.*;

import static com.globalmentor.java.Objects.*;

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

	/**Retrieves the collection of values associated with the given key.
	If no collection of values is associated with the key, one will be created and added to the map.
	@param key The key in the map.
	@return The collection associated with the given key
	@see #createCollection()
	*/
	public C getCollection(final K key)
	{
		C collection=get(key);	//get the collection of objects for the key, which will be performed with a read lock
		if(collection==null)	//if there is yet no collection for this key
		{
			writeLock().lock();	//get a write lock
			try
			{
				collection=collectionMap.getCollection(key);	//ask the collection map for the collection, which may create and add a collection
			}
			finally
			{
				writeLock().unlock();	//always release the write lock
			}
		}
		return collection;	//return the collection
	}

	/**Creates a collection in which to store values.*/
	public C createCollection() {return collectionMap.createCollection();}

	/**Retrieves whether there are items in a collection associated with the key.
	@param key The key in the map.
	@return <code>true</code> if there is at least one item associated with the key.
	*/
	public boolean hasItems(final K key) {readLock().lock(); try{return collectionMap.hasItems(key);} finally{readLock().unlock();}}

	/**Retrieves the number of values in the collection, if any, associated with the key.
	@param key The key in the map.
	@return The number of items associated with the key.
	*/
	public int getItemCount(final K key) {readLock().lock(); try{return collectionMap.getItemCount(key);} finally{readLock().unlock();}}

	/**Adds a value to the collection of values associated with the key.
	If no collection of values is associated with the key, one will be created and added to the map.
	@param key The key in the map.
	@param value The value to store in the collection.
	@see #getCollection(Object)
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

	/**Removes the first occurence of the given value from the collection of values, if any, associated with the key.
	If all items from the collection are removed, the collection itself is removed from the map.
	@param key The key in the map.
	@param value The item to be removed from the collection, if present.
	@return <code>true</code> if an item was removed as a result of this call.
	*/
	public boolean removeItem(final K key, final V value) {writeLock().lock(); try{return collectionMap.removeItem(key, value);} finally{writeLock().unlock();}}

}
