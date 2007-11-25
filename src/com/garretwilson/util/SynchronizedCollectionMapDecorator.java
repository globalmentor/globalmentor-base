package com.garretwilson.util;

import java.util.*;

import static com.garretwilson.lang.ObjectUtilities.*;

/**A collection map that wraps an existing collection map, providing access through the {@link CollectionMap} interface.
All map access is synchronized on the provided synchronization object.
@param <K> The type of key used in the map.
@param <V> The type of value stored in the map.
@param <C> The type of collection in which to store values in the map.
@author Garret Wilson
*/
public class SynchronizedCollectionMapDecorator<K, V, C extends Collection<V>> extends SynchronizedMapDecorator<K, C> implements CollectionMap<K, V, C>
{

	/**The collection map this class decorates.*/
	protected final CollectionMap<K, V, C> collectionMap;

	/**Collection map constructor.
	The new instance of this class is used as a mutex.
	@param collectionMap The collection map this collection map should decorate.
	@exception NullPointerException if the provided collection map is <code>null</code>.
	*/
	public SynchronizedCollectionMapDecorator(final CollectionMap<K, V, C> collectionMap)
	{
		super(collectionMap);	//construct the parent class with the collection map
		this.collectionMap=checkInstance(collectionMap, "Collection map cannot be null");	//save the collection map
	}

	/**Collection map constructor.
	@param collectionMap The collection map this collection map should decorate.
	@param mutex The mutual exclusion synchronization object.
	@exception NullPointerException if the provided collection map and/or mutex is <code>null</code>.
	*/
	public SynchronizedCollectionMapDecorator(final CollectionMap<K, V, C> collectionMap, final Object mutex)
	{
		super(collectionMap, mutex);	//construct the parent class with the collection map
		this.collectionMap=checkInstance(collectionMap, "Collection map cannot be null");	//save the collection map
	}

	/**Retrieves the collection of values associated with the given key.
	If no collection of values is associated with the key, one will be created and added to the map.
	@param key The key in the map.
	@return The collection associated with the given key
	@see #createCollection()
	*/
	public C getCollection(final K key) {synchronized(mutex) {return collectionMap.getCollection(key);}}

	/**Creates a collection in which to store values.
	This implementation is not synchronized.
	*/
	public C createCollection() {return collectionMap.createCollection();}

	/**Retrieves whether there are items in a collection associated with the key.
	@param key The key in the map.
	@return <code>true</code> if there is at least one item associated with the key.
	*/
	public boolean hasItems(final K key) {synchronized(mutex) {return collectionMap.hasItems(key);}}

	/**Retrieves the number of values in the collection, if any, associated with the key.
	@param key The key in the map.
	@return The number of items associated with the key.
	*/
	public int getItemCount(final K key) {synchronized(mutex) {return collectionMap.getItemCount(key);}}

	/**Adds a value to the collection of values associated with the key.
	If no collection of values is associated with the key, one will be created and added to the map.
	@param key The key in the map.
	@param value The value to store in the collection.
	*/
	public void addItem(final K key, final V value) {synchronized(mutex) {collectionMap.addItem(key, value);}} 

	/**Retrieves the first value from the collection of values, if any, associated with the key.
	@param key The key in the map.
	@return The first value in the collection, or <code>null</code> if there is no collection associated with the key or no values in the collection.
	*/
	public V getItem(final K key) {synchronized(mutex) {return collectionMap.getItem(key);}}

	/**Retrieves iterable access to all items, if any, associated with the given key
	@param key The key in the map.
	@return An object that will iterate all items, if any, associated with the given key.
	*/
	public Iterable<V> getItems(final K key) {synchronized(mutex) {return collectionMap.getItems(key);}}
	
	/**Removes the first occurence of the given value from the collection of values, if any, associated with the key.
	If all items from the collection are removed, the collection itself is removed from the map.
	@param key The key in the map.
	@param value The item to be removed from the collection, if present.
	@return <code>true</code> if an item was removed as a result of this call.
	*/
	public boolean removeItem(final K key, final V value) {synchronized(mutex) {return collectionMap.removeItem(key, value);}}

}