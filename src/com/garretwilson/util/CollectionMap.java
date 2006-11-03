package com.garretwilson.util;

import java.util.Collection;
import java.util.Map;

/**A map that stores a collection of values for each key, with special methods for retrieving single values.
@param <K> The type of key used in the map.
@param <V> The type of value stored in each collection in the map.
@param <C> The type of collection in which to store values in the map.
@author Garret Wilson
*/
public interface CollectionMap<K, V, C extends Collection<V>> extends Map<K, C> 
{
	/**Adds a value to the collection of values associated with the key.
	If no collection of values is associated with the key, one will be created and added to the map.
	@param key The key in the map.
	@param value The value to store in the collection.
	*/
	public void addItem(final K key, final V value);

	/**Retrieves the first value from the collection of values, if any, associated with the key.
	@param key The key in the map.
	@return The first value in the collection, or <code>null</code> if there is no collection associated with the key or no values in the collection.
	*/
	public V getItem(final K key);

	/**Retrieves iterable access to all items, if any, associated with the given key
	@param key The key in the map.
	@return An object that will iterate all items, if any, associated with the given key.
	*/
	public Iterable<V> getItems(final K key);
}
