package com.garretwilson.util;

import java.util.*;

/**A map that stores a list of values for each key, with special methods for retrieving single values.
@author Garret Wilson
*/
public interface ListMap<K, V> extends Map<K, List<V>> 
{
	/**Adds a value to the list of values associated with the key.
	If no list of values is associated with the key, one will be created and added to the map.
	@param key The key in the map.
	@param value The value to store in the list.
	*/
	public void addItem(final K key, final V value);

	/**Retrieves the first value from the list of values, if any, associated with the key.
	@param key The key in the map.
	@return The first value in the list, or <code>null</code> if there is no list associated with the key or no values in the list.
	*/
	public V getItem(final K key);
}