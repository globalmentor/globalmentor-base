package com.garretwilson.util;

import java.util.*;

/**An implementation of a map that stores a list of values for each key, with special methods for retrieving single values.
Array lists are stored in a hash map.
@author Garret Wilson
*/
public class ArrayListHashMap<K, V> extends HashMap<K, List<V>> implements ListMap<K, V> 
{

	/**Adds a value to the list of values associated with the key.
	If no list of values is associated with the key, one will be created and added to the map.
	@param key The key in the map.
	@param value The value to store in the list.
	*/
	public void addItem(final K key, final V value)
	{
		List<V> list=get(key);	//get the list of objects for the key
		if(list==null)	//if there is yet no list for this key
		{
			list=new ArrayList<V>();	//create a new list
			put(key, list);	//store the list in the map
		}
		list.add(value);	//add this value to the list
	}

	/**Retrieves the first value from the list of values, if any, associated with the key.
	@param key The key in the map.
	@return The first value in the list, or <code>null</code> if there is no list associated with the key or no values in the list.
	*/
	public V getItem(final K key)
	{
		final List<V> list=get(key);	//get the list of objects for the key
		return list!=null && !list.isEmpty() ? list.get(0) : null;	//return the first object in the list, if there is a non-empty list
	}

}
