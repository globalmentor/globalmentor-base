package com.garretwilson.util;

import java.util.*;

/**An abstract list map that decorates an existing map.
Child classes must implements {@link #createList()} and return the appropriate type of list.
@param <K> The type of map key.
@param <V> The type of map value.
@author Garret Wilson
*/
public abstract class AbstractDecoratorListMap<K, V> extends MapDecorator<K, List<V>> implements ListMap<K, V>
{

	/**Map constructor.
	@param map The map this map should decorate.
	@exception NullPointerException if the provided map is <code>null</code>.
	*/
	public AbstractDecoratorListMap(final Map<K, List<V>> map)
	{
		super(map);	//construct the parent class
	}

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
			list=createList();	//create a new list
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

	/**Retrieves iterable access to all items, if any, associated with the given key
	@param key The key in the map.
	@return An object that will iterate all items, if any, associated with the given key.
	*/
	public Iterable<V> getItems(final K key)
	{
		final List<V> list=get(key);	//get the list of objects for the key
		return list!=null ? list : new EmptyIterable<V>();	//return the list or an empty iterable if there is no list for this key
	}

	/**Creates a list in which to store values.*/
	protected abstract List<V> createList();
}