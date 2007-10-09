package com.garretwilson.util;

import java.util.*;

import static com.garretwilson.util.IteratorUtilities.*;

/**An abstract collection map that decorates an existing map.
Child classes must implements {@link #createCollection()} and return the appropriate type of collection.
@param <K> The type of map key.
@param <V> The type of map value.
@param <C> The type of collection in which to store values in the map.
@author Garret Wilson
*/
public abstract class AbstractDecoratorCollectionMap<K, V, C extends Collection<V>> extends MapDecorator<K, C> implements CollectionMap<K, V, C>
{

	/**Map constructor.
	@param map The map this map should decorate.
	@exception NullPointerException if the provided map is <code>null</code>.
	*/
	public AbstractDecoratorCollectionMap(final Map<K, C> map)
	{
		super(map);	//construct the parent class
	}

	/**Retrieves whether there are items in a collection associated with the key.
	@param key The key in the map.
	@return <code>true</code> if there is at least one item associated with the key.
	*/
	public boolean hasItems(final K key)
	{
		final C collection=get(key);	//get the collection of objects for the key, if any
		return collection!=null ? !collection.isEmpty() : false;	//if there is no collection, there are no items
	}

	/**Retrieves the number of values in the collection, if any, associated with the key.
	@param key The key in the map.
	@return The number of items associated with the key.
	*/
	public int getItemCount(final K key)
	{
		final C collection=get(key);	//get the collection of objects for the key, if any
		return collection!=null ? collection.size() : 0;	//return the size of the item collection, if there is one
	}

	/**Adds a value to the collection of values associated with the key.
	If no collection of values is associated with the key, one will be created and added to the map.
	@param key The key in the map.
	@param value The value to store in the collection.
	@see #getCollection(Object)
	*/
	public void addItem(final K key, final V value)
	{
		getCollection(key).add(value);	//add this value to the collection
	}

	/**Retrieves the first value from the collection of values, if any, associated with the key.
	@param key The key in the map.
	@return The first value in the collection, or <code>null</code> if there is no collection associated with the key or no values in the collection.
	*/
	public V getItem(final K key)
	{
		final C collection=get(key);	//get the collection of objects for the key, if any
		return collection!=null && !collection.isEmpty() ? collection.iterator().next() : null;	//return the first object in the collection, if there is a non-empty collection
	}

	/**Retrieves iterable access to all items, if any, associated with the given key
	@param key The key in the map.
	@return An object that will iterate all items, if any, associated with the given key.
	*/
	public Iterable<V> getItems(final K key)
	{
		final C collection=get(key);	//get the collection of objects for the key
		return collection!=null ? collection : (Iterable<V>)EMPTY_ITERABLE;	//return the collection or an empty iterable if there is no collection for this key
	}

	/**Retrieves the collection of values associated with the given key.
	If no collection of values is associated with the key, one will be created and added to the map.
	@param key The key in the map.
	@return The collection associated with the given key
	@see #createCollection()
	*/
	public C getCollection(final K key)
	{
		C collection=get(key);	//get the collection of objects for the key
		if(collection==null)	//if there is yet no collection for this key
		{
			collection=createCollection();	//create a new collection
			put(key, collection);	//store the collection in the map
		}
		return collection;	//return the collection
	}
}