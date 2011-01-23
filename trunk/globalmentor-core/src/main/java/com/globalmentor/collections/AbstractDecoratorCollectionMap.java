/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.collections;

import java.util.*;

import static com.globalmentor.collections.iterators.Iterators.*;

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
	@SuppressWarnings("unchecked")
	public Iterable<V> getItems(final K key)
	{
		final C collection=get(key);	//get the collection of objects for the key
		return collection!=null ? collection : (Iterable<V>)EMPTY_ITERABLE;	//return the collection or an empty iterable if there is no collection for this key
	}

	/**Removes the first occurence of the given value from the collection of values, if any, associated with the key.
	If all items from the collection are removed, the collection itself is removed from the map.
	@param key The key in the map.
	@param value The item to be removed from the collection, if present.
	@return <code>true</code> if an item was removed as a result of this call.
	*/
	public boolean removeItem(final K key, final V value)
	{
		final C collection=get(key);	//get the collection of objects for the key, if any
		final boolean removed;	//we'll determined if the item was removed
		if(collection!=null)	//if there is a collection associated with this key
		{
			removed=collection.remove(value);	//try to remove the item from the collection
			if(removed)	//if we removed something
			{
				if(collection.isEmpty())	//if the collection no longer has any values
				{
					remove(key);	//remove the entire collection from the map
				}
			}
		}
		else	//if there is no collection associated with this key
		{
			removed=false;	//there is nothing to remove
		}
		return removed;	//return whether we removed an item
	}

}