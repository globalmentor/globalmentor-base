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

import java.util.Collection;
import java.util.Map;

/**
 * A map that stores a collection of values for each key, using a pair of read and write locks to access its data, with special methods for retrieving single
 * values.
 * @param <K> The type of key used in the map.
 * @param <V> The type of value stored in each collection in the map.
 * @param <C> The type of collection in which to store values in the map.
 * @author Garret Wilson
 */
public interface CollectionMap<K, V, C extends Collection<V>> extends Map<K, C> {

	/**
	 * Retrieves the collection of values associated with the given key. If no collection of values is associated with the key, one will be created and added to
	 * the map.
	 * @param key The key in the map.
	 * @return The collection associated with the given key
	 * @see #createCollection()
	 */
	public C getCollection(final K key);

	/** Creates a collection in which to store values. */
	public C createCollection();

	/**
	 * Retrieves whether there are items in a collection associated with the key.
	 * @param key The key in the map.
	 * @return <code>true</code> if there is at least one item associated with the key.
	 */
	public boolean hasItems(final K key);

	/**
	 * Retrieves the number of values in the collection, if any, associated with the key.
	 * @param key The key in the map.
	 * @return The number of items associated with the key.
	 */
	public int getItemCount(final K key);

	/**
	 * Adds a value to the collection of values associated with the key. If no collection of values is associated with the key, one will be created and added to
	 * the map.
	 * @param key The key in the map.
	 * @param value The value to store in the collection.
	 */
	public void addItem(final K key, final V value);

	/**
	 * Retrieves the first value from the collection of values, if any, associated with the key.
	 * @param key The key in the map.
	 * @return The first value in the collection, or <code>null</code> if there is no collection associated with the key or no values in the collection.
	 * @see #getCollection(Object)
	 */
	public V getItem(final K key);

	/**
	 * Retrieves iterable access to all items, if any, associated with the given key
	 * @param key The key in the map.
	 * @return An object that will iterate all items, if any, associated with the given key.
	 */
	public Iterable<V> getItems(final K key);

	/**
	 * Removes the first occurence of the given value from the collection of values, if any, associated with the key. If all items from the collection are
	 * removed, the collection itself is removed from the map.
	 * @param key The key in the map.
	 * @param value The item to be removed from the collection, if present.
	 * @return <code>true</code> if an item was removed as a result of this call.
	 */
	public boolean removeItem(final K key, final V value);

}
