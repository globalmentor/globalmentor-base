/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.collections;

import java.util.*;

import static java.util.Objects.*;

/**
 * A collection map that wraps an existing collection map, providing access through the {@link CollectionMap} interface. All map access is synchronized on the
 * provided synchronization object.
 * @param <K> The type of key used in the map.
 * @param <V> The type of value stored in the map.
 * @param <C> The type of collection in which to store values in the map.
 * @author Garret Wilson
 */
public class SynchronizedCollectionMapDecorator<K, V, C extends Collection<V>> extends SynchronizedMapDecorator<K, C> implements CollectionMap<K, V, C> {

	/** The collection map this class decorates. */
	protected final CollectionMap<K, V, C> collectionMap;

	/**
	 * Collection map constructor. The new instance of this class is used as a mutex.
	 * @param collectionMap The collection map this collection map should decorate.
	 * @throws NullPointerException if the provided collection map is <code>null</code>.
	 */
	public SynchronizedCollectionMapDecorator(final CollectionMap<K, V, C> collectionMap) {
		super(collectionMap); //construct the parent class with the collection map
		this.collectionMap = requireNonNull(collectionMap, "Collection map cannot be null"); //save the collection map
	}

	/**
	 * Collection map constructor.
	 * @param collectionMap The collection map this collection map should decorate.
	 * @param mutex The mutual exclusion synchronization object.
	 * @throws NullPointerException if the provided collection map and/or mutex is <code>null</code>.
	 */
	public SynchronizedCollectionMapDecorator(final CollectionMap<K, V, C> collectionMap, final Object mutex) {
		super(collectionMap, mutex); //construct the parent class with the collection map
		this.collectionMap = requireNonNull(collectionMap, "Collection map cannot be null"); //save the collection map
	}

	/**
	 * Retrieves the collection of values associated with the given key. If no collection of values is associated with the key, one will be created and added to
	 * the map.
	 * @param key The key in the map.
	 * @return The collection associated with the given key
	 * @see #createCollection()
	 */
	@Override
	public C getCollection(final K key) {
		synchronized(mutex) {
			return collectionMap.getCollection(key);
		}
	}

	/**
	 * {@inheritDoc}
	 * @implNote This implementation is not synchronized.
	 */
	@Override
	public C createCollection() {
		return collectionMap.createCollection();
	}

	@Override
	public boolean hasItems(final K key) {
		synchronized(mutex) {
			return collectionMap.hasItems(key);
		}
	}

	@Override
	public int getItemCount(final K key) {
		synchronized(mutex) {
			return collectionMap.getItemCount(key);
		}
	}

	@Override
	public void addItem(final K key, final V value) {
		synchronized(mutex) {
			collectionMap.addItem(key, value);
		}
	}

	@Override
	public V getItem(final K key) {
		synchronized(mutex) {
			return collectionMap.getItem(key);
		}
	}

	@Override
	public Iterable<V> getItems(final K key) {
		synchronized(mutex) {
			return collectionMap.getItems(key);
		}
	}

	@Override
	public boolean removeItem(final K key, final V value) {
		synchronized(mutex) {
			return collectionMap.removeItem(key, value);
		}
	}

}
