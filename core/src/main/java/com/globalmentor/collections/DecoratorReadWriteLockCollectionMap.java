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
import java.util.concurrent.locks.*;

import static java.util.Objects.*;

/**
 * A thread-safe collection map decorator that allows many readers but only one writer to access a map at a time. For operations that iterate over live map
 * data, a read or write lock should be acquired before the call to acquire the data and held until the data is consumed.
 * @param <K> The type of key used in the map.
 * @param <V> The type of value stored in the map.
 * @param <C> The type of collection in which to store values in the map.
 * @author Garret Wilson
 */
public class DecoratorReadWriteLockCollectionMap<K, V, C extends Collection<V>> extends DecoratorReadWriteLockMap<K, C> implements
		ReadWriteLockCollectionMap<K, V, C> {

	/** The collection map this class decorates. */
	protected final CollectionMap<K, V, C> collectionMap;

	/**
	 * Collection map constructor with a default reentrant read/write lock.
	 * @param collectionMap The collection map this map should decorate.
	 * @throws NullPointerException if the provided collection map is <code>null</code>.
	 */
	public DecoratorReadWriteLockCollectionMap(final CollectionMap<K, V, C> collectionMap) {
		this(collectionMap, new ReentrantReadWriteLock()); //create the map with a default lock
	}

	/**
	 * Collection map and read/write lock constructor.
	 * @param collectionMap The collection map this map should decorate.
	 * @param lock The lock for controlling access to the map.
	 * @throws NullPointerException if the provided map and/or lock is <code>null</code>.
	 */
	public DecoratorReadWriteLockCollectionMap(final CollectionMap<K, V, C> collectionMap, final ReadWriteLock lock) {
		super(collectionMap, lock); //construct the parent class
		this.collectionMap = requireNonNull(collectionMap, "Collection map cannot be null"); //save the collection map
	}

	@Override
	public C getCollection(final K key) {
		C collection = get(key); //get the collection of objects for the key, which will be performed with a read lock
		if(collection == null) { //if there is yet no collection for this key
			writeLock().lock(); //get a write lock
			try {
				collection = collectionMap.getCollection(key); //ask the collection map for the collection, which may create and add a collection
			} finally {
				writeLock().unlock(); //always release the write lock
			}
		}
		return collection; //return the collection
	}

	@Override
	public C createCollection() {
		return collectionMap.createCollection();
	}

	@Override
	public boolean hasItems(final K key) {
		readLock().lock();
		try {
			return collectionMap.hasItems(key);
		} finally {
			readLock().unlock();
		}
	}

	@Override
	public int getItemCount(final K key) {
		readLock().lock();
		try {
			return collectionMap.getItemCount(key);
		} finally {
			readLock().unlock();
		}
	}

	@Override
	public void addItem(final K key, final V value) {
		writeLock().lock();
		try {
			collectionMap.addItem(key, value);
		} finally {
			writeLock().unlock();
		}
	}

	@Override
	public V getItem(final K key) {
		readLock().lock();
		try {
			return collectionMap.getItem(key);
		} finally {
			readLock().unlock();
		}
	}

	@Override
	public Iterable<V> getItems(final K key) {
		readLock().lock();
		try {
			return collectionMap.getItems(key);
		} finally {
			readLock().unlock();
		}
	}

	@Override
	public boolean removeItem(final K key, final V value) {
		writeLock().lock();
		try {
			return collectionMap.removeItem(key, value);
		} finally {
			writeLock().unlock();
		}
	}

}
