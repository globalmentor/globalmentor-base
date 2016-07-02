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
import java.util.concurrent.locks.*;

import static java.util.Objects.*;

/**
 * A thread-safe map decorator that allows many readers but only one writer to access a map at a time, and that also allows lookup of the map keys keyed to the
 * values by decorating two maps. For operations that iterate over live map data, a read or write lock should be acquired before the call to acquire the data
 * and held until the data is consumed.
 * @param <K> The type of key used in the map.
 * @param <V> The type of value stored in the map.
 * @author Garret Wilson
 */
public class DecoratorReadWriteLockReverseMap<K, V> extends DecoratorReadWriteLockMap<K, V> implements ReadWriteLockReverseMap<K, V> {

	/** The map containing reverse-lookup values. */
	private final Map<V, K> reverseMap;

	/**
	 * Map constructor with a default reentrant read/write lock.
	 * @param map The map this map should decorate.
	 * @param reverseMap The map to contain reverse lookup values.
	 * @throws NullPointerException if the provided map and/or reverse map is <code>null</code>.
	 */
	public DecoratorReadWriteLockReverseMap(final Map<K, V> map, final Map<V, K> reverseMap) {
		this(map, reverseMap, new ReentrantReadWriteLock()); //create the map with a default lock
	}

	/**
	 * Map and read/write lock constructor.
	 * @param map The map this map should decorate.
	 * @param reverseMap The map to contain reverse lookup values.
	 * @param lock The lock for controlling access to the map.
	 * @throws NullPointerException if the provided map, reverse map, and/or lock is <code>null</code>.
	 */
	public DecoratorReadWriteLockReverseMap(final Map<K, V> map, final Map<V, K> reverseMap, final ReadWriteLock lock) {
		super(map, lock); //construct the parent class
		this.reverseMap = requireNonNull(reverseMap, "Reverse map cannot be null.");
	}

	/**
	 * Returns the key that represents the given value.
	 * @param value The value whose associated key is to be returned.
	 * @return The key to which this map reverse maps the specified value, or <code>null</code> if the map contains no reverse mapping for this value.
	 * @throws ClassCastException Thrown if the value is of an inappropriate type for this map (optional).
	 * @throws NullPointerException Thrown if the value is <code>null</code> and this map does not not permit <code>null</code> values (optional).
	 * @see #containsValue(Object)
	 */
	public K getKey(final V value) {
		readLock().lock();
		try {
			return reverseMap.get(value);
		} finally {
			readLock().unlock();
		}
	} //return the key keyed to the given value in the key map

	/**
	 * Removes the mapping for a value from this map if it is present.
	 * @param value The value whose mapping is to be removed from the map.
	 * @return The previous key associated with the value, or <code>null</code> if there was no mapping for the value.
	 * @throws UnsupportedOperationException if the remove operation is not supported by this map
	 * @throws ClassCastException if the value is of an inappropriate type for this map (optional).
	 * @throws NullPointerException if the specified value is <code>null</code> and this map does not permit <code>null</code> values (optional).
	 */
	public K removeValue(final V value) {
		writeLock().lock(); //get a lock for writing
		try {
			final K oldKey = reverseMap.remove(value); //remove the value from the reverse map
			if(oldKey != null) { //if there was a key associated with the value
				super.remove(oldKey); //remove the old key from the map; call the superclass version so that we won't try to remove values from the reverse map again
			}
			return oldKey; //return the old key, if any
		} finally {
			writeLock().unlock(); //always release the write lock
		}
	}

	/**
	 * Returns <code>true</code> if this map maps a key to the specified value.
	 * <p>
	 * This version uses an internal reverse map to provide faster lookups than the default linear-time lookup.
	 * </p>
	 * @param value The value whose presence in this map is to be tested.
	 * @return <code>true</code> if this map maps a key to the specified value.
	 */
	public boolean containsValue(final Object value) {
		readLock().lock();
		try {
			return reverseMap.containsKey(value);
		} finally {
			readLock().unlock();
		}
	} //see if this value is stored in the key map

	/**
	 * Associates the specified value with the specified key in this map, and associates the specified key with the specified value in the internal reverse map.
	 * <p>
	 * If the map previously contained a mapping for this key, the old value is replaced.
	 * </p>
	 * @param key The key with which the specified value is to be associated.
	 * @param value The value to be associated with the specified key.
	 * @return The previous value associated with specified key, or <code>null</code> if there was no mapping for key. A <code>null</code> return can also
	 *         indicate that the map previously associated <code>null</code> with the specified key.
	 */
	public V put(final K key, final V value) {
		writeLock().lock(); //get a lock for writing
		try {
			final V oldValue = super.put(key, value); //store the value in the map, keyed to the key
			reverseMap.put(value, key); //store the key in the key map, keyed to the value
			return oldValue; //return the old value previously mapped to the key, if any
		} finally {
			writeLock().unlock(); //always release the write lock
		}
	}

	/**
	 * Removes the mapping for this key from this map if it is present.
	 * @param key The key whose mapping is to be removed from the map.
	 * @return The previous value associated with specified key, or <code>null</code> if there was no mapping for key.
	 * @throws ClassCastException if the key is of an inappropriate type for this map (optional).
	 * @throws NullPointerException if the key is <code>null</code> and this map does not permit <code>null</code> keys (optional).
	 * @throws UnsupportedOperationException if the remove method is not supported by this map.
	 */
	public V remove(final Object key) {
		writeLock().lock(); //get a lock for writing
		try {
			final V oldValue = super.remove(key); //remove the key
			if(oldValue != null) { //if there was a value associated with the key
				reverseMap.remove(oldValue); //remove the old value from the reverse map
			}
			return oldValue; //return the old value, if any
		} finally {
			writeLock().unlock(); //always release the write lock
		}
	}

	/** Removes all mappings from this map. */
	public void clear() {
		writeLock().lock(); //get a lock for writing
		try {
			super.clear(); //do the default clearing
			reverseMap.clear(); //clear the key map as well
		} finally {
			writeLock().unlock(); //always release the write lock
		}
	}

}
