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
import java.util.function.*;

import static java.util.Objects.*;

/**
 * A thread-safe map decorator that allows many readers but only one writer to access a map at a time, and that also allows lookup of the map keys keyed to the
 * values by decorating two maps. For operations that iterate over live map data, a read or write lock should be acquired before the call to acquire the data
 * and held until the data is consumed.
 * @apiNote This implementation does not support <code>null</code> keys or values.
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

	@Override
	public K getKey(final V value) {
		requireNonNull(value, "Value cannot be null.");
		readLock().lock();
		try {
			return reverseMap.get(value);
		} finally {
			readLock().unlock();
		}
	} //return the key keyed to the given value in the key map

	@Override
	public K removeValue(final V value) {
		requireNonNull(value, "Value cannot be null.");
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
	 * {@inheritDoc}
	 * @implNote This implementation uses an internal reverse map to provide faster lookups than the default linear-time lookup.
	 */
	@Override
	public boolean containsValue(final Object value) {
		readLock().lock();
		try {
			return reverseMap.containsKey(value);
		} finally {
			readLock().unlock();
		}
	} //see if this value is stored in the key map

	/**
	 * {@inheritDoc}
	 * @implNote This implementation maintains the one-to-one relationship by removing any existing mapping when a value is reassigned to a different key.
	 */
	@Override
	public V put(final K key, final V value) {
		requireNonNull(key, "Key cannot be null.");
		requireNonNull(value, "Value cannot be null.");
		writeLock().lock(); //get a lock for writing
		try {
			final V oldValue = super.put(key, value); //store the value in the map, keyed to the key
			//Check if this value was already mapped to a different key
			final K oldKeyForValue = reverseMap.put(value, key); //store the key in the key map, keyed to the value
			if(oldKeyForValue != null && !oldKeyForValue.equals(key)) { //if the value was previously mapped to a different key
				super.remove(oldKeyForValue); //remove the old key from the map to maintain one-to-one relationship
			}
			//Clean up the old value's reverse mapping if needed
			if(oldValue != null && !oldValue.equals(value)) { //if there was a different old value
				reverseMap.remove(oldValue); //remove the old value from the reverse map
			}
			return oldValue; //return the old value previously mapped to the key, if any
		} finally {
			writeLock().unlock(); //always release the write lock
		}
	}

	@Override
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

	@Override
	public void clear() {
		writeLock().lock(); //get a lock for writing
		try {
			super.clear(); //do the default clearing
			reverseMap.clear(); //clear the key map as well
		} finally {
			writeLock().unlock(); //always release the write lock
		}
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation ensures the reverse map is updated to maintain the one-to-one relationship between keys and values.
	 */
	@Override
	public V putIfAbsent(final K key, final V value) {
		requireNonNull(key, "Key cannot be null.");
		requireNonNull(value, "Value cannot be null.");
		writeLock().lock(); //get a lock for writing (we may modify)
		try {
			final V existingValue = get(key); //check if there's already a value for this key
			if(existingValue != null) {
				return existingValue; //return the existing value without modification
			}
			//No existing value, so perform the put operation
			return put(key, value); //use our overridden put() to maintain consistency
		} finally {
			writeLock().unlock();
		}
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation ensures the reverse map is updated to maintain the one-to-one relationship between keys and values.
	 */
	@Override
	public boolean remove(final Object key, final Object value) {
		writeLock().lock(); //get a lock for writing
		try {
			final V currentValue = get(key);
			if(currentValue == null || !currentValue.equals(value)) {
				return false; //key is not mapped to the specified value
			}
			remove(key); //use our overridden remove(Object) to maintain consistency
			return true;
		} finally {
			writeLock().unlock();
		}
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation ensures the reverse map is updated to maintain the one-to-one relationship between keys and values.
	 */
	@Override
	public boolean replace(final K key, final V oldValue, final V newValue) {
		requireNonNull(key, "Key cannot be null.");
		requireNonNull(oldValue, "Old value cannot be null.");
		requireNonNull(newValue, "New value cannot be null.");
		writeLock().lock(); //get a lock for writing
		try {
			final V currentValue = get(key);
			if(currentValue == null || !currentValue.equals(oldValue)) {
				return false; //key is not mapped to the old value
			}
			put(key, newValue); //use our overridden put() to maintain consistency
			return true;
		} finally {
			writeLock().unlock();
		}
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation ensures the reverse map is updated to maintain the one-to-one relationship between keys and values.
	 */
	@Override
	public V replace(final K key, final V value) {
		requireNonNull(key, "Key cannot be null.");
		requireNonNull(value, "Value cannot be null.");
		writeLock().lock(); //get a lock for writing
		try {
			if(!containsKey(key)) {
				return null; //key is not present
			}
			return put(key, value); //use our overridden put() to maintain consistency
		} finally {
			writeLock().unlock();
		}
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation ensures the reverse map is updated to maintain the one-to-one relationship between keys and values.
	 */
	@Override
	public V computeIfAbsent(final K key, final Function<? super K, ? extends V> mappingFunction) {
		requireNonNull(key, "Key cannot be null.");
		requireNonNull(mappingFunction);
		writeLock().lock(); //get a lock for writing (we may modify)
		try {
			final V existingValue = get(key);
			if(existingValue != null) {
				return existingValue;
			}
			final V newValue = mappingFunction.apply(key);
			if(newValue != null) {
				put(key, newValue); //use our overridden put() to maintain consistency
			}
			return newValue;
		} finally {
			writeLock().unlock();
		}
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation ensures the reverse map is updated to maintain the one-to-one relationship between keys and values.
	 */
	@Override
	public V computeIfPresent(final K key, final BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
		requireNonNull(key, "Key cannot be null.");
		requireNonNull(remappingFunction);
		writeLock().lock(); //get a lock for writing
		try {
			final V oldValue = get(key);
			if(oldValue == null) {
				return null;
			}
			final V newValue = remappingFunction.apply(key, oldValue);
			if(newValue != null) {
				put(key, newValue); //use our overridden put() to maintain consistency
			} else {
				remove(key); //remove the mapping if the new value is null
			}
			return newValue;
		} finally {
			writeLock().unlock();
		}
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation ensures the reverse map is updated to maintain the one-to-one relationship between keys and values.
	 */
	@Override
	public V compute(final K key, final BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
		requireNonNull(key, "Key cannot be null.");
		requireNonNull(remappingFunction);
		writeLock().lock(); //get a lock for writing
		try {
			final V oldValue = get(key);
			final V newValue = remappingFunction.apply(key, oldValue);
			if(newValue != null) {
				put(key, newValue); //use our overridden put() to maintain consistency
			} else if(oldValue != null) {
				remove(key); //remove the mapping if the new value is null and there was an old value
			}
			return newValue;
		} finally {
			writeLock().unlock();
		}
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation ensures the reverse map is updated to maintain the one-to-one relationship between keys and values.
	 */
	@Override
	public V merge(final K key, final V value, final BiFunction<? super V, ? super V, ? extends V> remappingFunction) {
		requireNonNull(key, "Key cannot be null.");
		requireNonNull(value);
		requireNonNull(remappingFunction);
		writeLock().lock(); //get a lock for writing
		try {
			final V oldValue = get(key);
			final V newValue = (oldValue == null) ? value : remappingFunction.apply(oldValue, value);
			if(newValue != null) {
				put(key, newValue); //use our overridden put() to maintain consistency
			} else {
				remove(key); //remove the mapping if the result is null
			}
			return newValue;
		} finally {
			writeLock().unlock();
		}
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation ensures the reverse map is updated to maintain the one-to-one relationship between keys and values. If the remapping function
	 *           produces the same value for multiple keys, the last key processed will retain the mapping and earlier keys will be removed from the map.
	 */
	@Override
	public void replaceAll(final BiFunction<? super K, ? super V, ? extends V> function) {
		requireNonNull(function);
		writeLock().lock(); //get a lock for writing
		try {
			//Collect entries first to avoid ConcurrentModificationException
			final Map<K, V> updates = new java.util.LinkedHashMap<>();
			for(final Map.Entry<K, V> entry : entrySet()) {
				final K key = entry.getKey();
				final V oldValue = entry.getValue();
				final V newValue = function.apply(key, oldValue);
				if(newValue != null) {
					updates.put(key, newValue);
				} else {
					updates.put(key, null); //mark for removal
				}
			}
			//Apply updates
			for(final Map.Entry<K, V> entry : updates.entrySet()) {
				final K key = entry.getKey();
				final V newValue = entry.getValue();
				if(newValue != null) {
					put(key, newValue); //use our overridden put() to maintain consistency
				} else {
					remove(key); //remove the mapping if the new value is null
				}
			}
		} finally {
			writeLock().unlock();
		}
	}

}
