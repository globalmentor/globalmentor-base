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
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;

import static java.util.Objects.*;

/**
 * A thread-safe map decorator that allows many readers but only one writer to access a map at a time. For operations that iterate over live map data, a read or
 * write lock should be acquired before the call to acquire the data and held until the data is consumed.
 * @param <K> The type of key used in the map.
 * @param <V> The type of value stored in the map.
 * @author Garret Wilson
 */
public class DecoratorReadWriteLockMap<K, V> extends ReadWriteLockDecorator implements ReadWriteLockMap<K, V> {

	/** The map this class decorates. */
	protected final Map<K, V> map;

	/**
	 * Map constructor with a default reentrant read/write lock.
	 * @param map The map this map should decorate.
	 * @throws NullPointerException if the provided map is <code>null</code>.
	 */
	public DecoratorReadWriteLockMap(final Map<K, V> map) {
		this(map, new ReentrantReadWriteLock()); //create the map with a default lock
	}

	/**
	 * Map and read/write lock constructor.
	 * @param map The map this map should decorate.
	 * @param lock The lock for controlling access to the map.
	 * @throws NullPointerException if the provided map and/or lock is <code>null</code>.
	 */
	public DecoratorReadWriteLockMap(final Map<K, V> map, final ReadWriteLock lock) {
		super(lock); //construct the parent class
		this.map = requireNonNull(map, "Map cannot be null"); //save the map
	}

	@Override
	public int size() {
		readLock().lock();
		try {
			return map.size();
		} finally {
			readLock().unlock();
		}
	}

	@Override
	public boolean isEmpty() {
		readLock().lock();
		try {
			return map.isEmpty();
		} finally {
			readLock().unlock();
		}
	}

	@Override
	public boolean containsKey(Object key) {
		readLock().lock();
		try {
			return map.containsKey(key);
		} finally {
			readLock().unlock();
		}
	}

	@Override
	public boolean containsValue(Object value) {
		readLock().lock();
		try {
			return map.containsValue(value);
		} finally {
			readLock().unlock();
		}
	}

	@Override
	public V get(Object key) {
		readLock().lock();
		try {
			return map.get(key);
		} finally {
			readLock().unlock();
		}
	}

	// Modification Operations

	@Override
	public V put(K key, V value) {
		writeLock().lock();
		try {
			return map.put(key, value);
		} finally {
			writeLock().unlock();
		}
	}

	@Override
	public V remove(Object key) {
		writeLock().lock();
		try {
			return map.remove(key);
		} finally {
			writeLock().unlock();
		}
	}

	// Bulk Operations

	@Override
	public void putAll(Map<? extends K, ? extends V> t) {
		writeLock().lock();
		try {
			map.putAll(t);
		} finally {
			writeLock().unlock();
		}
	}

	@Override
	public void clear() {
		writeLock().lock();
		try {
			map.clear();
		} finally {
			writeLock().unlock();
		}
	}

	// Views

	@Override
	public Set<K> keySet() {
		readLock().lock();
		try {
			return map.keySet();
		} finally {
			readLock().unlock();
		}
	}

	@Override
	public Collection<V> values() {
		readLock().lock();
		try {
			return map.values();
		} finally {
			readLock().unlock();
		}
	}

	@Override
	public Set<Map.Entry<K, V>> entrySet() {
		readLock().lock();
		try {
			return map.entrySet();
		} finally {
			readLock().unlock();
		}
	}

	// Comparison and hashing

	@Override
	public boolean equals(Object o) {
		readLock().lock();
		try {
			return map.equals(o);
		} finally {
			readLock().unlock();
		}
	}

	@Override
	public int hashCode() {
		readLock().lock();
		try {
			return map.hashCode();
		} finally {
			readLock().unlock();
		}
	}

	// Default methods

	@Override
	public V getOrDefault(Object key, V defaultValue) {
		readLock().lock();
		try {
			return map.getOrDefault(key, defaultValue);
		} finally {
			readLock().unlock();
		}
	}

	@Override
	public void forEach(BiConsumer<? super K, ? super V> action) {
		readLock().lock();
		try {
			map.forEach(action);
		} finally {
			readLock().unlock();
		}
	}

	@Override
	public void replaceAll(BiFunction<? super K, ? super V, ? extends V> function) {
		writeLock().lock();
		try {
			map.replaceAll(function);
		} finally {
			writeLock().unlock();
		}
	}

	@Override
	public V putIfAbsent(K key, V value) {
		final V currentValue = get(key);
		if(currentValue == null) {
			writeLock().lock();
			try {
				map.putIfAbsent(key, value); // this will make the check again in a write lock to avoid any race conditions.
			} finally {
				writeLock().unlock();
			}
		}

		return currentValue;
	}

	@Override
	public boolean remove(Object key, Object value) {
		readLock().lock();
		try {
			final Object currentValue = get(key);
			if(!Objects.equals(currentValue, value) || (currentValue == null && !containsKey(key))) {
				return false;
			}
		} finally {
			readLock().unlock();
		}

		writeLock().lock();
		try {
			map.remove(key, value); // this will make the check again in a write lock to avoid any race conditions.
			return true;
		} finally {
			writeLock().unlock();
		}
	}

	@Override
	public boolean replace(K key, V oldValue, V newValue) {
		readLock().lock();
		try {
			final Object currentValue = get(key);
			if(!Objects.equals(currentValue, oldValue) || (currentValue == null && !containsKey(key))) {
				return false;
			}
		} finally {
			readLock().unlock();
		}

		writeLock().lock();
		try {
			return map.replace(key, oldValue, newValue); // this will make the check again in a write lock to avoid any race conditions.
		} finally {
			writeLock().unlock();
		}
	}

	@Override
	public V replace(K key, V value) {
		if(!map.containsKey(key)) { //done under read lock
			return null;
		}

		writeLock().lock();
		try {
			return map.replace(key, value); // this will make the check again in a write lock to avoid any race conditions.
		} finally {
			writeLock().unlock();
		}
	}

	@Override
	public V computeIfAbsent(K key, Function<? super K, ? extends V> mappingFunction) {
		V v;
		if((v = get(key)) == null) { // under read lock.

			writeLock().lock();
			try {
				v = map.computeIfAbsent(key, mappingFunction); // this will make the check again in a write lock to avoid any race conditions.
			} finally {
				writeLock().unlock();
			}

		}

		return v;
	}

	@Override
	public V computeIfPresent(K key, BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
		V oldValue;
		if((oldValue = get(key)) != null) { // under read lock.

			writeLock().lock();
			try {
				oldValue = map.computeIfPresent(key, remappingFunction); // this will make the check again in a write lock to avoid any race conditions.
			} finally {
				writeLock().unlock();
			}

		} else {
			oldValue = null;
		}

		return oldValue;
	}

	@Override
	public V compute(K key, BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
		writeLock().lock();
		try {
			return map.compute(key, remappingFunction);
		} finally {
			writeLock().unlock();
		}
	}

	@Override
	public V merge(K key, V value, BiFunction<? super V, ? super V, ? extends V> remappingFunction) {
		writeLock().lock();
		try {
			return map.merge(key, value, remappingFunction);
		} finally {
			writeLock().unlock();
		}
	}

}
