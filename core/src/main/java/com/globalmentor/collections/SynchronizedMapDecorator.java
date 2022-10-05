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
import java.util.function.*;

import static java.util.Objects.*;

/**
 * A map that wraps an existing map, providing access through the {@link Map} interface. All map access is synchronized on the provided synchronization object.
 * @param <K> The type of key used in the map.
 * @param <V> The type of value stored in the map.
 * @author Garret Wilson
 */
public class SynchronizedMapDecorator<K, V> implements Map<K, V> {

	/** The map this class decorates. */
	protected final Map<K, V> map;

	/** The mutual exclusion synchronization object. */
	protected final Object mutex;

	/**
	 * Map constructor. The new instance of this class is used as a mutex.
	 * @param map The map this map should decorate.
	 * @throws NullPointerException if the provided map is <code>null</code>.
	 */
	public SynchronizedMapDecorator(final Map<K, V> map) {
		this.map = requireNonNull(map, "Map cannot be null"); //save the map
		this.mutex = this; //use this instance as a mutex		
	}

	/**
	 * Map and mutex constructor.
	 * @param map The map this map should decorate.
	 * @param mutex The mutual exclusion synchronization object.
	 * @throws NullPointerException if the provided map and/or mutex is <code>null</code>.
	 */
	public SynchronizedMapDecorator(final Map<K, V> map, final Object mutex) {
		this.map = requireNonNull(map, "Map cannot be null"); //save the map
		this.mutex = requireNonNull(mutex, "Mutex cannot be null"); //save the mutex
	}

	@Override
	public int size() {
		synchronized(mutex) {
			return map.size();
		}
	}

	@Override
	public boolean isEmpty() {
		synchronized(mutex) {
			return map.isEmpty();
		}
	}

	@Override
	public boolean containsKey(Object key) {
		synchronized(mutex) {
			return map.containsKey(key);
		}
	}

	@Override
	public boolean containsValue(Object value) {
		synchronized(mutex) {
			return map.containsValue(value);
		}
	}

	@Override
	public V get(Object key) {
		synchronized(mutex) {
			return map.get(key);
		}
	}

	// Modification Operations

	@Override
	public V put(K key, V value) {
		synchronized(mutex) {
			return map.put(key, value);
		}
	}

	@Override
	public V remove(Object key) {
		synchronized(mutex) {
			return map.remove(key);
		}
	}

	// Bulk Operations

	@Override
	public void putAll(Map<? extends K, ? extends V> t) {
		synchronized(mutex) {
			map.putAll(t);
		}
	}

	@Override
	public void clear() {
		synchronized(mutex) {
			map.clear();
		}
	}

	// Views

	@Override
	public Set<K> keySet() {
		synchronized(mutex) {
			return map.keySet();
		}
	}

	@Override
	public Collection<V> values() {
		synchronized(mutex) {
			return map.values();
		}
	}

	@Override
	public Set<Map.Entry<K, V>> entrySet() {
		synchronized(mutex) {
			return map.entrySet();
		}
	}

	// Comparison and hashing

	@Override
	public boolean equals(Object o) {
		synchronized(mutex) {
			return map.equals(o);
		}
	}

	@Override
	public int hashCode() {
		synchronized(mutex) {
			return map.hashCode();
		}
	}

	// Default methods

	@Override
	public V getOrDefault(Object key, V defaultValue) {
		synchronized(mutex) {
			return map.getOrDefault(key, defaultValue);
		}
	}

	@Override
	public void forEach(BiConsumer<? super K, ? super V> action) {
		synchronized(mutex) {
			map.forEach(action);
		}
	}

	@Override
	public void replaceAll(BiFunction<? super K, ? super V, ? extends V> function) {
		synchronized(mutex) {
			map.replaceAll(function);
		}
	}

	@Override
	public V putIfAbsent(K key, V value) {
		synchronized(mutex) {
			return map.putIfAbsent(key, value);
		}
	}

	@Override
	public boolean remove(Object key, Object value) {
		synchronized(mutex) {
			return map.remove(key, value);
		}
	}

	@Override
	public boolean replace(K key, V oldValue, V newValue) {
		synchronized(mutex) {
			return map.replace(key, oldValue, newValue);
		}
	}

	@Override
	public V replace(K key, V value) {
		synchronized(mutex) {
			return map.replace(key, value);
		}
	}

	@Override
	public V computeIfAbsent(K key, Function<? super K, ? extends V> mappingFunction) {
		synchronized(mutex) {
			return map.computeIfAbsent(key, mappingFunction);
		}
	}

	@Override
	public V computeIfPresent(K key, BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
		synchronized(mutex) {
			return map.computeIfPresent(key, remappingFunction);
		}
	}

	@Override
	public V compute(K key, BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
		synchronized(mutex) {
			return map.compute(key, remappingFunction);
		}
	}

	@Override
	public V merge(K key, V value, BiFunction<? super V, ? super V, ? extends V> remappingFunction) {
		synchronized(mutex) {
			return map.merge(key, value, remappingFunction);
		}
	}

}
