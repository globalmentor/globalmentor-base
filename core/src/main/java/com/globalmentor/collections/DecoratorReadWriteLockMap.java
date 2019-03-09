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

	/**
	 * Returns the number of key-value mappings in this map. If the map contains more than <code>Integer.MAX_VALUE</code> elements, returns <code>Integer.MAX_VALUE</code>
	 * .
	 *
	 * @return the number of key-value mappings in this map.
	 */
	public int size() {
		readLock().lock();
		try {
			return map.size();
		} finally {
			readLock().unlock();
		}
	}

	/**
	 * Returns <code>true</code> if this map contains no key-value mappings.
	 *
	 * @return <code>true</code> if this map contains no key-value mappings.
	 */
	public boolean isEmpty() {
		readLock().lock();
		try {
			return map.isEmpty();
		} finally {
			readLock().unlock();
		}
	}

	/**
	 * Returns <code>true</code> if this map contains a mapping for the specified key. More formally, returns <code>true</code> if and only if this map contains a mapping
	 * for a key <code>k</code> such that <code>(key==null ? k==null : key.equals(k))</code>. (There can be at most one such mapping.)
	 *
	 * @param key key whose presence in this map is to be tested.
	 * @return <code>true</code> if this map contains a mapping for the specified key.
	 * 
	 * @throws ClassCastException if the key is of an inappropriate type for this map (optional).
	 * @throws NullPointerException if the key is <code>null</code> and this map does not permit <code>null</code> keys (optional).
	 */
	public boolean containsKey(Object key) {
		readLock().lock();
		try {
			return map.containsKey(key);
		} finally {
			readLock().unlock();
		}
	}

	/**
	 * Returns <code>true</code> if this map maps one or more keys to the specified value. More formally, returns <code>true</code> if and only if this map contains at
	 * least one mapping to a value <code>v</code> such that <code>(value==null ? v==null : value.equals(v))</code>. This operation will probably require time linear in
	 * the map size for most implementations of the <code>Map</code> interface.
	 *
	 * @param value value whose presence in this map is to be tested.
	 * @return <code>true</code> if this map maps one or more keys to the specified value.
	 * @throws ClassCastException if the value is of an inappropriate type for this map (optional).
	 * @throws NullPointerException if the value is <code>null</code> and this map does not permit <code>null</code> values (optional).
	 */
	public boolean containsValue(Object value) {
		readLock().lock();
		try {
			return map.containsValue(value);
		} finally {
			readLock().unlock();
		}
	}

	/**
	 * Returns the value to which this map maps the specified key. Returns <code>null</code> if the map contains no mapping for this key. A return value of
	 * <code>null</code> does not <i>necessarily</i> indicate that the map contains no mapping for the key; it's also possible that the map explicitly maps the key to
	 * <code>null</code>. The <code>containsKey</code> operation may be used to distinguish these two cases.
	 *
	 * <p>
	 * More formally, if this map contains a mapping from a key <code>k</code> to a value <code>v</code> such that <code>(key==null ? k==null :
	 * key.equals(k))</code>, then this method returns <code>v</code>; otherwise it returns <code>null</code>. (There can be at most one such mapping.)
	 *
	 * @param key key whose associated value is to be returned.
	 * @return the value to which this map maps the specified key, or <code>null</code> if the map contains no mapping for this key.
	 * 
	 * @throws ClassCastException if the key is of an inappropriate type for this map (optional).
	 * @throws NullPointerException if the key is <code>null</code> and this map does not permit <code>null</code> keys (optional).
	 * 
	 * @see #containsKey(Object)
	 */
	public V get(Object key) {
		readLock().lock();
		try {
			return map.get(key);
		} finally {
			readLock().unlock();
		}
	}

	// Modification Operations

	/**
	 * Associates the specified value with the specified key in this map (optional operation). If the map previously contained a mapping for this key, the old
	 * value is replaced by the specified value. (A map <code>m</code> is said to contain a mapping for a key <code>k</code> if and only if {@link #containsKey(Object)
	 * m.containsKey(k)} would return <code>true</code>.))
	 *
	 * @param key key with which the specified value is to be associated.
	 * @param value value to be associated with the specified key.
	 * @return previous value associated with specified key, or <code>null</code> if there was no mapping for key. A <code>null</code> return can also indicate that the
	 *         map previously associated <code>null</code> with the specified key, if the implementation supports <code>null</code> values.
	 * 
	 * @throws UnsupportedOperationException if the <code>put</code> operation is not supported by this map.
	 * @throws ClassCastException if the class of the specified key or value prevents it from being stored in this map.
	 * @throws IllegalArgumentException if some aspect of this key or value prevents it from being stored in this map.
	 * @throws NullPointerException if this map does not permit <code>null</code> keys or values, and the specified key or value is <code>null</code>.
	 */
	public V put(K key, V value) {
		writeLock().lock();
		try {
			return map.put(key, value);
		} finally {
			writeLock().unlock();
		}
	}

	/**
	 * Removes the mapping for this key from this map if it is present (optional operation). More formally, if this map contains a mapping from key <code>k</code> to
	 * value <code>v</code> such that <code>(key==null ?  k==null : key.equals(k))</code>, that mapping is removed. (The map can contain at most one such mapping.)
	 *
	 * <p>
	 * Returns the value to which the map previously associated the key, or <code>null</code> if the map contained no mapping for this key. (A <code>null</code> return
	 * can also indicate that the map previously associated <code>null</code> with the specified key if the implementation supports <code>null</code> values.) The map
	 * will not contain a mapping for the specified key once the call returns.
	 *
	 * @param key key whose mapping is to be removed from the map.
	 * @return previous value associated with specified key, or <code>null</code> if there was no mapping for key.
	 *
	 * @throws ClassCastException if the key is of an inappropriate type for this map (optional).
	 * @throws NullPointerException if the key is <code>null</code> and this map does not permit <code>null</code> keys (optional).
	 * @throws UnsupportedOperationException if the <code>remove</code> method is not supported by this map.
	 */
	public V remove(Object key) {
		writeLock().lock();
		try {
			return map.remove(key);
		} finally {
			writeLock().unlock();
		}
	}

	// Bulk Operations

	/**
	 * Copies all of the mappings from the specified map to this map (optional operation). The effect of this call is equivalent to that of calling
	 * {@link #put(Object,Object) put(k, v)} on this map once for each mapping from key <code>k</code> to value <code>v</code> in the specified map. The behavior of this
	 * operation is unspecified if the specified map is modified while the operation is in progress.
	 *
	 * @param t Mappings to be stored in this map.
	 * 
	 * @throws UnsupportedOperationException if the <code>putAll</code> method is not supported by this map.
	 * 
	 * @throws ClassCastException if the class of a key or value in the specified map prevents it from being stored in this map.
	 * 
	 * @throws IllegalArgumentException some aspect of a key or value in the specified map prevents it from being stored in this map.
	 * @throws NullPointerException if the specified map is <code>null</code>, or if this map does not permit <code>null</code> keys or values, and the specified map
	 *           contains <code>null</code> keys or values.
	 */
	public void putAll(Map<? extends K, ? extends V> t) {
		writeLock().lock();
		try {
			map.putAll(t);
		} finally {
			writeLock().unlock();
		}
	}

	/**
	 * Removes all mappings from this map (optional operation).
	 *
	 * @throws UnsupportedOperationException clear is not supported by this map.
	 */
	public void clear() {
		writeLock().lock();
		try {
			map.clear();
		} finally {
			writeLock().unlock();
		}
	}

	// Views

	/**
	 * Returns a set view of the keys contained in this map. The set is backed by the map, so changes to the map are reflected in the set, and vice-versa. If the
	 * map is modified while an iteration over the set is in progress (except through the iterator's own <code>remove</code> operation), the results of the iteration
	 * are undefined. The set supports element removal, which removes the corresponding mapping from the map, via the <code>Iterator.remove</code>,
	 * <code>Set.remove</code>, <code>removeAll</code> <code>retainAll</code>, and <code>clear</code> operations. It does not support the add or <code>addAll</code> operations.
	 *
	 * @return a set view of the keys contained in this map.
	 */
	public Set<K> keySet() {
		readLock().lock();
		try {
			return map.keySet();
		} finally {
			readLock().unlock();
		}
	}

	/**
	 * Returns a collection view of the values contained in this map. The collection is backed by the map, so changes to the map are reflected in the collection,
	 * and vice-versa. If the map is modified while an iteration over the collection is in progress (except through the iterator's own <code>remove</code> operation),
	 * the results of the iteration are undefined. The collection supports element removal, which removes the corresponding mapping from the map, via the
	 * <code>Iterator.remove</code>, <code>Collection.remove</code>, <code>removeAll</code>, <code>retainAll</code> and <code>clear</code> operations. It does not support the add or
	 * <code>addAll</code> operations.
	 *
	 * @return a collection view of the values contained in this map.
	 */
	public Collection<V> values() {
		readLock().lock();
		try {
			return map.values();
		} finally {
			readLock().unlock();
		}
	}

	/**
	 * Returns a set view of the mappings contained in this map. Each element in the returned set is a {@link Map.Entry}. The set is backed by the map, so changes
	 * to the map are reflected in the set, and vice-versa. If the map is modified while an iteration over the set is in progress (except through the iterator's
	 * own <code>remove</code> operation, or through the <code>setValue</code> operation on a map entry returned by the iterator) the results of the iteration are
	 * undefined. The set supports element removal, which removes the corresponding mapping from the map, via the <code>Iterator.remove</code>, <code>Set.remove</code>,
	 * <code>removeAll</code>, <code>retainAll</code> and <code>clear</code> operations. It does not support the <code>add</code> or <code>addAll</code> operations.
	 *
	 * @return a set view of the mappings contained in this map.
	 */
	public Set<Map.Entry<K, V>> entrySet() {
		readLock().lock();
		try {
			return map.entrySet();
		} finally {
			readLock().unlock();
		}
	}

	// Comparison and hashing

	/**
	 * Compares the specified object with this map for equality. Returns <code>true</code> if the given object is also a map and the two Maps represent the same
	 * mappings. More formally, two maps <code>t1</code> and <code>t2</code> represent the same mappings if <code>t1.entrySet().equals(t2.entrySet())</code>. This ensures
	 * that the <code>equals</code> method works properly across different implementations of the <code>Map</code> interface.
	 *
	 * @param o object to be compared for equality with this map.
	 * @return <code>true</code> if the specified object is equal to this map.
	 */
	public boolean equals(Object o) {
		readLock().lock();
		try {
			return map.equals(o);
		} finally {
			readLock().unlock();
		}
	}

	/**
	 * Returns the hash code value for this map. The hash code of a map is defined to be the sum of the hashCodes of each entry in the map's entrySet view. This
	 * ensures that <code>t1.equals(t2)</code> implies that <code>t1.hashCode()==t2.hashCode()</code> for any two maps <code>t1</code> and <code>t2</code>, as required by the
	 * general contract of Object.hashCode.
	 *
	 * @return the hash code value for this map.
	 * @see Map.Entry#hashCode()
	 * @see Object#hashCode()
	 * @see Object#equals(Object)
	 * @see #equals(Object)
	 */
	public int hashCode() {
		readLock().lock();
		try {
			return map.hashCode();
		} finally {
			readLock().unlock();
		}
	}

}
