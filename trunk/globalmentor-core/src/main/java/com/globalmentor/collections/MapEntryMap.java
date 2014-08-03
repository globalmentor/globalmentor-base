/*
 * Copyright © 2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import com.globalmentor.java.Objects;
import com.globalmentor.model.NameValuePair;

/**
 * A read-only map containing a single map entry.
 * 
 * @author Garret Wilson
 * 
 * @param <K> The type of key contained in the map.
 * @param <V> The type of value contained in the map.
 */
public class MapEntryMap<K, V> implements Map<K, V>, ImmutableMap<K, V> {

	/** The map entry held in the map. */
	private Map.Entry<K, V> mapEntry;

	/**
	 * Key and value constructor.
	 * @param key The key to hold in the map.
	 * @param value The value to hold in the map.
	 */
	public MapEntryMap(final K key, final V value) {
		mapEntry = new NameValuePairMapEntry<K, V>(key, value);
	}

	/**
	 * Name/value pair copy constructor.
	 * @param nameValuePair The name/value pair the values of which to hold in the map.
	 * @throws NullPointerException if the given name/value pair is <code>null</code>.
	 */
	public MapEntryMap(final NameValuePair<K, V> nameValuePair) {
		mapEntry = new NameValuePairMapEntry<K, V>(nameValuePair);
	}

	/**
	 * Map entry copy constructor.
	 * @param mapEntry The map entry the values of which to hold in the map.
	 * @throws NullPointerException if the given map entry is <code>null</code>.
	 */
	public MapEntryMap(final Map.Entry<K, V> mapEntry) {
		this.mapEntry = new NameValuePairMapEntry<K, V>(mapEntry);
	}

	/** {@inheritDoc} */
	public int size() {
		return 1;
	}

	/** {@inheritDoc} */
	public boolean isEmpty() {
		return false;
	}

	/** {@inheritDoc} */
	public boolean containsKey(Object key) {
		return Objects.equals(key, mapEntry.getKey());
	}

	/** {@inheritDoc} */
	public boolean containsValue(Object value) {
		return Objects.equals(value, mapEntry.getValue());
	}

	/** {@inheritDoc} */
	public V get(Object key) {
		return Objects.equals(key, mapEntry.getKey()) ? mapEntry.getValue() : null;
	}

	/** {@inheritDoc} */
	public V put(K key, V value) {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	public V remove(Object key) {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	public void putAll(Map<? extends K, ? extends V> m) {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	public void clear() {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	public Set<K> keySet() {
		return new ObjectSet<K>(mapEntry.getKey());
	}

	/** {@inheritDoc} */
	public Collection<V> values() {
		return new ObjectSet<V>(mapEntry.getValue());
	}

	/** {@inheritDoc} */
	public Set<Map.Entry<K, V>> entrySet() {
		return new ObjectSet<Map.Entry<K, V>>(mapEntry);
	}

	/** {@inheritDoc} */
	public boolean equals(Object o) {
		if(!(o instanceof Map)) {
			return false;
		}
		final Map<?, ?> map = (Map<?, ?>)o;
		return map.size() == 1 && mapEntry.equals(map.entrySet().iterator().next());
	}

	/** {@inheritDoc} */
	public int hashCode() {
		return mapEntry.hashCode();
	}

}
