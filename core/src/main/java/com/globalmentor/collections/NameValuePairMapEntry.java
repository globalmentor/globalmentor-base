/*
 * Copyright © 2011 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import com.globalmentor.model.NameValuePair;

/**
 * A read-only map entry that is also a name/value pair.
 * 
 * @author Garret Wilson
 * 
 * @param <K> The type of key contained in the map.
 * @param <V> The type of value contained in the map.
 */
public class NameValuePairMapEntry<K, V> extends NameValuePair<K, V> implements Map.Entry<K, V> {

	/**
	 * Constructor specifying the name and value.
	 * @param name The object's new name.
	 * @param value The object's new value
	 */
	public NameValuePairMapEntry(final K name, final V value) {
		super(name, value); //construct the parent class
	}

	/**
	 * Name/value pair copy constructor
	 * @param nameValuePair The name/value pair the values of which to hold in the map entry.
	 * @throws NullPointerException if the given name/value pair is <code>null</code>.
	 */
	public NameValuePairMapEntry(final NameValuePair<K, V> nameValuePair) {
		this(nameValuePair.getName(), nameValuePair.getValue());
	}

	/**
	 * Map entry copy constructor
	 * @param mapEntry The map entry the values of which to hold in the map entry.
	 * @throws NullPointerException if the given map entry is <code>null</code>.
	 */
	public NameValuePairMapEntry(final Map.Entry<K, V> mapEntry) {
		this(mapEntry.getKey(), mapEntry.getValue());
	}

	/** {@inheritDoc} */
	public K getKey() {
		return getName();
	}

	/** {@inheritDoc} */
	public V setValue(V value) {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	public boolean equals(Object o) {
		if(!(o instanceof Map.Entry)) {
			return false;
		}
		final Map.Entry<?, ?> mapEntry = (Map.Entry<?, ?>)o;
		return Objects.equals(getKey(), mapEntry.getKey()) && Objects.equals(getValue(), mapEntry.getValue());
	}

}
