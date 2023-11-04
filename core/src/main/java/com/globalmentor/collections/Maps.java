/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import javax.annotation.*;

/**
 * Various utilities to be used with objects implementing the {@link Map} interface.
 * @author Garret Wilson
 */
public class Maps {

	/**
	 * Returns an unmodifiable {@link Map.Entry} containing the given nullable key and nullable value.
	 * @apiNote This method is similar to {@link Map#entry(Object, Object)}, except that this method accepts <code>null</code> keys and values. If it is known
	 *          that the key and value cannot be <code>null</code>, {@link Map#entry(Object, Object)} is preferred, as it is part of the JDK and it is best to
	 *          guard against <code>null</code> value whenever possible.
	 * @implSpec This implementation returns an instance of {@link AbstractMap.SimpleImmutableEntry}.
	 * @param <K> The type of the key.
	 * @param <V> The type of the value.
	 * @param key The map entry key.
	 * @param value The map entry value.
	 * @return A map entry containing the specified key and value.
	 * @see Map#entry(Object, Object)
	 */
	public static <K, V> Map.Entry<K, V> entryOfNullables(@Nullable final K key, @Nullable final V value) {
		return new AbstractMap.SimpleImmutableEntry<>(key, value);
	}

	/**
	 * Puts all given associations from map entries pairs to a map. If more than one entry with the same key is given, the last one will override the others.
	 * @param <K> The map entry key type.
	 * @param <V> The map entry value type.
	 * @param map The map which will receive the name-value mappings.
	 * @param entries The source of map entries.
	 * @return The given map.
	 */
	public static <K, V> Map<K, V> putAll(final Map<K, V> map, final Iterable<Map.Entry<K, V>> entries) {
		for(final Map.Entry<K, V> entriy : entries) { //for all elements already in the iterable
			map.put(entriy.getKey(), entriy.getValue()); //store the value in the map, keyed to the name
		}
		return map; //return the map with the new values added
	}

	/**
	 * Associates the specified value with the specified key in this map. If the map previously contained a mapping for this key, the old value is replaced by the
	 * specified value. If <var>value</var> is <code>null</code>, the value with the specified key will be removed from the map.
	 * @param <K> The type of key that is used by the map.
	 * @param <V> The type of value that is used by the map.
	 * @param map The map into which the value should be put.
	 * @param key The key with which the specified value is to be associated.
	 * @param value The value to be associated with the specified key, or <code>null</code> if the value associated with the key should be removed.
	 * @return The previous value associated with specified key, or <code>null</code> if there was no mapping for key.
	 * @throws UnsupportedOperationException Thrown if the <code>put</code> operation is not supported by this map.
	 * @throws ClassCastException Thrown if the class of the specified key or value prevents it from being stored in this map.
	 * @throws IllegalArgumentException if some aspect of this key or value prevents it from being stored in this map.
	 * @throws NullPointerException this map does not permit <code>null</code> keys, and the specified key is <code>null</code>.
	 */
	public static <K, V> V putRemoveNull(final Map<K, V> map, final K key, final V value) {
		if(value == null) { //if the value is null
			return map.remove(key); //remove the value associated with the key, and return the old value, if any
		} else { //if the value is not null
			return map.put(key, value); //store the value normally, returning the old value, if any
		}
	}

	/**
	 * Retains only the entries in the map the keys of which have a corresponding entry in the given map.
	 * @param <K> The type of keys used in the map.
	 * @param <V> The type of values stored in the map.
	 * @param map The map from which to remove values.
	 * @param set The set to indicate which key entries to retain.
	 */
	public static <K, V> void retainAll(final Map<K, V> map, final Set<? super K> set) {
		final Iterator<Map.Entry<K, V>> entryIterator = map.entrySet().iterator(); //get an iterator to the entries in the map
		while(entryIterator.hasNext()) { //while there are more entries
			final Map.Entry<K, V> entry = entryIterator.next(); //get the next entry
			if(!set.contains(entry.getKey())) { //if this entry is not represented in the set
				entryIterator.remove(); //remove this entry
			}
		}
	}

	/**
	 * Converts the given map to a properties object. If the map is already a properties object, it is returned. Otherwise, a new properties object is created and
	 * populated with the entries of the given map.
	 * @param map The map to convert to a properties object.
	 * @return A properties object, potentially the same instance, containing entries from the given map.
	 */
	public static Properties toProperties(@Nonnull final Map<?, ?> map) {
		if(map instanceof Properties properties) { //if the map is already a properties object
			return properties; //return the map as a properties object
		} else { //if the map is not a properties object
			final Properties properties = new Properties(); //create a new properties object
			properties.putAll(map); //put all the properties from the map
			return properties; //return the populated properties object
		}
	}

}
