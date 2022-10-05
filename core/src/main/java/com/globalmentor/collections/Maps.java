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

import static java.util.Collections.*;

import java.util.*;
import java.util.Collections;

import javax.annotation.*;

import com.globalmentor.java.CloneSupported;
import com.globalmentor.java.Objects;
import com.globalmentor.model.NameValuePair;

/**
 * Various utilities to be used with objects implementing the {@link Map} interface.
 * @author Garret Wilson
 */
public class Maps {

	/**
	 * Adds values from an array of name-value pairs to a map. If more than one pair with the same name is given, the last one will override the others.
	 * @param <N> The type of the name used as key of the map.
	 * @param <V> The type of the value of the map.
	 * @param map The map to receive the name-value pair names and values.
	 * @param nameValuePairs An array of name-value pairs.
	 * @return The given map.
	 */
	public static <N, V> Map<N, V> addAll(final Map<N, V> map, final NameValuePair<N, V>[] nameValuePairs) {
		for(final NameValuePair<N, V> nameValuePair : nameValuePairs) { //look at each name-value pair
			map.put(nameValuePair.getName(), nameValuePair.getValue()); //add the name-value pair name and value to the map
		}
		return map; //return the map with the new values added
	}

	/**
	 * Adds all given values from name-value pairs to a map, keyed to the names of those name-value pairs. If more than one pair with the same name is given, the
	 * last one will override the others.
	 * @param <K> The name-value pair name type.
	 * @param <V> The name-value pair value type.
	 * @param map The map which will receive the name-value mappings.
	 * @param iterable The source of name-value pairs.
	 * @return The given map.
	 */
	public static <K, V> Map<K, V> addAll(final Map<K, V> map, final Iterable<NameValuePair<K, V>> iterable) {
		for(final NameValuePair<K, V> nameValuePair : iterable) { //for all elements already in the iterable
			map.put(nameValuePair.getName(), nameValuePair.getValue()); //store the value in the map, keyed to the name
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
	public static <K, V> void retainAll(final Map<K, V> map, final Set<K> set) {
		final Iterator<Map.Entry<K, V>> entryIterator = map.entrySet().iterator(); //get an iterator to the entries in the map
		while(entryIterator.hasNext()) { //while there are more entries
			final Map.Entry<K, V> entry = entryIterator.next(); //get the next entry
			if(!set.contains(entry.getKey())) { //if this entry is not represented in the set
				entryIterator.remove(); //remove this entry
			}
		}
	}

	/**
	 * Retrieves all map entries from the given map and returns them as a set of name value pairs.
	 * @param <K> The type of key.
	 * @param <V> The type of value.
	 * @param map The map from which to the retrieve the key values.
	 * @return A set of the key value pairs added.
	 */
	public static <K, V> Set<NameValuePair<K, V>> getKeyValues(final Map<K, V> map) {
		return getKeyValues(map, new HashSet<NameValuePair<K, V>>());
	}

	/**
	 * Retrieves all map entries from the given map and returns them as name value pairs in the given collection.
	 * @param <K> The type of key.
	 * @param <V> The type of value.
	 * @param <C> The type of collection.
	 * @param map The map from which to the retrieve the key values.
	 * @param collection The collection to which the key value pairs should be added.
	 * @return The given collection, with the key value pairs added.
	 */
	public static <K, V, C extends Collection<NameValuePair<K, V>>> C getKeyValues(final Map<K, V> map, final C collection) {
		for(final Map.Entry<K, V> entry : map.entrySet()) { //convert all the map entries to NameValues and add them to the collection
			collection.add(NameValuePair.fromMapEntry(entry));
		}
		return collection;
	}

	/**
	 * Retrieves all map entries from the given map and returns them as name value pairs in the given collection; the values are cloned.
	 * @param <K> The type of key.
	 * @param <V> The type of value.
	 * @param <C> The type of collection.
	 * @param map The map from which to the retrieve the key values.
	 * @param collection The collection to which the key value pairs should be added.
	 * @return The given collection, with the key value pairs added.
	 */
	public static <K, V extends CloneSupported, C extends Collection<NameValuePair<K, V>>> C getKeyValuesCloned(final Map<K, V> map, final C collection) {
		for(final Map.Entry<K, V> entry : map.entrySet()) { //convert all the map entries to NameValues and add them to the collection
			collection.add(new NameValuePair<K, V>(entry.getKey(), Objects.clone(entry.getValue()))); //clone the values
		}
		return collection;
	}

	/**
	 * Creates a read-only copy of the given map. If the map is already read-only, the map itself is returned.
	 * @param <K> The type of key contained in the map.
	 * @param <V> The type of value contained in the map.
	 * @param map The map which should be returned in read-only form.
	 * @return The immutable version of the map.
	 * @throws NullPointerException if the given map is <code>null</code>.
	 */
	public static <K, V> Map<K, V> toImmutableMap(final Map<K, V> map) { //TODO improve to return an ImmutableMap<K, V>
		if(map instanceof ImmutableMap) { //if the map is already immutable TODO fix for Java's immutable maps
			return map;
		}
		final int size = map.size(); //see how big the map is
		if(size == 1) { //if the map only contains one map entry
			final Map.Entry<K, V> entry = map.entrySet().iterator().next();
			return singletonMap(entry.getKey(), entry.getValue()); //return an immutable map containing the single key and value
		}
		return Collections.unmodifiableMap(new HashMap<K, V>(map)); //copy the map and wrap it in an unmodifiable map
	}

	/**
	 * Converts the given map to a properties object. If the map is already a properties object, it is returned. Otherwise, a new properties object is created and
	 * populated with the entries of the given map.
	 * @param map The map to convert to a properties object.
	 * @return A properties object, potentially the same instance, containing entries from the given map.
	 */
	public static Properties toProperties(@Nonnull final Map<?, ?> map) {
		if(map instanceof Properties) { //if the map is already a properties object
			return (Properties)map; //return the map as a properties object
		} else { //if the map is not a properties object
			final Properties properties = new Properties(); //create a new properties object
			properties.putAll(map); //put all the properties from the map
			return properties; //return the populated properties object
		}
	}

}
