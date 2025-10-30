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

import java.util.Map;
import java.util.function.*;

import static java.util.Objects.*;

/**
 * A map that also allows lookup of the map keys keyed to the values by decorating two maps.
 * <p>The keys and values in this map have a one-to-one relationship. Associating multiple values with a key will likely result in errant functionality.</p>
 * @apiNote This implementation does not support <code>null</code> keys or values.
 * @param <K> The type of map key.
 * @param <V> The type of map value.
 * @author Garret Wilson
 */
public class DecoratorReverseMap<K, V> extends MapDecorator<K, V> implements ReverseMap<K, V> { //TODO programmatically verify the one-to-one relationship

	/** The map containing reverse-lookup values. */
	private final Map<V, K> reverseMap;

	/**
	 * Constructs a reverse map by decorating two other maps.
	 * @param map The main map to be decorated.
	 * @param reverseMap The map to contain reverse lookup values.
	 * @throws NullPointerException if the given map and/or reverse map is <code>null</code>.
	 */
	public DecoratorReverseMap(final Map<K, V> map, final Map<V, K> reverseMap) {
		super(map); //create the parent class
		this.reverseMap = requireNonNull(reverseMap, "Reverse map cannot be null.");
	}

	/**
	 * {@inheritDoc}
	 * @see #containsValue(Object)
	 */
	@Override
	public K getKey(final V value) {
		requireNonNull(value, "Value cannot be null.");
		return reverseMap.get(value); //return the key keyed to the given value in the key map
	}

	@Override
	public K removeValue(final V value) {
		requireNonNull(value, "Value cannot be null.");
		final K oldKey = reverseMap.remove(value); //remove the value from the reverse map
		if(oldKey != null) { //if there was a key associated with the value
			super.remove(oldKey); //remove the old key from the map; call the superclass version so that we won't try to remove values from the reverse map again
		}
		return oldKey; //return the old key, if any
	}

	/**
	 * {@inheritDoc}
	 * @implNote This implementation uses an internal reverse map to provide faster lookups than the default linear-time lookup.
	 */
	@Override
	public boolean containsValue(final Object value) {
		return reverseMap.containsKey(value); //see if this value is stored in the key map
	}

	/**
	 * {@inheritDoc}
	 * @implNote This implementation maintains the one-to-one relationship by removing any existing mapping when a value is reassigned to a different key.
	 */
	@Override
	public V put(final K key, final V value) {
		requireNonNull(key, "Key cannot be null.");
		requireNonNull(value, "Value cannot be null.");
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
	}

	@Override
	public V remove(final Object key) {
		final V oldValue = super.remove(key); //remove the key
		if(oldValue != null) { //if there was a value associated with the key
			reverseMap.remove(oldValue); //remove the old value from the reverse map
		}
		return oldValue; //return the old value, if any
	}

	@Override
	public void clear() {
		super.clear(); //do the default clearing
		reverseMap.clear(); //clear the key map as well
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation ensures the reverse map is updated to maintain the one-to-one relationship between keys and values.
	 */
	@Override
	public V putIfAbsent(final K key, final V value) {
		requireNonNull(key, "Key cannot be null.");
		requireNonNull(value, "Value cannot be null.");
		final V existingValue = get(key); //check if there's already a value for this key
		if(existingValue != null) {
			return existingValue; //return the existing value without modification
		}
		//No existing value, so perform the put operation
		return put(key, value); //use our overridden put() to maintain consistency
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation ensures the reverse map is updated to maintain the one-to-one relationship between keys and values.
	 */
	@Override
	public boolean remove(final Object key, final Object value) {
		final V currentValue = get(key);
		if(currentValue == null || !currentValue.equals(value)) {
			return false; //key is not mapped to the specified value
		}
		remove(key); //use our overridden remove(Object) to maintain consistency
		return true;
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
		final V currentValue = get(key);
		if(currentValue == null || !currentValue.equals(oldValue)) {
			return false; //key is not mapped to the old value
		}
		put(key, newValue); //use our overridden put() to maintain consistency
		return true;
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation ensures the reverse map is updated to maintain the one-to-one relationship between keys and values.
	 */
	@Override
	public V replace(final K key, final V value) {
		requireNonNull(key, "Key cannot be null.");
		requireNonNull(value, "Value cannot be null.");
		if(!containsKey(key)) {
			return null; //key is not present
		}
		return put(key, value); //use our overridden put() to maintain consistency
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation ensures the reverse map is updated to maintain the one-to-one relationship between keys and values.
	 */
	@Override
	public V computeIfAbsent(final K key, final Function<? super K, ? extends V> mappingFunction) {
		requireNonNull(key, "Key cannot be null.");
		requireNonNull(mappingFunction);
		final V existingValue = get(key);
		if(existingValue != null) {
			return existingValue;
		}
		final V newValue = mappingFunction.apply(key);
		if(newValue != null) {
			put(key, newValue); //use our overridden put() to maintain consistency
		}
		return newValue;
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation ensures the reverse map is updated to maintain the one-to-one relationship between keys and values.
	 */
	@Override
	public V computeIfPresent(final K key, final BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
		requireNonNull(key, "Key cannot be null.");
		requireNonNull(remappingFunction);
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
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation ensures the reverse map is updated to maintain the one-to-one relationship between keys and values.
	 */
	@Override
	public V compute(final K key, final BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
		requireNonNull(key, "Key cannot be null.");
		requireNonNull(remappingFunction);
		final V oldValue = get(key);
		final V newValue = remappingFunction.apply(key, oldValue);
		if(newValue != null) {
			put(key, newValue); //use our overridden put() to maintain consistency
		} else if(oldValue != null) {
			remove(key); //remove the mapping if the new value is null and there was an old value
		}
		return newValue;
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
		final V oldValue = get(key);
		final V newValue = (oldValue == null) ? value : remappingFunction.apply(oldValue, value);
		if(newValue != null) {
			put(key, newValue); //use our overridden put() to maintain consistency
		} else {
			remove(key); //remove the mapping if the result is null
		}
		return newValue;
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation ensures the reverse map is updated to maintain the one-to-one relationship between keys and values. If the remapping function
	 *           produces the same value for multiple keys, the last key processed will retain the mapping and earlier keys will be removed from the map.
	 */
	@Override
	public void replaceAll(final BiFunction<? super K, ? super V, ? extends V> function) {
		requireNonNull(function);
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
	}

}
