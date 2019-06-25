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

import java.util.Map;

import static java.util.Objects.*;

/**
 * A map that also allows lookup of the map keys keyed to the values by decorating two maps.
 * @param <K> The type of map key.
 * @param <V> The type of map value.
 *          <p>
 *          The keys and values in this map have a one-to-one relationship. Associating multiple values with a key will likely result in errant functionality.
 *          </p>
 * @author Garret Wilson
 */
public class DecoratorReverseMap<K, V> extends MapDecorator<K, V> implements ReverseMap<K, V> { //TODO programmatically verify the one-to-one relationship

	//TODO most likely prohibit nulls

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

	@Override
	public K getKey(final V value) {
		return reverseMap.get(value); //return the key keyed to the given value in the key map
	}

	@Override
	public K removeValue(final V value) {
		final K oldKey = reverseMap.remove(value); //remove the value from the reverse map
		if(oldKey != null) { //if there was a key associated with the value
			super.remove(oldKey); //remove the old key from the map; call the superclass version so that we won't try to remove values from the reverse map again
		}
		return oldKey; //return the old key, if any
	}

}
