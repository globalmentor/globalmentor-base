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

package com.globalmentor.util;

import java.util.*;

import static com.globalmentor.collections.iterators.Iterators.*;
import static java.util.Objects.*;

import com.globalmentor.collections.iterators.*;

/**
 * A resource bundle backed by a hash map.
 * @author Garret Wilson
 * @see PropertyResourceBundle
 */
public class HashMapResourceBundle extends ResourceBundle {

	/** The map containing the resource mappings. */
	private final Map<String, Object> map;

	/** Default constructor with no parent using a hash map. */
	public HashMapResourceBundle() {
		this((ResourceBundle)null); //construct the class without a parent 
	}

	/**
	 * Parent constructor using a hash map.
	 * @param parent The parent resource bundle, or <code>null</code> if there should be no parent for resolving resources.
	 */
	public HashMapResourceBundle(final ResourceBundle parent) {
		map = new HashMap<String, Object>(); //create a default hash map
		setParent(parent); //set the parent to that given
	}

	/**
	 * Map constructor with no parent. Keys will be converted to strings.
	 * @param map The map containing the resource mappings which will be copied to this map.
	 * @throws NullPointerException if the given map is <code>null</code>.
	 */
	public HashMapResourceBundle(final Map<?, ?> map) {
		this(map, null); //construct the class with no parent
	}

	/**
	 * Map and parent constructor. All values will be copied to the map. Keys will be converted to strings.
	 * @param map The map containing the resource mappings which will be copied to this map.
	 * @param parent The parent resource bundle, or <code>null</code> if there should be no parent for resolving resources.
	 * @throws NullPointerException if the given map is <code>null</code>.
	 */
	public HashMapResourceBundle(final Map<?, ?> map, final ResourceBundle parent) {
		this(parent); //do the default construction
		for(final Map.Entry<?, ?> entry : map.entrySet()) { //for each entry in the map
			final Object key = entry.getKey(); //get this entry's key
			if(key != null) { //if this entry has a key
				this.map.put(key.toString(), entry.getValue()); //save this key and value in the map
			}
		}
	}

	/**
	 * Gets an object for the given key from this resource bundle.
	 * @param key The key for the desired object.
	 * @throws NullPointerException if the given key is <code>null</code>.
	 * @return The object for the given key, or <code>null</code>.
	 */
	protected Object handleGetObject(final String key) {
		return map.get(requireNonNull(key, "Resource key cannot be null.")); //look up the object from the map
	}

	/** @return An enumeration of the resource keys. */
	public Enumeration<String> getKeys() {
		final Iterator<String> keyIterator = map.keySet().iterator(); //get an iterator to our keys
		final ResourceBundle parent = this.parent; //get the parent resource bundle, if there is one
		if(parent == null) { //if there is no parent
			return toEnumeration(keyIterator); //simply convert our keys to an enumeration
		}
		return Iterators.concat(keyIterator, parent.getKeys());
	}
}
