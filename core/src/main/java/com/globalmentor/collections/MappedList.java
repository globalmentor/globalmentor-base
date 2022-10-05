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

/**
 * A list that allows quick lookup of its elements. This interface does not implement {@link Map}, because the {@link Map} interface has
 * <code>remove(Object)</code> semantics that conflict with those of the corresponding <code>List</code> method.
 * @author Garret Wilson
 * @see java.util.List
 * @see java.util.Map
 */
public interface MappedList<K, E> extends List<E> {

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
	public boolean containsKey(Object key);

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
	public E get(Object key);

	/**
	 * Removes the value from the list mapped to the given key value.
	 * @param key The key whose mapping is to be removed from the map and whose corresponding value is to be removed from the list.
	 * @return previous value associated with specified key, or <code>null</code> if there was no mapping for key.
	 * @throws ClassCastException if the key is of an inappropriate type for this mapped list (optional).
	 * @throws NullPointerException if the key is <code>null</code> and this mapped list does not permit <code>null</code> keys (optional).
	 * @throws UnsupportedOperationException if the <code>remove</code> method is not supported by this mapped list.
	 */
	public E removeKey(Object key);

}
