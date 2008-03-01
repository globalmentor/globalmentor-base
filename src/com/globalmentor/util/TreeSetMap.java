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

/**An decorator map that stores a {@link TreeSet} of values for each key, with special methods for retrieving single values.
@param <K> The type of map key.
@param <V> The type of map value.
@author Garret Wilson
*/
public class TreeSetMap<K, V> extends AbstractDecoratorCollectionMap<K, V, Set<V>>
{

	/**The comparator that will be used to order the values in the set, or <code>null</code> if natural ordering should be used.*/
	private final Comparator<? super V> comparator;

		/**@return The comparator that will be used to order the values in the set, or <code>null</code> if natural ordering should be used.*/
		public Comparator<? super V> getComparator() {return comparator;}

	/**Map constructor with a natural ordering of set values.
	@param map The map this map should decorate.
	@exception NullPointerException if the provided map is <code>null</code>.
	*/
	public TreeSetMap(final Map<K, Set<V>> map)
	{
		this(map, null);	//construct the map using natural ordering of set values
	}

	/**Map and comparator constructor that sorts set values in the order specified by the comparator.
	@param map The map this map should decorate.
	@param comparator The comparator that will be used to order the values in the set, or <code>null</code> if natural ordering should be used.
	@exception NullPointerException if the provided map is <code>null</code>.
	*/
	public TreeSetMap(final Map<K, Set<V>> map, final Comparator<? super V> comparator)
	{
		super(map);	//construct the parent class
		this.comparator=comparator;	//save the comparator
	}

	/**Creates a collection in which to store values.
	This version returns a {@link TreeSet}.
	*/
	public Set<V> createCollection()
	{
		return new TreeSet<V>(getComparator());	//create and return a new set using the specified ordering
	}
}