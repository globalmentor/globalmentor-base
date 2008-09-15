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

/**An implementation of a {@link HashMap} that stores an {@link HashSet} of values for each key, with special methods for retrieving single values.
@author Garret Wilson
*/
public class HashSetHashMap<K, V> extends AbstractDecoratorCollectionMap<K, V, Set<V>> 
{

	/**Default constructor that decorates a {@link HashMap}.*/
	public HashSetHashMap()
	{
		super(new HashMap<K, Set<V>>());	//create a new hash map to decorate
	}

	/**Creates a collection in which to store values.
	This version returns an {@link HashSet}.
	*/
	public Set<V> createCollection()
	{
		return new HashSet<V>();
	}
}