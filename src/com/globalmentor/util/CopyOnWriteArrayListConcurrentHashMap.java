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
import java.util.concurrent.*;

/**An implementation of a {@link ConcurrentHashMap} that stores a {@link CopyOnWriteArrayList} of values for each key, with special methods for retrieving single values.
@author Garret Wilson
*/
public class CopyOnWriteArrayListConcurrentHashMap<K, V> extends AbstractDecoratorCollectionMap<K, V, List<V>> 
{

	/**Default constructor that decorates a {@link ConcurrentHashMap}.*/
	public CopyOnWriteArrayListConcurrentHashMap()
	{
		super(new ConcurrentHashMap<K, List<V>>());	//create a new concurrent hash map to decorate
	}

	/**Creates a collection in which to store values.
	This version returns an {@link ArrayList}.
	*/
	public List<V> createCollection()
	{
		return new ArrayList<V>();
	}
}
