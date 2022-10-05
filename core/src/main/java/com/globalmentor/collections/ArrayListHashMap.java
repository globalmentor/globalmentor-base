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
 * An implementation of a {@link HashMap} that stores an {@link ArrayList} of values for each key, with special methods for retrieving single values.
 * @param <K> The type of map key.
 * @param <V> The type of map value.
 * @author Garret Wilson
 */
public class ArrayListHashMap<K, V> extends ArrayListMap<K, V> {

	/** Default constructor that decorates a {@link HashMap}. */
	public ArrayListHashMap() {
		super(new HashMap<K, List<V>>()); //create a new hash map to decorate
	}
}
