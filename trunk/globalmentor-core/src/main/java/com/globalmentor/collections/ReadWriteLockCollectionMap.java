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

import java.util.Collection;

/**
 * A thread-safe map that stores a collection of values for each key, with special methods for retrieving single values.
 * @param <K> The type of key used in the map.
 * @param <V> The type of value stored in each collection in the map.
 * @param <C> The type of collection in which to store values in the map.
 * @author Garret Wilson
 */
public interface ReadWriteLockCollectionMap<K, V, C extends Collection<V>> extends CollectionMap<K, V, C>, ReadWriteLockMap<K, C> {
}
