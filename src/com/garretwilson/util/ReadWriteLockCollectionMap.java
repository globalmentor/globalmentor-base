package com.garretwilson.util;

import java.util.Collection;

/**A thread-safe map that stores a collection of values for each key, with special methods for retrieving single values.
@param <K> The type of key used in the map.
@param <V> The type of value stored in each collection in the map.
@param <C> The type of collection in which to store values in the map.
@author Garret Wilson
*/
public interface ReadWriteLockCollectionMap<K, V, C extends Collection<V>> extends CollectionMap<K, V, C>, ReadWriteLockMap<K, C> 
{
}
