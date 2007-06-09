package com.garretwilson.util;

/**A thread-safe reverse map that uses a pair of read and write locks to access its data.
@param <K> The type of key used in the map.
@param <V> The type of value stored in the map.
@author Garret Wilson
*/
public interface ReadWriteLockReverseMap<K, V> extends ReadWriteLockMap<K, V>, ReverseMap<K, V>
{
}
