package com.garretwilson.util;

import java.util.*;

import java.util.concurrent.locks.ReadWriteLock;

/**A thread-safe map that uses a pair of read and write locks to access its data.
@param <K> The type of key used in the map.
@param <V> The type of value stored in the map.
@author Garret Wilson
*/
public interface ReadWriteLockMap<K, V> extends Map<K, V>, ReadWriteLock
{
}
