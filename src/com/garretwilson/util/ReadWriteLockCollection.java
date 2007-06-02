package com.garretwilson.util;

import java.util.Collection;

import java.util.concurrent.locks.ReadWriteLock;

/**A thread-safe collection that uses a pair of read and write locks to access its data.
@param <E> The type of elements in the collection.
@author Garret Wilson
*/
public interface ReadWriteLockCollection<E> extends Collection<E>, ReadWriteLock
{
}
