package com.garretwilson.util;

import java.util.Set;

/**A thread-safe set that uses a pair of read and write locks to access its data.
@param <E> The type of elements in the set.
@author Garret Wilson
*/
public interface ReadWriteLockSet<E> extends ReadWriteLockCollection<E>, Set<E>
{
}
