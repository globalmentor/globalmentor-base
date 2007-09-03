package com.garretwilson.util;

import java.util.*;
import java.util.concurrent.locks.*;

/**A thread-safe collection decorator that allows many readers but only one writer to access a set at a time.
For operations that iterate over live collection data, a read or write lock should be acquired before the call to acquire the data and held until the data is consumed.
@param <E> The type of elements in the set.
@author Garret Wilson
*/
public class DecoratorReadWriteLockSet<E> extends DecoratorReadWriteLockCollection<E> implements ReadWriteLockSet<E>
{
	/**@return The set this class decorates.*/
	protected Set<E> getCollection() {return (Set<E>)super.getCollection();}

	/**Set constructor with a default reentrant read/write lock.
	@param set The set this set should decorate.
	@exception NullPointerException if the provided set is <code>null</code>.
	*/
	public DecoratorReadWriteLockSet(final Set<E> set)
	{
		this(set, new ReentrantReadWriteLock());	//create the set with a default lock
	}

	/**Set and read/write lock constructor.
	@param set The set this set should decorate.
	@param lock The lock for controlling access to the set.
	@exception NullPointerException if the provided set and/or lock is <code>null</code>.
	*/
	public DecoratorReadWriteLockSet(final Set<E> set, final ReadWriteLock lock)
	{
		super(set, lock);	//construct the parent class
	}
}
