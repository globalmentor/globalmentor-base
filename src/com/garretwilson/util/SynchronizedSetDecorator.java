package com.garretwilson.util;

import java.util.*;

/**A set that wraps an existing set, providing access through the <code>Set</code> interface.
All collection access is synchronized on the provided synchronization object.
@param <E> The type of element contained in the set.
@author Garret Wilson
*/
public class SynchronizedSetDecorator<E> extends SynchronizedCollectionDecorator<E> implements Set<E>
{

	/**The set this class decorates.*/
	protected final Set<E> set;

	/**Set constructor.
	@param set The set this set should decorate.
	@param mutex The mutual exclusion synchronization object.
	@exception NullPointerException if the provided set and/or mutex is <code>null</code>.
	*/
	public SynchronizedSetDecorator(final Set<E> set, final Object mutex)
	{
		super(set, mutex);	//construct the parent class
		this.set=set;	//save the list
	}
}
