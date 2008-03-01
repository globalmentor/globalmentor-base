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
