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

import java.util.*;

/**A set that wraps an existing set, providing access through the {@link Set} interface.
All collection access is synchronized on the provided synchronization object.
@param <E> The type of element contained in the set.
@author Garret Wilson
*/
public class SynchronizedSetDecorator<E> extends SynchronizedCollectionDecorator<E> implements Set<E>
{

	/**The set this class decorates.*/
	protected final Set<E> set;

	/**Set constructor.
	The new instance of this class is used as a mutex.
	@param set The set this set should decorate.
	@throws NullPointerException if the provided set is <code>null</code>.
	*/
	public SynchronizedSetDecorator(final Set<E> set)
	{
		super(set);	//construct the parent class
		this.set=set;	//save the set
	}

	/**Set and mutext constructor.
	@param set The set this set should decorate.
	@param mutex The mutual exclusion synchronization object.
	@throws NullPointerException if the provided set and/or mutex is <code>null</code>.
	*/
	public SynchronizedSetDecorator(final Set<E> set, final Object mutex)
	{
		super(set, mutex);	//construct the parent class
		this.set=set;	//save the set
	}
}
