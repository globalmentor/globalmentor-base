/*
 * Copyright © 1996-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.collections.iterators;

import java.util.*;

/**
 * An iterable and iterator to a single object. This implementation allows <code>null</code> values. This implementation does not allow removal, as removing has
 * no meaning in this context.
 * @param <E> The type of elements in this iterator
 * @author Garret Wilson
 */
public class ObjectListIterator<E> extends ObjectIterator<E> implements ListIterator<E>
{
	/** The single object being iterated. */
	private E object;

	/** Whether we've not retrieved the object. */
	protected boolean hasNext = true;

	/**
	 * Object constructor.
	 * @param object The single object over which iteration should occur.
	 */
	public ObjectListIterator(final E object)
	{
		super(object);
	}

	/** @return <code>true</code> if the single object has not yet been retrieved. */
	public boolean hasNext()
	{
		return hasNext;
	}

	/** {@inheritDoc} */
	@Override
	public E next()
	{
		if(hasNext) //if we haven't returned the object, yet
		{
			hasNext = false;
			return object; //return our copy of the object, but don't release it 
		}
		else
		//if we've already returned the object
		{
			throw new NoSuchElementException();
		}
	}

	/** {@inheritDoc} */
	@Override
	public boolean hasPrevious()
	{
		return !hasNext;
	}

	/** {@inheritDoc} */
	@Override
	public E previous()
	{
		if(!hasNext) //if we've already returned the object
		{
			hasNext = true;
			return object; //return our copy of the object, but don't release it 
		}
		else
		{
			throw new NoSuchElementException();
		}
	}

	/** {@inheritDoc} */
	@Override
	public int nextIndex()
	{
		return hasNext ? 0 : 1;
	}

	/** {@inheritDoc} */
	@Override
	public int previousIndex()
	{
		return hasNext ? -1 : 0;
	}

	/** {@inheritDoc} This implementation does not support setting the element. */
	@Override
	public void set(E e)
	{
		throw new UnsupportedOperationException("This iterator does not support setting the element.");
	}

	/** {@inheritDoc} This implementation does not support adding an element. */
	@Override
	public void add(final E e)
	{
		throw new UnsupportedOperationException("This iterator does not support adding an element.");
	}
}
