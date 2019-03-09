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
 * no meaning in this context. This version releases the object when iteration has occurred (i.e. when {@link #hasNext()} would return <code>false</code>).
 * @param <E> The type of elements in this iterator
 * @author Garret Wilson
 */
public class ObjectIterator<E> implements Iterator<E>, Iterable<E> {

	/** The single object being iterated. */
	protected E object;

	/** Whether we've not retrieved the object. */
	protected boolean hasNext = true;

	/**
	 * Object constructor.
	 * @param object The single object over which iteration should occur.
	 */
	public ObjectIterator(final E object) {
		this.object = object; //save the object
	}

	/** {@inheritDoc} */
	@Override
	public boolean hasNext() {
		return hasNext;
	}

	/** {@inheritDoc} */
	@Override
	public E next() {
		if(hasNext) { //if we haven't returned the object, yet
			final E next = object; //get the object
			hasNext = false; //indicate we've retrieved the object
			object = null; //release the object
			return next; //return our copy of the object
		} else { //if we've already returned the object
			throw new NoSuchElementException("Already returned object.");
		}
	}

	/** {@inheritDoc} This implementation does not support removal. */
	@Override
	public void remove() {
		throw new UnsupportedOperationException("This iterator does not support removing the object.");
	}

	/** {@inheritDoc} This implementation returns <code>this</code>. */
	@Override
	public Iterator<E> iterator() {
		return this;
	}
}
