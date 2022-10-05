/*
 * Copyright © 2011 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
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
 * Abstract implementation of an iterator that lazily primes its next value.
 * 
 * <p>
 * This version does not support {@link #remove()}.
 * </p>
 * 
 * <p>
 * This class is not thread safe.
 * </p>
 * 
 * @author Garret Wilson
 * 
 * @param <E> The type of element returned by the iterator.
 */
public abstract class AbstractPrimedIterator<E> implements Iterator<E> {

	/** Indicates whether the iterator has been primed. */
	private boolean primed = false;

	/** The next value, or <code>null</code> if there is no next value. */
	private E next = null;

	/**
	 * Ensures that the iterator has been primed.
	 * @see #primeNext()
	 */
	private void ensurePrimed() {
		if(!primed) { //if we aren't prime
			next = primeNext(); //prime the value
			primed = true; //we are now primed
		}
	}

	@Override
	public boolean hasNext() {
		ensurePrimed(); //make sure we are primed
		return next != null;
	}

	@Override
	public E next() {
		ensurePrimed(); //make sure we are primed
		if(next == null) { //if there is no next value
			throw new NoSuchElementException();
		}
		primed = false; //we're returning the next value, so we are no longer primed
		return next;
	}

	/**
	 * Returns the next available element for the iterator. The value returned by this method will be returned by the next call to {@link #next}. The
	 * implementation guarantees that this method will not be called again after <code>null</code> is returned.
	 * @return The next primed value for the iterator, or <code>null</code> if there are no further values.
	 */
	protected abstract E primeNext();

	@Override
	public void remove() {
		throw new UnsupportedOperationException();
	}

}
