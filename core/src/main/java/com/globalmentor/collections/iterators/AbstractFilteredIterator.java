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

import static java.util.Objects.*;

/**
 * Abstract implementation of an iterator that filters an existing iterator.
 * 
 * <p>
 * This version does not support {@link #remove()}.
 * </p>
 * 
 * <p>
 * This version releases the decorated iterator when iteration is finished.
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
public abstract class AbstractFilteredIterator<E> extends AbstractPrimedIterator<E> {

	/** The filtered iterator. */
	private Iterator<E> iterator;

	/**
	 * Decorated iterator constructor.
	 * @param iterator The decorated iterator to be filtered.
	 * @throws NullPointerException if the given iterator is <code>null</code>.
	 */
	public AbstractFilteredIterator(final Iterator<E> iterator) {
		this.iterator = requireNonNull(iterator);
	}

	/**
	 * {@inheritDoc}
	 * @see #isPass(Object)
	 */
	@Override
	protected final E primeNext() {
		if(iterator != null) { //if we still have a decorated iterator
			while(iterator.hasNext()) { //find the next included element
				final E element = iterator.next(); //get the next element from the underlying iterator
				if(isPass(element)) { //if this element is included
					return element; //return it as the primed next element
				}
			}
			iterator = null; //if we are out of elements, release the iterator as a good citizen---we won't use it further in this class
		}
		return null; //if there are no more elements, there can be no further filtered elements
	}

	/**
	 * Indicates whether the given element is included when filtering.
	 * @param element The element in the filtered iterator.
	 * @return <code>true</code> if the element should be included, or <code>null</code> if it should be filtered out.
	 */
	protected abstract boolean isPass(final E element);

}
