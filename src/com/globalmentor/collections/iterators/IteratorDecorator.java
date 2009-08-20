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

package com.globalmentor.collections.iterators;

import java.util.*;

import static com.globalmentor.java.Objects.*;

/**An iterator that wraps an existing iterator, providing access through the {@link Iterator} interface.
@author Garret Wilson
*/
public class IteratorDecorator<E> implements Iterator<E>
{

	/**The iterator this class decorates.*/
	protected final Iterator<E> iterator;

	/**Iterator constructor.
	@param iterator The iterator this iterator should decorate.
	@exception NullPointerException if the given iterator is <code>null</code>.
	*/
	public IteratorDecorator(final Iterator<E> iterator)
	{
		this.iterator=checkInstance(iterator, "Iterator cannot be null");	//save the iterator
	}

	/**
	 * Returns <code>true</code> if the iteration has more elements. (In other
	 * words, returns <code>true</code> if {@link #next()} would return an element
	 * rather than throwing an exception.)
	 *
	 * @return <code>true</code> if the iterator has more elements.
	 */
	public boolean hasNext() {return iterator.hasNext();}

	/**
	 * Returns the next element in the iteration.
	 *
	 * @return the next element in the iteration.
	 * @exception NoSuchElementException iteration has no more elements.
	 */
	public E next() {return iterator.next();}

	/**
	 * 
	 * Removes from the underlying collection the last element returned by the
	 * iterator (optional operation).  This method can be called only once per
	 * call to {@link #next()}.  The behavior of an iterator is unspecified if
	 * the underlying collection is modified while the iteration is in
	 * progress in any way other than by calling this method.
	 *
	 * @exception UnsupportedOperationException if the {@link #remove()}
	 *		  operation is not supported by this Iterator.
     
	 * @exception IllegalStateException if the {@link #next()} method has not
	 *		  yet been called, or the {@link #remove()} method has already
	 *		  been called after the last call to the {@link #next()}
	 *		  method.
	 */
	public void remove() {iterator.remove();}

}
