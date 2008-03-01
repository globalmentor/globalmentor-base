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

import static com.globalmentor.java.Objects.*;
import static com.globalmentor.util.Collections.*;

/**An iterator that wraps an existing list iterator and provides its elements in reverse order.
The list list iterator will be placed at the end of the list. 
For added efficiency, the decorated list iterator should already be at the end of the list.
@author Garret Wilson
*/
public class ReverseIterator<E> implements Iterator<E>
{

	/**The list iterator this class decorates.*/
	protected final ListIterator<E> listIterator;

	/**Iterable constructor.
	A temporary collection will be created and filled with the contents of the given iterable unless the given iterable is a {@link List}.
	@param iterable The iterable this iterator should decorate.
	@exception NullPointerException if the given iterable is <code>null</code>.
	*/
	public ReverseIterator(final Iterable<E> iterable)
	{
		this(toEndListIterator(iterable));	//get a list iterator to the end of the iterable 
	}

	/**List iterator constructor.
	The list list iterator will be placed at the end of the list. 
	For added efficiency, the decorated list iterator should already be at the end of the list.
	@param listIterator The list iterator this iterator should decorate.
	@exception NullPointerException if the given iterator is <code>null</code>.
	*/
	public ReverseIterator(final ListIterator<E> listIterator)
	{
		this.listIterator=checkInstance(listIterator, "Iterator cannot be null");	//save the iterator
		while(listIterator.hasNext())	//while the list iterator isn't at the end of the lsit
		{
			listIterator.next();	//advance to the end of the list
		}
	}

	/**Returns a list iterator to the given iterable.
	The list iterator will be placed at the end of the elements for efficiency.
	If the given iterable is not a {@link List}, a temporary list will be created and filled with the contents of the given iterable.
	@param <T> The type of elements contained in the iterable.
	@param iterable The iterable to elements.
	@return A list iterator to the elements of the given iterable, pointing to the end of the list.
	 */
	protected static <T> ListIterator<T> toEndListIterator(final Iterable<T> iterable)
	{
		final List<T> list=toList(iterable);	//if the iterable isn't already a list, create a new list will the contents of the iterable
		return list.listIterator(list.size());	//return a list iterator at the end of the list
	}

	/**Returns <code>true</code> if the iteration has more elements.
	This implementation delegates to the decorated iterator's opposite method, {@link ListIterator#hasPrevious()}.
	@return <code>true</code> if the iterator has more elements, which for this implementation is in the reverse direction.
	 */
	public boolean hasNext() {return listIterator.hasPrevious();}

	/**Returns the next element in the iteration.
	This implementation delegates to the decorated iterator's opposite method, {@link ListIterator#previous()}.
	@return The next element in the iteration, which for this implementation is in the reverse direction.
	@exception NoSuchElementException iteration has no more elements.
	*/
	public E next() {return listIterator.previous();}

	/**Removes from the underlying collection the last element returned by the iterator (optional operation). 
	This implementation delegates to the decorated iterator's {@link ListIterator#remove()} method.
	@exception UnsupportedOperationException if the {@link #remove()} operation is not supported by this iterator.
	@exception IllegalStateException if the {@link #next()} method has not yet been called,
	or the {@link #remove()} method has already been called after the last call to the {@link #next()} method.
	*/
	public void remove() {listIterator.remove();}

}
