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

import static com.globalmentor.util.Arrays.*;
import static com.globalmentor.util.Collections.*;

/**Various utilities to be used with iterators.
@author Garret Wilson
@see java.util.Iterator
*/
public class Iterators
{

	/**The singleton immutable empty iterator.*/
	public final static Iterator<?> EMPTY_ITERATOR=new EmptyIterator<Object>();

		/**@return The immutable empty iterator.*/
		@SuppressWarnings("unchecked")
		public static final <T> Iterator<T> emptyIterator() {return (Iterator<T>)EMPTY_ITERATOR;}	//return the singleton empty iterator cast to the correct type

	/**The singleton immutable empty iterable.*/
	public final static Iterable<?> EMPTY_ITERABLE=new EmptyIterable<Object>();

		/**@return The immutable empty iterablke.*/
		@SuppressWarnings("unchecked")
		public static final <T> Iterable<T> emptyIterable() {return (Iterable<T>)EMPTY_ITERABLE;}	//return the singleton empty iterable cast to the correct type

	/**Creates a copy of the iterator that contains the same data but will
		not reflect any modified values of the underlying collection.
	Creates a new collection, collects the values of the given iterator, then
		returns an iterator to the new collection.
	@param iterator The iterator to make a copy of.
	@return An iterator containing the same values as the given iterator but not
		reflecting the underlying values of the original collection.
	*/
	public static <E> ListIterator<E> createCopy(final Iterator<E> iterator)
	{
		final List<E> list=new ArrayList<E>();	//create a list in which to store the iterator contents
		addAll(list, iterator);	//add the contents of the iterator to the list
		return list.listIterator();	//return an iterator to our local list containing what the original iterator contained 		
	}

	/**Returns the next item from an iterator, or <code>null</code> if there is no
		next item. This differs from <code>Iterator.next()</code>, which throws an
		exception if there is no next item.
	@param iterator The iterator from which the next object should be retrieved.
	@see java.util.Iterator#next
	*/
	public static <E> E getNext(final Iterator<E> iterator)
	{
			//get the next element, or null if there is no next element
	  return iterator.hasNext() ? iterator.next() : null;
	}

	/**Returns a copy of the contents of the iterator in a new array in iterator traversal order.
	@param iterator The iterator the contents of which to return as an array.
	@param elementClass The class representing the type of elements returned by the iterable.
	@return A array containing the contents of the iterable.
	@exception NullPointerException if the given iterator and/or element class is <code>null</code>.
	*/
	public static <E> E[] toArray(final Iterator<E> iterator, final Class<E> elementClass)
	{
		final List<E> list=new ArrayList<E>();	//create a list in which to store the iterator contents
		addAll(list, iterator);	//add the contents of the iterator to the list
		return list.toArray(createArray(elementClass, list.size()));	//create an array and return the contents of the list in it
	}

	/**Returns a copy of the contents of the iterable in a new array in iterator traversal order.
	@param iterable The iterable the contents of which to return as an array.
	@param elementClass The class representing the type of elements returned by the iterable.
	@return A array containing the contents of the iterable.
	@exception NullPointerException if the given iterable and/or element class is <code>null</code>.
	*/
	public static <E> E[] toArray(final Iterable<E> iterable, final Class<E> elementClass)
	{
		return toArray(iterable.iterator(), elementClass);	//get an iterator to the iterable and return an array from it
	}

	/**An implementation of an iterator that contains no data.
	@author Garret Wilson
	*/
	private static class EmptyIterator<E> implements Iterator<E>
	{
		/**@return <code>false</code>, because the empty iterator never has elements.*/
		public boolean hasNext() {return false;}

		/**Returns the next element in the iteration.
		This implementation always throws an exception because the empty iterator
		never has next elements.
		@return The next element in the iteration.
		@exception NoSuchElementException Thrown because the empty iteration has no elements.
		*/
		public E next() {throw new NoSuchElementException("The empty iterator has no elements.");}

		/**Removes from the underlying collection the last element returned by the
			iterator (optional operation).
		This implementation does not support remove and always throws an exception.
		@exception UnsupportedOperationException Thrown because the
			<code>remove</code> operation is not supported by the empty iterator.
		*/
		public void remove() {throw new UnsupportedOperationException("The empty iterator does not support removal of elements.");}
	}

	/**An implementation of an iterable that returns an iterator that contains no data.
	Besides being lighter than an empty collection, this class is useful for returning an empty iterator for code that must be converted back to JDK 1.4 compliance
		because the java.util.Collection classes cannot be retrofitted to implement a replacement Iterable, while this class can.
	@param <E> The type of object that can be iterated.
	@author Garret Wilson
	*/
	private static class EmptyIterable<E> implements Iterable<E>
	{

		/**@return An iterator to elements. This implementation returns the empty iterator.*/
		public Iterator<E> iterator() {return emptyIterator();}
	}
}