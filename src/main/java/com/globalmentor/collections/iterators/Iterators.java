/*
 * Copyright © 1996-2019 GlobalMentor, Inc. <http://www.globalmentor.com/>
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
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import javax.annotation.*;

import com.globalmentor.java.Objects;

import static com.globalmentor.collections.Collections.*;
import static com.globalmentor.java.Arrays.*;

/**
 * Various utilities to be used with iterators.
 * @author Garret Wilson
 * @see Iterator
 */
public class Iterators {

	/** The singleton immutable empty iterator. */
	public static final Iterator<?> EMPTY_ITERATOR = new EmptyIterator<Object>();

	/**
	 * @param <T> The type of the iterator.
	 * @return The immutable empty iterator.
	 */
	@SuppressWarnings("unchecked")
	public static final <T> Iterator<T> emptyIterator() {
		return (Iterator<T>)EMPTY_ITERATOR; //return the singleton empty iterator cast to the correct type
	}

	/** The singleton immutable empty iterable. */
	public static final Iterable<?> EMPTY_ITERABLE = new EmptyIterable<Object>();

	/**
	 * @param <T> The generic type of empty iterable to return.
	 * @return The immutable empty iterable.
	 */
	@SuppressWarnings("unchecked")
	public static final <T> Iterable<T> emptyIterable() {
		return (Iterable<T>)EMPTY_ITERABLE; //return the singleton empty iterable cast to the correct type
	}

	/**
	 * Returns an iterator concatenating the contents of two iterators.
	 * <p>
	 * <strong>Tip:</strong> The returned iterator can be used as an {@link Iterator} or an {@link Enumeration}.
	 * </p>
	 * @param <E> The type of elements returned by the iterators.
	 * @param iterator1 The iterator the contents of which will appear first in the iteration.
	 * @param iterator2 The iterator the contents of which will appear second in the iteration.
	 * @return An iterator that will return the contents of the two iterators in order.
	 */
	public static <E> JoinIterator<E> concat(@Nonnull final Iterator<E> iterator1, @Nonnull final Iterator<E> iterator2) {
		return new JoinIterator<>(Stream.of(iterator1, iterator2));
	}

	/**
	 * Returns an iterator concatenating the contents of multiple iterators.
	 * <p>
	 * <strong>Tip:</strong> The returned iterator can be used as an {@link Iterator} or an {@link Enumeration}.
	 * </p>
	 * @param <E> The type of elements returned by the iterators.
	 * @param iterators The iterators to concatenate.
	 * @return An iterator that will return the contents of the given iterators in order.
	 * @throws NullPointerException if any of the given iterators is <code>null</code>.
	 */
	@SafeVarargs
	public static <E> JoinIterator<E> concat(@Nonnull final Iterator<E>... iterators) {
		return new JoinIterator<>(iterators);
	}

	/**
	 * Returns an iterator concatenating the contents of an iterator and an enumeration.
	 * <p>
	 * <strong>Tip:</strong> The returned iterator can be used as an {@link Iterator} or an {@link Enumeration}.
	 * </p>
	 * @param <E> The type of elements returned by the iterator and the enumeration.
	 * @param iterator An iterator the contents of which will appear first in the iteration.
	 * @param enumeration An enumeration the contents of which will appear after that of the iterator.
	 * @return An iterator that will return the contents of the iterator and the enumeration in order.
	 */
	public static <E> JoinIterator<E> concat(@Nonnull final Iterator<E> iterator, @Nonnull final Enumeration<E> enumeration) {
		return concat(iterator, toIterator(enumeration));
	}

	/**
	 * Creates a copy of the iterator that contains the same data but will not reflect any modified values of the underlying collection. Creates a new collection,
	 * collects the values of the given iterator, then returns an iterator to the new collection.
	 * @param <E> The type of items that the iterator is set up to iterate.
	 * @param iterator The iterator to make a copy of.
	 * @return An iterator containing the same values as the given iterator but not reflecting the underlying values of the original collection.
	 */
	public static <E> ListIterator<E> createCopy(final Iterator<E> iterator) {
		final List<E> list = new ArrayList<E>(); //create a list in which to store the iterator contents
		addAll(list, iterator); //add the contents of the iterator to the list
		return list.listIterator(); //return an iterator to our local list containing what the original iterator contained 		
	}

	/**
	 * Returns the next item from an iterator, or <code>null</code> if there is no next item. This differs from <code>Iterator.next()</code>, which throws an
	 * exception if there is no next item.
	 * @param <E> The type of items that the iterator is set up to iterate.
	 * @param iterator The iterator from which the next object should be retrieved.
	 * @return The next item of the iterator.
	 * @see Iterator#next()
	 * @deprecated Migrate to {@link #findNext(Iterator)}.
	 */
	@Deprecated
	public static <E> E getNext(final Iterator<E> iterator) {
		//get the next element, or null if there is no next element
		return iterator.hasNext() ? iterator.next() : null;
	}

	/**
	 * Returns an {@link Optional} describing the first element of this iterator, or an empty {@code Optional} if the iterator is empty.
	 * @apiNote This method is equivalent to calling {@link Iterator#next()}, except an empty optional is returned rather than throwing an exception if there is
	 *          no next item.
	 * @param <E> The type of elements returned by the iterator.
	 * @param iterator The iterator from which the next object should be retrieved.
	 * @return The next item of the iterator.
	 * @see Iterator#next()
	 * @see Stream#findFirst()
	 */
	public static <E> Optional<E> findNext(@Nonnull final Iterator<E> iterator) {
		return iterator.hasNext() ? Optional.of(iterator.next()) : Optional.empty();
	}

	/**
	 * Returns a copy of the contents of the iterator in a new array in iterator traversal order.
	 * @param <E> The type of the iterator.
	 * @param iterator The iterator the contents of which to return as an array.
	 * @param elementClass The class representing the type of elements returned by the iterable.
	 * @return A array containing the contents of the iterable.
	 * @throws NullPointerException if the given iterator and/or element class is <code>null</code>.
	 */
	public static <E> E[] toArray(final Iterator<E> iterator, final Class<E> elementClass) {
		final List<E> list = new ArrayList<E>(); //create a list in which to store the iterator contents
		addAll(list, iterator); //add the contents of the iterator to the list
		return list.toArray(createArray(elementClass, list.size())); //create an array and return the contents of the list in it
	}

	/**
	 * Returns a copy of the contents of the iterable in a new array in iterator traversal order.
	 * @param <E> The type of the iterator.
	 * @param iterable The iterable the contents of which to return as an array.
	 * @param elementClass The class representing the type of elements returned by the iterable.
	 * @return A array containing the contents of the iterable.
	 * @throws NullPointerException if the given iterable and/or element class is <code>null</code>.
	 */
	public static <E> E[] toArray(final Iterable<E> iterable, final Class<E> elementClass) {
		return toArray(iterable.iterator(), elementClass); //get an iterator to the iterable and return an array from it
	}

	/**
	 * Converts an iterator to an enumeration, if it isn't already.
	 * @param <E> the type of elements returned by the iterator.
	 * @param iterator The iterator to be converted to an enumeration.
	 * @return An enumeration delegating to the given iterator, or the iterator itself it is also an {@link Enumeration}.
	 */
	@SuppressWarnings("unchecked") //if it's an Iterator of type <E>, we assume the Enumeration would be as well
	public static <E> Enumeration<E> toEnumeration(@Nonnull final Iterator<E> iterator) {
		return Objects.asInstance(iterator, Enumeration.class).orElseGet(() -> new IteratorDecorator<>(iterator));
	}

	/**
	 * Returns a single-use iterable that returns the given iterator.
	 * <p>
	 * Unlike most iterables, the returned iterable may only be iterated a single time.
	 * </p>
	 * @param <E> the type of elements returned by the iterator.
	 * @param iterator The iterator to be converted to an iterable.
	 * @return A single-user iterable that returns the given iterator.
	 */
	public static <E> Iterable<E> toIterable(@Nonnull final Iterator<E> iterator) {
		return () -> iterator;
	}

	/**
	 * Converts an enumeration to an iteration, if it isn't already.
	 * @param <E> the type of elements returned by the enumeration.
	 * @param enumeration The enumeration to be converted to an iterator.
	 * @return An iterator delegating to the given enumeration, or the enumeration itself it is also an {@link Iterator}.
	 */
	@SuppressWarnings("unchecked") //if it's an Iterator of type <E>, we assume the Enumeration would be as well
	public static <E> Iterator<E> toIterator(@Nonnull final Enumeration<E> enumeration) {
		return Objects.asInstance(enumeration, Iterator.class).orElseGet(() -> new EnumerationDecorator<>(enumeration));
	}

	/**
	 * Returns a stream providing access to the contents of the given iterator.
	 * @param <E> The type of elements returned by the iterator.
	 * @param iterator The iterator to be converted to a stream.
	 * @return A stream that returns the contents of the given iterator.
	 */
	public static <E> Stream<E> toStream(@Nonnull final Iterator<E> iterator) {
		return StreamSupport.stream(toIterable(iterator).spliterator(), false);
	}

	/**
	 * An implementation of an iterator that contains no data.
	 * @author Garret Wilson
	 */
	private static class EmptyIterator<E> implements Iterator<E> {

		/** @return <code>false</code>, because the empty iterator never has elements. */
		public boolean hasNext() {
			return false;
		}

		/**
		 * Returns the next element in the iteration. This implementation always throws an exception because the empty iterator never has next elements.
		 * @return The next element in the iteration.
		 * @throws NoSuchElementException Thrown because the empty iteration has no elements.
		 */
		public E next() {
			throw new NoSuchElementException("The empty iterator has no elements.");
		}

		/**
		 * Removes from the underlying collection the last element returned by the iterator (optional operation). This implementation does not support remove and
		 * always throws an exception.
		 * @throws UnsupportedOperationException Thrown because the <code>remove</code> operation is not supported by the empty iterator.
		 */
		public void remove() {
			throw new UnsupportedOperationException("The empty iterator does not support removal of elements.");
		}
	}

	/**
	 * An implementation of an iterable that returns an iterator that contains no data. Besides being lighter than an empty collection, this class is useful for
	 * returning an empty iterator for code that must be converted back to JDK 1.4 compliance because the java.util.Collection classes cannot be retrofitted to
	 * implement a replacement Iterable, while this class can.
	 * @param <E> The type of object that can be iterated.
	 * @author Garret Wilson
	 */
	private static class EmptyIterable<E> implements Iterable<E> {

		/** @return An iterator to elements. This implementation returns the empty iterator. */
		public Iterator<E> iterator() {
			return emptyIterator();
		}
	}

}
