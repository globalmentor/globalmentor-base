/*
 * Copyright © 1996-2019 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static java.util.Objects.*;
import static java.util.Spliterators.*;
import static java.util.stream.StreamSupport.*;

import java.util.*;
import java.util.function.Supplier;
import java.util.stream.*;

import javax.annotation.*;

import com.globalmentor.collections.Lists;
import com.globalmentor.java.Objects;

/**
 * Various utilities to be used with iterators.
 * @author Garret Wilson
 * @see Iterator
 */
public class Iterators {

	/**
	 * Returns an iterator concatenating the contents of two iterators.
	 * @apiNote The returned iterator can be used as an {@link Iterator} or an {@link Enumeration}.
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
	 * @apiNote The returned iterator can be used as an {@link Iterator} or an {@link Enumeration}.
	 * @param <E> The type of elements returned by the iterators.
	 * @param iterators The iterators to concatenate.
	 * @return An iterator that will return the contents of the given iterators in order.
	 * @throws NullPointerException if any of the given iterators is <code>null</code>.
	 */
	@SafeVarargs
	@SuppressWarnings("varargs")
	public static <E> JoinIterator<E> concat(@Nonnull final Iterator<E>... iterators) {
		return new JoinIterator<>(iterators);
	}

	/**
	 * Returns an iterator concatenating the contents of an iterator and an enumeration.
	 * @apiNote The returned iterator can be used as an {@link Iterator} or an {@link Enumeration}.
	 * @param <E> The type of elements returned by the iterator and the enumeration.
	 * @param iterator An iterator the contents of which will appear first in the iteration.
	 * @param enumeration An enumeration the contents of which will appear after that of the iterator.
	 * @return An iterator that will return the contents of the iterator and the enumeration in order.
	 */
	public static <E> JoinIterator<E> concat(@Nonnull final Iterator<E> iterator, @Nonnull final Enumeration<E> enumeration) {
		return concat(iterator, toIterator(enumeration));
	}

	/**
	 * Returns an {@link Optional} describing the first element of this iterator, or an empty {@code Optional} if the iterator is empty.
	 * @apiNote This method is equivalent to calling {@link Iterator#next()}, except an empty optional is returned rather than throwing an exception if there is
	 *          no next item.
	 * @param <E> The type of elements returned by the iterator.
	 * @param iterator The iterator from which the next object should be retrieved.
	 * @return An {@code Optional} describing the first element of this iterator, or an empty {@code Optional} if the iterator is empty.
	 * @see Iterator#next()
	 * @see Stream#findFirst()
	 */
	public static <E> Optional<E> findNext(@Nonnull final Iterator<E> iterator) {
		return iterator.hasNext() ? Optional.of(iterator.next()) : Optional.empty();
	}

	/**
	 * Returns an {@link Optional} describing the first and only element of this iterator, or an empty {@code Optional} if the iterator is empty.
	 * @implSpec This implementation delegates to {@link #findOnly(Iterator, Supplier)}.
	 * @param <E> The type of elements returned by the iterator.
	 * @param iterator The iterator from which the only object should be retrieved.
	 * @return An {@code Optional} describing the only element of this iterator, or an empty {@code Optional} if the iterator is empty.
	 * @throws IllegalArgumentException if the given stream has more than one element.
	 * @see Iterator#next()
	 */
	public static <E> Optional<E> findOnly(@Nonnull final Iterator<E> iterator) {
		return findOnly(iterator, () -> new IllegalArgumentException("Multiple elements encountered when at most one was expected."));
	}

	/**
	 * Returns an {@link Optional} describing the first and only element of this iterator, or an empty {@code Optional} if the iterator is empty.
	 * @param <E> The type of elements returned by the iterator.
	 * @param <X> The type of exception to be thrown if there are many elements.
	 * @param iterator The iterator from which the only object should be retrieved.
	 * @param manyElementsExceptionSupplier The strategy for creating an exception to throw if more than one element is present.
	 * @return An {@code Optional} describing the only element of this iterator, or an empty {@code Optional} if the iterator is empty.
	 * @throws RuntimeException if the given stream has more than one element.
	 * @see Iterator#next()
	 */
	public static <E, X extends RuntimeException> Optional<E> findOnly(@Nonnull final Iterator<E> iterator,
			@Nonnull final Supplier<X> manyElementsExceptionSupplier) {
		final Optional<E> only;
		if(iterator.hasNext()) {
			only = Optional.of(iterator.next());
			if(iterator.hasNext()) {
				throw manyElementsExceptionSupplier.get();
			}
		} else {
			only = Optional.empty();
		}
		return only;
	}

	/**
	 * Retrieves the one and only one element expected to be in the iterator.
	 * @param <E> The type of element in the iterator.
	 * @param iterator The iterator from which the only element will be retrieved.
	 * @return The one and only one element in the iterator.
	 * @throws NoSuchElementException if the iterator has no more elements
	 * @throws IllegalArgumentException if the given iterator has more than one element.
	 */
	public static <E> E getOnly(@Nonnull final Iterator<E> iterator) {
		return getOnly(iterator, () -> new IllegalArgumentException("Multiple elements encountered when at most one was expected."));
	}

	/**
	 * Retrieves the one and only one element expected to be in the iterator.
	 * @param <E> The type of element in the iterator.
	 * @param <X> The type of exception to be thrown if there are many elements.
	 * @param iterator The iterator from which only element will be retrieved.
	 * @param manyElementsExceptionSupplier The strategy for creating an exception to throw if more than one element is present.
	 * @return The one and only one element in the iterator.
	 * @throws NoSuchElementException if the iterator has no more elements
	 * @throws RuntimeException if the given stream has more than one element.
	 */
	public static <E, X extends RuntimeException> E getOnly(@Nonnull final Iterator<E> iterator, @Nonnull final Supplier<X> manyElementsExceptionSupplier) {
		requireNonNull(manyElementsExceptionSupplier);
		final E next = iterator.next();
		if(iterator.hasNext()) {
			throw manyElementsExceptionSupplier.get();
		}
		return next;
	}

	/**
	 * Reverses a given list iterator by returning a new iterator that iterates in the opposite direction, <em>starting at the present iteration location</em>.
	 * @apiNote This method does not initially change the present iteration location. If it is desired to start at the other end of the iterator, the iterator's
	 *          position must be changed <em>before calling this method</em>—for example by calling:
	 *          <code>while(listIterator.hasNext()) listIterator.next();</code>. Nevertheless what is normally desired is to provide a reversed order
	 *          {@link Iterable} of a {@link List}, and for this {@link Lists#reversing(List)} should be used, which can create and place the list iterator at the
	 *          end of the list in a more efficient manner.
	 * @param <E> The type of element in the iterator.
	 * @param listIterator The list iterator to be iterated in reverse order.
	 * @return An iterator view that iterates the given list iterator, but in reverse order.
	 * @see Lists#reversing(List)
	 */
	public static <E> Iterator<E> reverse(@Nonnull final ListIterator<E> listIterator) {
		return new ReverseIterator<>(listIterator);
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
	 * @apiNote Unlike most iterables, the returned iterable may only be iterated a single time.
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
	 * Returns a stream providing access to the contents of the given iterator. The input iterator must not be used further after calling this method.
	 * @apiNote If the characteristics of the iterator are known, it would be better to call {@link Spliterators#spliteratorUnknownSize(Iterator, int)},
	 *          indicating those characteristics.
	 * @implSpec The stream returned by this implementation is not parallel.
	 * @param <E> The type of elements returned by the iterator.
	 * @param iterator The iterator to be converted to a stream.
	 * @return A stream that returns the contents of the given iterator.
	 * @see com.globalmentor.collections.iterables.Iterables#toStream(Iterable)
	 * @see <a href=
	 *      "https://guava.dev/releases/snapshot-jre/api/docs/com/google/common/collect/Streams.html#stream(java.util.Iterator)"><code>com.google.common.collect.Streams.stream(Iterator)</code></a>
	 */
	public static <E> Stream<E> toStream(@Nonnull final Iterator<E> iterator) {
		return stream(spliteratorUnknownSize(iterator, 0), false);
	}

}
