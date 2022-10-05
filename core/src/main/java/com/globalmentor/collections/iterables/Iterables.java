/*
 * Copyright © 2019 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.collections.iterables;

import java.util.*;
import java.util.function.Supplier;
import java.util.stream.*;

import javax.annotation.*;

import com.globalmentor.collections.iterators.Iterators;

/**
 * Various utilities to be used with iterables.
 * @author Garret Wilson
 * @see Iterable
 */
public class Iterables {

	/**
	 * Returns an {@link Optional} describing the first element of this iterable, or an empty {@code Optional} if the iterable is empty.
	 * @implSpec This implementation efficiently short-circuits and avoids creating an iterator if the iterable is an instance of a {@link Collection}, which has
	 *           a known size. Otherwise this implementation delegates to {@link Iterators#findNext(Iterator)}.
	 * @param <T> The type of elements returned by the iterator.
	 * @param iterable The iterable from which the first object should be retrieved.
	 * @return An {@code Optional} describing the first element of this iterable, or an empty {@code Optional} if the iterable is empty.
	 * @see <a href="https://stackoverflow.com/q/13692700/421049">Good way to get *any* value from a Java Set?</a>
	 * @see Iterator#next()
	 * @see Stream#findFirst()
	 * @see Iterators#findNext(Iterator)
	 */
	public static <T> Optional<T> findFirst(@Nonnull final Iterable<T> iterable) {
		if(iterable instanceof Collection) { //short-circuit for empty collections
			final Collection<T> collection = (Collection<T>)iterable;
			if(collection.isEmpty()) {
				return Optional.empty();
			}
		}
		return Iterators.findNext(iterable.iterator());
	}

	/**
	 * Returns an {@link Optional} describing the first and only element of this iterable, or an empty {@code Optional} if the iterable is empty.
	 * @implSpec This implementation efficiently short-circuits and avoids creating an iterator if the iterable is an instance of a {@link Collection}, which has
	 *           a known size. Otherwise this implementation delegates to {@link Iterators#findOnly(Iterator)}.
	 * @param <T> The type of elements returned by the iterable.
	 * @param iterable The iterable from which the only object should be retrieved.
	 * @return An {@code Optional} describing the only element of this iterable, or an empty {@code Optional} if the iterable is empty.
	 * @throws IllegalArgumentException if the given stream has more than one element.
	 * @see Iterator#next()
	 */
	public static <T> Optional<T> findOnly(@Nonnull final Iterable<T> iterable) {
		if(iterable instanceof Collection) { //short-circuit for empty collections
			final Collection<T> collection = (Collection<T>)iterable;
			if(collection.isEmpty()) {
				return Optional.empty();
			}
		}
		return Iterators.findOnly(iterable.iterator());
	}

	/**
	 * Returns an {@link Optional} describing the first and only element of this iterable, or an empty {@code Optional} if the iterable is empty.
	 * @implSpec This implementation efficiently short-circuits and avoids creating an iterator if the iterable is an instance of a {@link Collection}, which has
	 *           a known size. Otherwise this implementation delegates to {@link Iterators#findOnly(Iterator, Supplier)}.
	 * @param <T> The type of elements returned by the iterable.
	 * @param <X> The type of exception to be thrown if there are many elements.
	 * @param iterable The iterable from which the only object should be retrieved.
	 * @param manyElementsExceptionSupplier The strategy for creating an exception to throw if more than one element is present.
	 * @return An {@code Optional} describing the only element of this iterable, or an empty {@code Optional} if the iterable is empty.
	 * @throws RuntimeException if the given stream has more than one element.
	 * @see Iterator#next()
	 */
	public static <T, X extends RuntimeException> Optional<T> findOnly(@Nonnull final Iterable<T> iterable,
			@Nonnull final Supplier<X> manyElementsExceptionSupplier) {
		if(iterable instanceof Collection) { //short-circuit for empty collections
			final Collection<T> collection = (Collection<T>)iterable;
			if(collection.isEmpty()) {
				return Optional.empty();
			}
		}
		return Iterators.findOnly(iterable.iterator(), manyElementsExceptionSupplier);
	}

	/**
	 * Retrieves the one and only one element expected to be in the iterable.
	 * @implSpec This implementation delegates to {@link Iterators#getOnly(Iterator)}.
	 * @param <E> The type of element in the iterable.
	 * @param iterable The iterable from which the element will be retrieved.
	 * @return The one and only one element in the iterable.
	 * @throws NoSuchElementException if the iterable has no more elements
	 * @throws IllegalArgumentException if the given iterable has more than one element.
	 */
	public static <E> E getOnly(@Nonnull final Iterable<E> iterable) {
		return Iterators.getOnly(iterable.iterator());
	}

	/**
	 * Retrieves the one and only one element expected to be in the iterable.
	 * @implSpec This implementation delegates to {@link Iterators#getOnly(Iterator, Supplier)}.
	 * @param <E> The type of element in the iterable.
	 * @param <X> The type of exception to be thrown if there are many elements.
	 * @param iterable The iterable from which the element will be retrieved.
	 * @param manyElementsExceptionSupplier The strategy for creating an exception to throw if more than one element is present.
	 * @return The one and only one element in the iterable.
	 * @throws NoSuchElementException if the iterable has no more elements
	 * @throws RuntimeException if the given iterable has more than one element.
	 */
	public static <E, X extends RuntimeException> E getOnly(@Nonnull final Iterable<E> iterable, @Nonnull final Supplier<X> manyElementsExceptionSupplier) {
		return Iterators.getOnly(iterable.iterator(), manyElementsExceptionSupplier);
	}

}
