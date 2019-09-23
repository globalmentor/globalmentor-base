/*
 * Copyright © 2019 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.util.stream;

import static java.util.stream.Collectors.*;

import java.util.*;
import java.util.function.*;
import java.util.stream.*;

import javax.annotation.*;

/**
 * Utilities for working with {@link Stream}s.
 * @author Garret Wilson
 */
public class Streams {

	/**
	 * Reduction operator to require a stream to contain at most one element.
	 * @implSpec This implementation delegates to {@link #toFindOnly(Supplier)},
	 * @apiNote The method performs functionality similar to the terminal operation {@link Stream#findAny()} except that an exception is thrown if more than one
	 *          element is present.
	 * @apiNote Code modified from <a href="https://blog.codefx.org/java/stream-findfirst-findany-reduce/">Beware Of findFirst() And findAny()</a>.
	 * @param <T> The type of element in the stream.
	 * @return An {@code Optional} describing the only element of this stream, or an empty {@code Optional} if the stream is empty.
	 * @throws IllegalArgumentException if the given stream has more than one element.
	 * @see Stream#reduce(BinaryOperator)
	 * @see #toOnly()
	 * @see <a href="https://blog.codefx.org/java/stream-findfirst-findany-reduce/">Beware Of findFirst() And findAny()</a>
	 */
	public static <T> BinaryOperator<T> toFindOnly() {
		return toFindOnly(() -> new IllegalArgumentException("Multiple elements encountered when at most one was expected."));
	}

	/**
	 * Reduction operator to require a stream to contain at most one element.
	 * @apiNote The method performs functionality similar to the terminal operation {@link Stream#findAny()} except that an exception is thrown if more than one
	 *          element is present.
	 * @apiNote Code modified from <a href="https://blog.codefx.org/java/stream-findfirst-findany-reduce/">Beware Of findFirst() And findAny()</a>.
	 * @param <T> The type of element in the stream.
	 * @param <X> The type of exception to be thrown if there are many elements.
	 * @param manyElementsExceptionSupplier The strategy for creating an exception to throw if more than one element is present.
	 * @return An {@code Optional} describing the only element of this stream, or an empty {@code Optional} if the stream is empty.
	 * @throws RuntimeException if the given stream has more than one element.
	 * @see Stream#reduce(BinaryOperator)
	 * @see #toOnly(Supplier)
	 * @see <a href="https://blog.codefx.org/java/stream-findfirst-findany-reduce/">Beware Of findFirst() And findAny()</a>
	 */
	public static <T, X extends RuntimeException> BinaryOperator<T> toFindOnly(@Nonnull final Supplier<X> manyElementsExceptionSupplier) {
		return (element, otherElement) -> {
			throw manyElementsExceptionSupplier.get();
		};
	}

	/**
	 * Collector that returns the one and only one element expected to be in the stream.
	 * @implSpec This implementation delegates to {@link #toOnly(Supplier)}.
	 * @implNote Code modified from <a href="https://stackoverflow.com/users/2057294/skiwi">skiwi</a> <a href="https://stackoverflow.com/a/22695031/421049">on
	 *           Stack Overflow</a>.
	 * @implNote The current implementation extends an existing list collector. It would probably be possible to create a more efficient implementation that
	 *           created fewer intermediate objects.
	 * @param <T> The type of element in the stream.
	 * @return A collector confirming that only a single element is present in the stream.
	 * @throws NoSuchElementException if the stream has no elements
	 * @throws IllegalArgumentException if the given stream has more than one element.
	 * @see Stream#collect(Collector)
	 * @see #toFindOnly()
	 * @see <a href="https://stackoverflow.com/q/22694884/421049">Filter Java Stream to 1 and only 1 element</a>
	 */
	public static <T> Collector<T, ?, T> toOnly() {
		return toOnly(() -> new IllegalArgumentException("Multiple elements encountered when only one was expected."));
	}

	/**
	 * Collector that returns the one and only one element expected to be in the stream.
	 * @implNote Code modified from <a href="https://stackoverflow.com/users/2057294/skiwi">skiwi</a> <a href="https://stackoverflow.com/a/22695031/421049">on
	 *           Stack Overflow</a>.
	 * @implNote The current implementation extends an existing list collector. It would probably be possible to create a more efficient implementation that
	 *           created fewer intermediate objects.
	 * @param <T> The type of element in the stream.
	 * @param <X> The type of exception to be thrown if there are many elements.
	 * @param manyElementsExceptionSupplier The strategy for creating an exception to throw if more than one element is present.
	 * @return A collector confirming that only a single element is present in the stream.
	 * @throws NoSuchElementException if the stream has no elements
	 * @throws RuntimeException if the given stream has more than one element.
	 * @see Stream#collect(Collector)
	 * @see #toFindOnly(Supplier)
	 * @see <a href="https://stackoverflow.com/q/22694884/421049">Filter Java Stream to 1 and only 1 element</a>
	 */
	public static <T, X extends RuntimeException> Collector<T, ?, T> toOnly(@Nonnull final Supplier<X> manyElementsExceptionSupplier) {
		return Collectors.collectingAndThen(toList(), list -> {
			if(list.isEmpty()) {
				throw new NoSuchElementException("No element present.");
			}
			if(list.size() > 1) {
				throw manyElementsExceptionSupplier.get();
			}
			return list.get(0);
		});
	}

}
