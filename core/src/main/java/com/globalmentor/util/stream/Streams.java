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

package com.globalmentor.util.stream;

import static java.lang.Math.*;
import static java.util.stream.Collectors.*;

import java.util.*;
import java.util.concurrent.atomic.*;
import java.util.function.*;
import java.util.stream.*;

import org.jspecify.annotations.*;

/**
 * Utilities for working with {@link Stream}s.
 * @author Garret Wilson
 */
public final class Streams {

	private Streams() {
	}

	/**
	 * Reduction operator to require a stream to contain at most one element.
	 * <p>Example usage:</p>
	 * <blockquote>{@code Optional<String> found = stream.reduce(toFindOnly());}</blockquote>
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
	 * <p>Example usage:</p>
	 * <blockquote>{@code Optional<String> found = stream.reduce(toFindOnly(() -> new CustomException("Multiple found")));}</blockquote>
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
	public static <T, X extends RuntimeException> BinaryOperator<T> toFindOnly(@NonNull final Supplier<X> manyElementsExceptionSupplier) {
		return (element, otherElement) -> {
			throw manyElementsExceptionSupplier.get();
		};
	}

	/**
	 * Collector to require a stream to contain at most one element, performing a custom action if more than one element is encountered rather than throwing an
	 * exception.
	 * <p>Example usage:</p>
	 * <blockquote>{@code Optional<String> found = stream.collect(toFindOnlyOrElse(() -> log.warn("Multiple found")));}</blockquote>
	 * @apiNote The method performs functionality similar to the terminal operation {@link Stream#findAny()} except that a custom action is performed if more than
	 *          one element is present, and an empty {@code Optional} is returned to signal the failure condition.
	 * @apiNote This method is useful in scenarios such as annotation processing where instead of throwing an exception, a warning or even an error can be
	 *          reported through a messenger (e.g. {@code javax.annotation.processing.Messager}), allowing the processing round to complete in a controlled manner
	 *          before compilation fails.
	 * @param <T> The type of element in the stream.
	 * @param multipleElementsAction The action to perform when multiple elements are encountered. The action is executed once upon detecting that more than one
	 *          element is present.
	 * @return An {@code Optional} describing an element of the stream if exactly one is present, or an empty {@code Optional} if the stream is empty or contains
	 *         more than one element.
	 * @see Stream#collect(Collector)
	 * @see #toFindOnly()
	 * @see #toFindOnly(Supplier)
	 * @see #toFindAnyWhenMany(Runnable)
	 */
	public static <T> Collector<T, ?, Optional<T>> toFindOnlyOrElse(@NonNull final Runnable multipleElementsAction) {
		final AtomicReference<T> foundElement = new AtomicReference<>();
		final AtomicBoolean multipleFound = new AtomicBoolean(false);
		return collectingAndThen(reducing((element, otherElement) -> {
			if(multipleFound.compareAndSet(false, true)) {
				foundElement.set(element);
				multipleElementsAction.run();
			}
			return null; //doesn't matter what we return here
		}), result -> multipleFound.get() ? Optional.empty() : result);
	}

	/**
	 * Reduction operator to find any element in a stream, performing a custom action when more than one element is encountered.
	 * <p>Example usage:</p>
	 * <blockquote>{@code Optional<String> found = stream.reduce(toFindAnyWhenMany(() -> log.warn("Multiple found")));}</blockquote>
	 * @apiNote The method performs functionality similar to the terminal operation {@link Stream#findAny()} except that a custom action is performed when more
	 *          than one element is present, allowing notification of the ambiguity while still proceeding with one of the elements.
	 * @apiNote This method is useful in scenarios such as annotation processing where multiple elements may be found, but processing can continue with one of
	 *          them after reporting a warning through a messenger (e.g., {@code javax.annotation.processing.Messager}).
	 * @apiNote Unlike {@link #toFindOnlyOrElse(Runnable)}, this method returns one of the elements found rather than an empty {@code Optional}, allowing
	 *          processing to continue.
	 * @implNote As a reduction operation, this will continue processing all stream elements even after multiple elements are detected, which may be inefficient
	 *           for large streams. However, for typical use cases with small streams, this is not a practical concern.
	 * @implNote This method does not guarantee which element will be returned when multiple elements are present, particularly in parallel streams.
	 * @param <T> The type of element in the stream.
	 * @param whenMany The action to perform when multiple elements are encountered. The action is executed once upon detecting that more than one element is
	 *          present.
	 * @return An {@code Optional} describing an element of the stream if one or more elements are present, or an empty {@code Optional} if the stream is empty.
	 * @see Stream#reduce(BinaryOperator)
	 * @see Stream#findAny()
	 * @see #toFindOnlyOrElse(Runnable)
	 */
	public static <T> BinaryOperator<T> toFindAnyWhenMany(@NonNull final Runnable whenMany) {
		final AtomicBoolean manyFound = new AtomicBoolean(false);
		return (element, otherElement) -> {
			if(manyFound.compareAndSet(false, true)) {
				whenMany.run();
			}
			return element; //return the first element encountered, allowing processing to continue
		};
	}

	/**
	 * Collector that returns the one and only one element expected to be in the stream.
	 * <p>Example usage:</p>
	 * <blockquote>{@code String only = stream.collect(toOnly());}</blockquote>
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
	 * <p>Example usage:</p>
	 * <blockquote>{@code String only = stream.collect(toOnly(() -> new CustomException("Multiple found")));}</blockquote>
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
	public static <T, X extends RuntimeException> Collector<T, ?, T> toOnly(@NonNull final Supplier<X> manyElementsExceptionSupplier) {
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

	/**
	 * Returns a stream that "zips" two other streams by applying some function to each subsequent pair of elements retrieved separately from the streams,
	 * ignoring and discarding remaining elements from the longer stream.
	 * @apiNote This method is similar to Guava's <a href=
	 *          "https://guava.dev/releases/snapshot/api/docs/com/google/common/collect/Streams.html#zip(java.util.stream.Stream,java.util.stream.Stream,java.util.function.BiFunction)"><code>Streams.zip()</code></a>
	 *          method. Compare also Python's <a href="https://docs.python.org/3.3/library/functions.html#zip"><code>zip()</code></a> method.
	 * @implNote This implementation follows closely that of Guava's <code>com.google.common.collect.Streams.zip()</code> method.
	 * @param <A> The type of elements in the first stream.
	 * @param <B> The type of elements in the second stream.
	 * @param <R> The type of elements in the resulting zipped stream.
	 * @param streamA The first stream to zip.
	 * @param streamB The second stream to zip.
	 * @param zipper The function for combining the elements.
	 * @return A new stream zipping the contents of the two input streams.
	 */
	public static <A, B, R> Stream<R> zip(@NonNull final Stream<A> streamA, @NonNull final Stream<B> streamB,
			@NonNull final BiFunction<? super A, ? super B, R> zipper) {
		return zip(streamA, streamB, false, zipper);
	}

	/**
	 * Returns a stream that "zips" two other streams by applying some function to each subsequent pair of elements retrieved separately from the streams.
	 * @apiNote This method is similar to Guava's <a href=
	 *          "https://guava.dev/releases/snapshot/api/docs/com/google/common/collect/Streams.html#zip(java.util.stream.Stream,java.util.stream.Stream,java.util.function.BiFunction)"><code>Streams.zip()</code></a>
	 *          method, except that this method has the option not to discard data from the longer stream. Compare also Python's
	 *          <a href="https://docs.python.org/3.3/library/functions.html#zip"><code>zip()</code></a> method.
	 * @implNote This implementation follows closely that of Guava's <code>com.google.common.collect.Streams.zip()</code> method.
	 * @param <A> The type of elements in the first stream.
	 * @param <B> The type of elements in the second stream.
	 * @param <R> The type of elements in the resulting zipped stream.
	 * @param streamA The first stream to zip.
	 * @param streamB The second stream to zip.
	 * @param retainExtraElements <code>true</code> if any additional elements from the longer stream will be kept, paired with <code>null</code> for the other
	 *          stream; or <code>false</code> if additional element will be ignored and discarded.
	 * @param zipper The function for combining the elements.
	 * @return A new stream zipping the contents of the two input streams.
	 */
	public static <A, B, R> Stream<R> zip(@NonNull final Stream<A> streamA, @NonNull final Stream<B> streamB, final boolean retainExtraElements,
			@NonNull final BiFunction<? super A, ? super B, R> zipper) {
		return zip(streamA, streamB, null, null, retainExtraElements, zipper);
	}

	/**
	 * Returns a stream that "zips" two other streams containing the same type of element by applying some function to each subsequent pair of elements retrieved
	 * separately from the streams, providing default values for the shorter stream.
	 * @apiNote This method is similar to Guava's <a href=
	 *          "https://guava.dev/releases/snapshot/api/docs/com/google/common/collect/Streams.html#zip(java.util.stream.Stream,java.util.stream.Stream,java.util.function.BiFunction)"><code>Streams.zip()</code></a>
	 *          method, except that this method does not to discard data from the longer stream, instead specifying default elements. Compare also Python's
	 *          <a href="https://docs.python.org/3.3/library/functions.html#zip"><code>zip()</code></a> method.
	 * @implNote This implementation follows closely that of Guava's <code>com.google.common.collect.Streams.zip()</code> method.
	 * @param <T> The type of elements in the streams.
	 * @param <R> The type of elements in the resulting zipped stream.
	 * @param streamA The first stream to zip.
	 * @param streamB The second stream to zip.
	 * @param defaultElement The default element to use for each stream if that stream is depleted of elements while the other stream still has elements.
	 * @param zipper The function for combining the elements.
	 * @return A new stream zipping the contents of the two input streams.
	 */
	public static <T, R> Stream<R> zip(@NonNull final Stream<T> streamA, @NonNull final Stream<T> streamB,
			@NonNull final BiFunction<? super T, ? super T, R> zipper, @Nullable final T defaultElement) {
		return zip(streamA, streamB, defaultElement, defaultElement, zipper);
	}

	/**
	 * Returns a stream that "zips" two other streams by applying some function to each subsequent pair of elements retrieved separately from the streams,
	 * providing default values for the shorter stream.
	 * @apiNote This method is similar to Guava's <a href=
	 *          "https://guava.dev/releases/snapshot/api/docs/com/google/common/collect/Streams.html#zip(java.util.stream.Stream,java.util.stream.Stream,java.util.function.BiFunction)"><code>Streams.zip()</code></a>
	 *          method, except that this method does not to discard data from the longer stream, instead specifying default elements. Compare also Python's
	 *          <a href="https://docs.python.org/3.3/library/functions.html#zip"><code>zip()</code></a> method.
	 * @implNote This implementation follows closely that of Guava's <code>com.google.common.collect.Streams.zip()</code> method.
	 * @param <A> The type of elements in the first stream.
	 * @param <B> The type of elements in the second stream.
	 * @param <R> The type of elements in the resulting zipped stream.
	 * @param streamA The first stream to zip.
	 * @param streamB The second stream to zip.
	 * @param defaultElementA The default element to use for the first stream if that stream is depleted of elements while the other stream still has elements.
	 * @param defaultElementB The default element to use for the second stream if that stream is depleted of elements while the other stream still has elements.
	 * @param zipper The function for combining the elements.
	 * @return A new stream zipping the contents of the two input streams.
	 */
	public static <A, B, R> Stream<R> zip(@NonNull final Stream<A> streamA, @NonNull final Stream<B> streamB, @Nullable final A defaultElementA,
			@Nullable final B defaultElementB, @NonNull final BiFunction<? super A, ? super B, R> zipper) {
		return zip(streamA, streamB, defaultElementA, defaultElementB, true, zipper);
	}

	/**
	 * Returns a stream that "zips" two other streams by applying some function to each subsequent pair of elements retrieved separately from the streams.
	 * @apiNote This method is similar to Guava's <a href=
	 *          "https://guava.dev/releases/snapshot/api/docs/com/google/common/collect/Streams.html#zip(java.util.stream.Stream,java.util.stream.Stream,java.util.function.BiFunction)"><code>Streams.zip()</code></a>
	 *          method, except that this method has the option not to discard data from the longer stream, along with the ability to specify default elements.
	 *          Compare also Python's <a href="https://docs.python.org/3.3/library/functions.html#zip"><code>zip()</code></a> method.
	 * @implNote This implementation follows closely that of Guava's <code>com.google.common.collect.Streams.zip()</code> method.
	 * @param <A> The type of elements in the first stream.
	 * @param <B> The type of elements in the second stream.
	 * @param <R> The type of elements in the resulting zipped stream.
	 * @param streamA The first stream to zip.
	 * @param streamB The second stream to zip.
	 * @param defaultElementA The default element to use for the first stream if that stream is depleted of elements while the other stream still has elements.
	 *          Only used if <code><var>retainExtraElements</var></code> is <code>true</code>.
	 * @param defaultElementB The default element to use for the second stream if that stream is depleted of elements while the other stream still has elements.
	 *          Only used if <code><var>retainExtraElements</var></code> is <code>true</code>.
	 * @param retainExtraElements <code>true</code> if any additional elements from the longer stream will be kept, paired with the default value specified for
	 *          the other stream; or <code>false</code> if additional element will be ignored and discarded.
	 * @param zipper The function for combining the elements.
	 * @return A new stream zipping the contents of the two input streams.
	 */
	static <A, B, R> Stream<R> zip(@NonNull final Stream<A> streamA, @NonNull final Stream<B> streamB, @Nullable final A defaultElementA,
			@Nullable final B defaultElementB, final boolean retainExtraElements, @NonNull final BiFunction<? super A, ? super B, R> zipper) {
		final boolean isParallel = streamA.isParallel() || streamB.isParallel();
		final Spliterator<A> spliteratorA = streamA.spliterator();
		final Spliterator<B> spliteratorB = streamB.spliterator();
		//make the resulting stream sized and ordered, but only if the input streams are sized and/or ordered
		final int characteristics = (Spliterator.SIZED | Spliterator.ORDERED) & spliteratorA.characteristics() & spliteratorB.characteristics();
		final Iterator<A> iteratorA = Spliterators.iterator(spliteratorA);
		final Iterator<B> iteratorB = Spliterators.iterator(spliteratorB);
		final long estimatedSize = retainExtraElements ? max(spliteratorA.estimateSize(), spliteratorB.estimateSize())
				: min(spliteratorA.estimateSize(), spliteratorB.estimateSize());
		return StreamSupport.stream(new Spliterators.AbstractSpliterator<R>(estimatedSize, characteristics) {
			@Override
			public boolean tryAdvance(final Consumer<? super R> action) {
				final A nextA;
				final B nextB;
				if(iteratorA.hasNext()) {
					nextA = iteratorA.next();
					if(iteratorB.hasNext()) {
						nextB = iteratorB.next();
					} else {
						if(!retainExtraElements) {
							return false;
						}
						nextB = defaultElementB;
					}
				} else if(iteratorB.hasNext() && retainExtraElements) {
					nextA = defaultElementA;
					nextB = iteratorB.next();
				} else {
					return false;
				}
				action.accept(zipper.apply(nextA, nextB));
				return true;
			}
		}, isParallel).onClose(streamA::close).onClose(streamB::close);
	}

}
