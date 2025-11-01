/*
 * Copyright © 2023 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.function;

import static com.globalmentor.java.Conditions.*;
import static java.util.Objects.*;

import java.util.concurrent.atomic.AtomicLong;
import java.util.function.*;

import org.jspecify.annotations.*;

/**
 * Utility higher-order functions.
 * @author Garret Wilson
 */
public final class Functions {

	private Functions() {
	}

	/**
	 * Creates a consumer that counts the number of times it has been invoked and passes that count to the provided consumer. Use the function to replace this:
	 * <pre><code>
	 * fooStream.forEach(foo -> doSomething());
	 * </code></pre> with this: <pre><code>
	 * fooStream.forEach(countingConsumer((foo, count) -> doSomething());
	 * </code></pre>
	 * @apiNote The returned consumer uses one-based "counts" and not zero-based "indexes" because an index implies some sort of order, and also makes it awkward
	 *          to keep track of an initial value when the consumer may never have been invoked. A "count" starts off with zero invocations, and in a parallel
	 *          context the concept of an "index" would not necessarily be accurate, as consumers may be called in an arbitrary order.
	 * @implSpec This implementation delegates to {@link #countingConsumer(AtomicLong, BiConsumer)} with a new counter initially set to zero.
	 * @implNote The returned consumer is thread-safe; each invocation will provide a unique count, although with concurrent invocations the counts will not
	 *           necessarily be in order.
	 * @param <T> The type of input argument.
	 * @param biConsumer The consumer accepting the input argument along with the count.
	 * @return A consumer that keeps track of the count of invocations and passes that to the enclosed consumer.
	 */
	public static <T> Consumer<T> countingConsumer(@NonNull final BiConsumer<T, Long> biConsumer) {
		return countingConsumer(new AtomicLong(0), biConsumer);
	}

	/**
	 * Creates a consumer that counts the number of times it has been invoked and passes that count to the provided consumer.
	 * @apiNote This version allows an existing counter to be used, e.g. to retrieve the count after or during stream traversal; or to continue counting in other
	 *          contexts (such as nested loops). See {@link #countingConsumer(BiConsumer)} for usage example.
	 * @apiNote The returned consumer uses one-based "counts" and not zero-based "indexes" because an index implies some sort of order, and also makes it awkward
	 *          to keep track of an initial value when the consumer may never have been invoked. A "count" starts off with zero invocations, and in a parallel
	 *          context the concept of an "index" would not necessarily be accurate, as consumers may be called in an arbitrary order.
	 * @implNote The returned consumer is thread-safe; each invocation will provide a unique count, although with concurrent invocations the counts will not
	 *           necessarily be in order.
	 * @param <T> The type of input argument.
	 * @param counter An existing counter to use.
	 * @param biConsumer The consumer accepting the input argument along with the count.
	 * @return A consumer that keeps track of the count of invocations and passes that to the enclosed consumer.
	 */
	public static <T> Consumer<T> countingConsumer(@NonNull final AtomicLong counter, @NonNull final BiConsumer<T, Long> biConsumer) {
		return new Consumer<T>() {
			@Override
			public void accept(final T t) {
				biConsumer.accept(t, counter.incrementAndGet());
			}
		};
	}

	/**
	 * Converts a function to a bifunction by passing the <em>first</em> bifunction argument <code><var>t</var></code> to the function and ignoring the second.
	 * @param <T> The type of the first argument to the bifunction.
	 * @param <U> The type of the second argument to the bifunction.
	 * @param <R> The type of the result of the function.
	 * @param function The function to convert to a bifunction.
	 * @return A bifunction that delegates to the given function, passing its first argument.
	 */
	public static <T, U, R> BiFunction<T, U, R> toBiFunctionT(@NonNull final Function<? super T, ? extends R> function) {
		requireNonNull(function);
		return (final T t, final U u) -> function.apply(t);
	}

	/**
	 * Converts a function to a bifunction by passing the <em>second</em> bifunction argument <code><var>u</var></code> to the function and ignoring the first.
	 * @param <T> The type of the first argument to the bifunction.
	 * @param <U> The type of the second argument to the bifunction.
	 * @param <R> The type of the result of the function.
	 * @param function The function to convert to a bifunction.
	 * @return A bifunction that delegates to the given function, passing its second argument.
	 */
	public static <T, U, R> BiFunction<T, U, R> toBiFunctionU(@NonNull final Function<? super U, ? extends R> function) {
		requireNonNull(function);
		return (final T t, final U u) -> function.apply(u);
	}

	/**
	 * Accepts a function that extracts a value from a type <code>T</code> and returns a {@code Predicate<T>} that tests the extracted value using the specified
	 * predicate.
	 * @apiNote The following example obtains a predicate that filters <code>Person</code> objects having a middle name, assuming that
	 *          <code>Person.findMiddleName()</code> returns {@code Optional<String>}. <pre>{@code
	 *   List<Person> people = …;
	 *   people.stream().filter(testing(Person::findMiddleName, Optional::isPresent)).…
	 * }</pre>
	 * @apiNote This pattern follows that of {@link java.util.Comparator#comparing(Function, Comparator)}.
	 * @param <T> The type of input to be tested.
	 * @param <U> The type of the extracted value.
	 * @param valueExtractor The function used to extract the actual value to test.
	 * @param valueTester The predicate used to test the value extracted from the input.
	 * @return A predicate that tests an extracted value using the specified predicate.
	 * @throws NullPointerException if either argument is <code>null</code>
	 */
	public static <T, U> Predicate<T> testing(@NonNull final Function<? super T, ? extends U> valueExtractor, @NonNull final Predicate<? super U> valueTester) {
		requireNonNull(valueExtractor);
		requireNonNull(valueTester);
		return input -> valueTester.test(valueExtractor.apply(input));
	}

	/**
	 * Creates a thread-safe supplier that lazily initializes and caches the result of the given supplier on first access. The supplier must not return itself as
	 * its value.
	 * <p>The returned supplier is thread-safe and guarantees the original supplier completes successfully at most once, even under concurrent access. Subsequent
	 * calls return the cached value with no synchronization overhead. If the original supplier throws an exception, the value is not cached and the supplier will
	 * be invoked again on the next call.</p>
	 * @apiNote This is useful for values that may not be needed, but if needed once may be needed multiple times.
	 * @implNote Uses double-checked locking for optimal performance after initialization.
	 * @param <T> The type of value supplied.
	 * @param supplier The supplier to invoke on first access.
	 * @return A new lazy, thread-safe supplier that caches the result.
	 * @throws IllegalStateException if the supplier returns itself as its value.
	 */
	public static <T> Supplier<T> supplyLazily(@NonNull final Supplier<T> supplier) {
		requireNonNull(supplier);
		return new Supplier<T>() {
			private volatile Object value = supplier;

			@Override
			public T get() {
				if(value == supplier) {
					synchronized(supplier) {
						if(value == supplier) {
							value = supplier.get();
							checkState(value != supplier, "Supplier cannot return itself as its value.");
						}
					}
				}
				@SuppressWarnings("unchecked")
				final T typedResult = (T)value;
				return typedResult;
			}
		};
	}

}
