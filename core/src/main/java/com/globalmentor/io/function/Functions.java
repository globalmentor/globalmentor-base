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

package com.globalmentor.io.function;

import java.util.concurrent.atomic.AtomicLong;
import java.util.function.*;

import javax.annotation.*;

/**
 * Utility higher-order functions.
 * @author Garret Wilson
 */
public class Functions {

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
	public static <T> Consumer<T> countingConsumer(@Nonnull final BiConsumer<T, Long> biConsumer) {
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
	public static <T> Consumer<T> countingConsumer(@Nonnull final AtomicLong counter, @Nonnull final BiConsumer<T, Long> biConsumer) {
		return new Consumer<T>() {
			@Override
			public void accept(final T t) {
				biConsumer.accept(t, counter.incrementAndGet());
			}

		};
	}

}
