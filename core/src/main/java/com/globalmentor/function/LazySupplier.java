/*
 * Copyright © 2025 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;

import org.jspecify.annotations.*;

/**
 * A supplier that lazily initializes and caches a value on first access.
 * @apiNote This is useful for values that may not be needed, but if needed once may be needed multiple times.
 * @param <T> The type of value supplied.
 * @author Garret Wilson
 */
public interface LazySupplier<T> extends Supplier<T> {

	/**
	 * Creates a non-thread-safe lazy supplier that caches the result of the given supplier on first access.
	 * <p>The returned supplier is <strong>not thread-safe</strong>. If the original supplier throws an exception, the value is not cached and the supplier will
	 * be invoked again on the next call.</p>
	 * @apiNote This implementation has minimal overhead and is appropriate for single-threaded usage or when external synchronization is provided.
	 * @param <T> The type of value supplied.
	 * @param supplier The supplier to invoke on first access. The supplier must not return <code>null</code>.
	 * @return A new lazy supplier that caches the result.
	 * @throws IllegalStateException if the supplier returns <code>null</code>.
	 */
	static <T> LazySupplier<T> of(@NonNull final Supplier<T> supplier) {
		requireNonNull(supplier);
		return new LazySupplier<T>() {
			private T value = null;

			@Override
			public T get() {
				if(value == null) {
					value = supplier.get();
					checkState(value != null, "Supplier cannot return `null`.");
				}
				return value;
			}
		};
	}

	/**
	 * Creates a thread-safe lazy supplier using atomic operations that caches the result of the given supplier on first access.
	 * <p>The returned supplier is thread-safe using lock-free atomic operations. Under concurrent access, the original supplier may be invoked multiple times,
	 * but only the first non-<code>null</code> result will be cached. All subsequent calls will return that same cached instance. If the original supplier throws
	 * an exception, the value is not cached and the supplier will be invoked again on the next call.</p>
	 * @implSpec This implementation uses an {@link AtomicReference}.
	 * @implNote This implementation provides thread safety with minimal overhead using compare-and-swap operations, but does not guarantee the supplier is
	 *           invoked only once under contention.
	 * @param <T> The type of value supplied.
	 * @param supplier The supplier to invoke on first access. The supplier must not return <code>null</code>.
	 * @return A new lazy, thread-safe supplier that caches the result.
	 * @throws IllegalStateException if the supplier returns <code>null</code>.
	 */
	static <T> LazySupplier<T> ofAtomic(@NonNull final Supplier<T> supplier) {
		requireNonNull(supplier);
		return new LazySupplier<T>() {
			private final AtomicReference<T> reference = new AtomicReference<>();

			@Override
			public T get() {
				T value = reference.get();
				if(value == null) {
					value = supplier.get();
					checkState(value != null, "Supplier cannot return `null`.");
					if(!reference.compareAndSet(null, value)) {
						value = reference.get(); // another thread won the race; use its value and discard ours
						assert value != null : "The compare-and-set conditions should prevent the value from being `null`.";
					}
				}
				return value;
			}
		};
	}

	/**
	 * Creates a thread-safe lazy supplier using synchronized blocks that caches the result of the given supplier on first access.
	 * <p>The returned supplier is thread-safe and guarantees the original supplier completes successfully at most once, even under concurrent access. Subsequent
	 * calls return the cached value with no synchronization overhead. If the original supplier throws an exception, the value is not cached and the supplier will
	 * be invoked again on the next call.</p>
	 * @implSpec This implementation uses synchronization on the {@link LazySupplier} interface itself.
	 * @implSpec This implementation guarantees the supplier is invoked at most once successfully, using double-checked locking for optimal performance after
	 *           initialization.
	 * @param <T> The type of value supplied.
	 * @param supplier The supplier to invoke on first access. The supplier must not return <code>null</code>.
	 * @return A new lazy, thread-safe supplier that caches the result.
	 * @throws IllegalStateException if the supplier returns <code>null</code>.
	 */
	static <T> LazySupplier<T> ofSynchronized(@NonNull final Supplier<T> supplier) {
		requireNonNull(supplier);
		return new LazySupplier<T>() {
			private volatile T value = null;

			@Override
			public T get() {
				if(value == null) {
					synchronized(LazySupplier.class) {
						if(value == null) {
							value = supplier.get();
							checkState(value != null, "Supplier cannot return `null`.");
						}
					}
				}
				return value;
			}
		};
	}

}
