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

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Supplier;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link LazySupplier}.
 * @author Garret Wilson
 */
public class LazySupplierTest {

	/**
	 * Base class for common tests across all {@link LazySupplier} implementations.
	 */
	abstract static class BaseTests {

		/**
		 * Creates a lazy supplier for testing.
		 * @param <T> The type of value supplied.
		 * @param supplier The supplier to wrap.
		 * @return A lazy supplier.
		 */
		protected abstract <T> LazySupplier<T> createLazySupplier(Supplier<T> supplier);

		/** Supplier is invoked on first access. */
		@Test
		void testSupplierInvokedOnFirstAccess() {
			final AtomicLong invocationCount = new AtomicLong(0);
			final LazySupplier<String> lazySupplier = createLazySupplier(() -> {
				invocationCount.incrementAndGet();
				return "value";
			});
			assertThat("supplier not invoked before first access", invocationCount.get(), is(0L));
			assertThat(lazySupplier.get(), is("value"));
			assertThat("supplier invoked once after first access", invocationCount.get(), is(1L));
		}

		/** Supplier is not invoked again on subsequent access. */
		@Test
		void testValueCached() {
			final AtomicLong invocationCount = new AtomicLong(0);
			final LazySupplier<String> lazySupplier = createLazySupplier(() -> {
				invocationCount.incrementAndGet();
				return "cached";
			});
			lazySupplier.get();
			lazySupplier.get();
			lazySupplier.get();
			assertThat("supplier invoked only once", invocationCount.get(), is(1L));
		}

		/** Cached value is returned on subsequent access. */
		@Test
		void testReturnsCachedValue() {
			final LazySupplier<String> lazySupplier = createLazySupplier(() -> "result");
			final String firstResult = lazySupplier.get();
			final String secondResult = lazySupplier.get();
			final String thirdResult = lazySupplier.get();
			assertThat("first result", firstResult, is("result"));
			assertThat("second result is same", secondResult, is(sameInstance(firstResult)));
			assertThat("third result is same", thirdResult, is(sameInstance(firstResult)));
		}

		/** Supplier returning null throws {@link IllegalStateException}. */
		@Test
		void testThrowsIfSupplierReturnsNull() {
			final LazySupplier<String> lazySupplier = createLazySupplier(() -> null);
			assertThrows(IllegalStateException.class, lazySupplier::get);
		}

		/** If supplier throws exception, value is not cached and supplier is invoked again on next call. */
		@Test
		void testExceptionNotCached() {
			final AtomicLong invocationCount = new AtomicLong(0);
			final LazySupplier<String> lazySupplier = createLazySupplier(() -> {
				invocationCount.incrementAndGet();
				throw new RuntimeException("Test exception");
			});
			assertThrows(RuntimeException.class, lazySupplier::get);
			assertThat("first invocation", invocationCount.get(), is(1L));
			assertThrows(RuntimeException.class, lazySupplier::get);
			assertThat("second invocation after exception", invocationCount.get(), is(2L));
		}

		/** If supplier throws exception then succeeds, successful value is cached. */
		@Test
		void testSuccessfulValueCachedAfterException() {
			final AtomicLong invocationCount = new AtomicLong(0);
			final LazySupplier<String> lazySupplier = createLazySupplier(() -> {
				final long count = invocationCount.incrementAndGet();
				if(count == 1) {
					throw new RuntimeException("First call fails");
				}
				return "success";
			});
			assertThrows(RuntimeException.class, lazySupplier::get);
			assertThat("first result after exception", lazySupplier.get(), is("success"));
			assertThat("value cached", lazySupplier.get(), is("success"));
			assertThat("supplier invoked only twice", invocationCount.get(), is(2L));
		}
	}

	// Tests for [LazySupplier#of(Supplier)].

	@Nested
	class OfTests extends BaseTests {

		@Override
		protected <T> LazySupplier<T> createLazySupplier(final Supplier<T> supplier) {
			return LazySupplier.of(supplier);
		}

		/** Non-thread-safe implementation is LazySupplier. */
		@Test
		void testReturnsLazySupplier() {
			final LazySupplier<String> lazySupplier = LazySupplier.of(() -> "test");
			assertThat(lazySupplier, is(instanceOf(LazySupplier.class)));
		}
	}

	// Tests for [LazySupplier#ofAtomic(Supplier)].

	@Nested
	class OfAtomicTests extends BaseTests {

		@Override
		protected <T> LazySupplier<T> createLazySupplier(final Supplier<T> supplier) {
			return LazySupplier.ofAtomic(supplier);
		}

		/** Atomic implementation is LazySupplier. */
		@Test
		void testReturnsLazySupplier() {
			final LazySupplier<String> lazySupplier = LazySupplier.ofAtomic(() -> "test");
			assertThat(lazySupplier, is(instanceOf(LazySupplier.class)));
		}
	}

	// Tests for [LazySupplier#ofSynchronized(Supplier)].

	@Nested
	class OfSynchronizedTests extends BaseTests {

		@Override
		protected <T> LazySupplier<T> createLazySupplier(final Supplier<T> supplier) {
			return LazySupplier.ofSynchronized(supplier);
		}

		/** Synchronized implementation is LazySupplier. */
		@Test
		void testReturnsLazySupplier() {
			final LazySupplier<String> lazySupplier = LazySupplier.ofSynchronized(() -> "test");
			assertThat(lazySupplier, is(instanceOf(LazySupplier.class)));
		}
	}

}
