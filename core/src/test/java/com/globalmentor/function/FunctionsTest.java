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

import static java.util.Arrays.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.time.LocalDate;
import java.util.*;
import java.util.concurrent.atomic.AtomicLong;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link Functions}.
 * @author Garret Wilson
 */
public class FunctionsTest {

	/** {@link Functions#countingConsumer(java.util.function.BiConsumer)} */
	@Test
	void testCountingConsumer() {
		final List<Map.Entry<String, Long>> results = new ArrayList<>();
		asList("foo", "bar", "baz").forEach(Functions.countingConsumer((id, count) -> results.add(Map.entry(id, count))));
		assertThat(results, contains(Map.entry("foo", 1L), Map.entry("bar", 2L), Map.entry("baz", 3L)));
	}

	/** {@link Functions#countingConsumer(AtomicLong, java.util.function.BiConsumer) */
	@Test
	void testCountingConsumerWithProvidedCounter() {
		final AtomicLong counter = new AtomicLong(5);
		final List<Map.Entry<String, Long>> results = new ArrayList<>();
		asList("foo", "bar", "baz").forEach(Functions.countingConsumer(counter, (id, count) -> results.add(Map.entry(id, count))));
		assertThat(results, contains(Map.entry("foo", 6L), Map.entry("bar", 7L), Map.entry("baz", 8L)));
	}

	/** {@link Functions#countingConsumer(java.util.function.BiConsumer)} */
	@Test
	void testCountingConsumerEmpty() {
		final List<Map.Entry<String, Long>> results = new ArrayList<>();
		Collections.<String>emptySet().forEach(Functions.countingConsumer((id, count) -> results.add(Map.entry(id, count))));
		assertThat(results, is(empty()));
	}

	/** {@link Functions#testing(java.util.function.Function, java.util.function.Predicate)} */
	void testTesting() {
		Set<LocalDate> localDates = Set.of(LocalDate.of(1950, 01, 02), LocalDate.of(1960, 03, 04), LocalDate.of(1970, 10, 20), LocalDate.of(1985, 11, 05),
				LocalDate.of(1999, 12, 31), LocalDate.of(2005, 05, 05), LocalDate.of(2015, 06, 25));
		assertThat(localDates.stream().filter(Functions.testing(LocalDate::getYear, __ -> __ < 2000))::iterator,
				contains(LocalDate.of(1950, 01, 02), LocalDate.of(1960, 03, 04), LocalDate.of(1970, 10, 20), LocalDate.of(1985, 11, 05), LocalDate.of(1999, 12, 31)));

	}

	// Tests for [Functions#supplyLazily(java.util.function.Supplier)].

	/** Supplier is invoked on first access. */
	@Test
	void testSupplyLazilyInvokesSupplierOnFirstAccess() {
		final AtomicLong invocationCount = new AtomicLong(0);
		final var lazySupplier = Functions.supplyLazily(() -> {
			invocationCount.incrementAndGet();
			return "value";
		});
		assertThat("supplier not invoked before first access", invocationCount.get(), is(0L));
		assertThat(lazySupplier.get(), is("value"));
		assertThat("supplier invoked once after first access", invocationCount.get(), is(1L));
	}

	/** Supplier is not invoked again on subsequent access. */
	@Test
	void testSupplyLazilyCachesValue() {
		final AtomicLong invocationCount = new AtomicLong(0);
		final var lazySupplier = Functions.supplyLazily(() -> {
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
	void testSupplyLazilyReturnsCachedValue() {
		final var lazySupplier = Functions.supplyLazily(() -> "result");
		final String firstResult = lazySupplier.get();
		final String secondResult = lazySupplier.get();
		final String thirdResult = lazySupplier.get();
		assertThat("first result", firstResult, is("result"));
		assertThat("second result is same", secondResult, is(sameInstance(firstResult)));
		assertThat("third result is same", thirdResult, is(sameInstance(firstResult)));
	}

	/** Null values are supported. */
	@Test
	void testSupplyLazilySupportsNull() {
		final AtomicLong invocationCount = new AtomicLong(0);
		final var lazySupplier = Functions.supplyLazily(() -> {
			invocationCount.incrementAndGet();
			return null;
		});
		assertThat(lazySupplier.get(), is(nullValue()));
		assertThat(lazySupplier.get(), is(nullValue()));
		assertThat("supplier invoked only once", invocationCount.get(), is(1L));
	}

	/** Supplier returning itself throws IllegalStateException. */
	@Test
	void testSupplyLazilyThrowsIfSupplierReturnsItself() {
		final var selfReturningSupplier = new java.util.function.Supplier<Object>() {
			@Override
			public Object get() {
				return this;
			}
		};
		final var lazySupplier = Functions.supplyLazily(selfReturningSupplier);
		assertThrows(IllegalStateException.class, lazySupplier::get);
	}

}
