/*
 * Copyright © 2022 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.time;

import static java.util.concurrent.TimeUnit.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.time.Duration;
import java.util.concurrent.atomic.AtomicLong;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link ElapsingTime}.
 * @author Garret Wilson
 */
public class ElapsingTimeTest {

	@Test
	void testNanoseconds() {
		final AtomicLong fakeTime = new AtomicLong(12345678910L);
		final ElapsingTime elapsingTime = ElapsingTime.fromNow(fakeTime::get, NANOSECONDS);
		assertThat(elapsingTime.get(NANOSECONDS), is(0L));
		assertThat(elapsingTime.get(MILLISECONDS), is(0L));
		fakeTime.addAndGet(1234567L);
		assertThat(elapsingTime.get(NANOSECONDS), is(1234567L));
		assertThat(elapsingTime.get(MILLISECONDS), is(1L));
	}

	@Test
	void testMilliseconds() {
		final AtomicLong fakeTime = new AtomicLong(12345678910L);
		final ElapsingTime elapsingTime = ElapsingTime.fromNow(fakeTime::get, MILLISECONDS);
		assertThat(elapsingTime.get(MILLISECONDS), is(0L));
		assertThat(elapsingTime.get(SECONDS), is(0L));
		fakeTime.addAndGet(1234L);
		assertThat(elapsingTime.get(NANOSECONDS), is(1234000000L));
		assertThat(elapsingTime.get(MILLISECONDS), is(1234L));
		assertThat(elapsingTime.get(SECONDS), is(1L));
	}

	//## withers

	/** @see ElapsingTime#withElapsingFromNow() */
	@Test
	void testWithElapsingFromNow() {
		final AtomicLong fakeTime = new AtomicLong(12345678910L);
		final ElapsingTime elapsingTime = ElapsingTime.fromNow(fakeTime::get, MILLISECONDS);
		fakeTime.addAndGet(1234L); //the amount here is arbitrary, just to see that adjustment will still occur
		final ElapsingTime newElapsingTime = elapsingTime.withElapsingFromNow();
		assertThat("New elapsing time has correct time.", newElapsingTime.get(NANOSECONDS), is(0L));
		assertThat("New elapsing time has same time suplier.", newElapsingTime.getTimeSupplier(), is(elapsingTime.getTimeSupplier()));
		assertThat("New elapsing time has same time unit.", newElapsingTime.getTimeUnit(), is(elapsingTime.getTimeUnit()));
	}

	/** @see ElapsingTime#withElapsed(Duration) */
	@Test
	void testWithElapsedZero() {
		final AtomicLong fakeTime = new AtomicLong(12345678910L);
		final ElapsingTime elapsingTime = ElapsingTime.fromNow(fakeTime::get, MILLISECONDS);
		fakeTime.addAndGet(1234L); //the amount here is arbitrary, just to see that adjustment will still occur
		final ElapsingTime newElapsingTime = elapsingTime.withElapsed(Duration.ZERO);
		assertThat("New elapsing time has correct time.", newElapsingTime.get(NANOSECONDS), is(0L));
		assertThat("New elapsing time has same time suplier.", newElapsingTime.getTimeSupplier(), is(elapsingTime.getTimeSupplier()));
		assertThat("New elapsing time has same time unit.", newElapsingTime.getTimeUnit(), is(elapsingTime.getTimeUnit()));
	}

	/** @see ElapsingTime#withElapsed(Duration) */
	@Test
	void testWithElapsedNanoseconds() {
		final AtomicLong fakeTime = new AtomicLong(12345678910L);
		final ElapsingTime elapsingTime = ElapsingTime.fromNow(fakeTime::get, NANOSECONDS);
		fakeTime.addAndGet(1234L); //the amount here is arbitrary, just to see that adjustment will still occur
		final ElapsingTime newElapsingTime = elapsingTime.withElapsed(Duration.ofNanos(54321));
		assertThat("New elapsing time has correct time.", newElapsingTime.get(NANOSECONDS), is(54321L));
		assertThat("New elapsing time has same time suplier.", newElapsingTime.getTimeSupplier(), is(elapsingTime.getTimeSupplier()));
		assertThat("New elapsing time has same time unit.", newElapsingTime.getTimeUnit(), is(elapsingTime.getTimeUnit()));
	}

	/** @see ElapsingTime#withElapsed(Duration) */
	@Test
	void testWithElapsedMillesconds() {
		final AtomicLong fakeTime = new AtomicLong(12345678910L);
		final ElapsingTime elapsingTime = ElapsingTime.fromNow(fakeTime::get, MILLISECONDS);
		fakeTime.addAndGet(1234L); //the amount here is arbitrary, just to see that adjustment will still occur
		final ElapsingTime newElapsingTime = elapsingTime.withElapsed(Duration.ofMillis(54321));
		assertThat("New elapsing time has correct time.", newElapsingTime.get(MILLISECONDS), is(54321L));
		assertThat("New elapsing time has same time suplier.", newElapsingTime.getTimeSupplier(), is(elapsingTime.getTimeSupplier()));
		assertThat("New elapsing time has same time unit.", newElapsingTime.getTimeUnit(), is(elapsingTime.getTimeUnit()));
	}

	/** @see ElapsingTime#withElapsed(Duration) */
	@Test
	void testWithElapsedMillescondsNegative() {
		final AtomicLong fakeTime = new AtomicLong(12345678910L);
		final ElapsingTime elapsingTime = ElapsingTime.fromNow(fakeTime::get, MILLISECONDS);
		fakeTime.addAndGet(1234L); //the amount here is arbitrary, just to see that adjustment will still occur
		final ElapsingTime newElapsingTime = elapsingTime.withElapsed(Duration.ofMillis(-54321));
		assertThat("New elapsing time has correct time.", newElapsingTime.get(MILLISECONDS), is(-54321L));
		assertThat("New elapsing time has same time suplier.", newElapsingTime.getTimeSupplier(), is(elapsingTime.getTimeSupplier()));
		assertThat("New elapsing time has same time unit.", newElapsingTime.getTimeUnit(), is(elapsingTime.getTimeUnit()));
	}

	/** @see ElapsingTime#withElapsed(Duration) */
	@Test
	void testWithElapsedSeconds() {
		final AtomicLong fakeTime = new AtomicLong(12345678910L);
		final ElapsingTime elapsingTime = ElapsingTime.fromNow(fakeTime::get, SECONDS);
		fakeTime.addAndGet(1234L); //the amount here is arbitrary, just to see that adjustment will still occur
		final ElapsingTime newElapsingTime = elapsingTime.withElapsed(Duration.ofSeconds(54321));
		assertThat("New elapsing time has correct time.", newElapsingTime.get(SECONDS), is(54321L));
		assertThat("New elapsing time has same time suplier.", newElapsingTime.getTimeSupplier(), is(elapsingTime.getTimeSupplier()));
		assertThat("New elapsing time has same time unit.", newElapsingTime.getTimeUnit(), is(elapsingTime.getTimeUnit()));
	}

}
