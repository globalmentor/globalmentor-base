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

}
