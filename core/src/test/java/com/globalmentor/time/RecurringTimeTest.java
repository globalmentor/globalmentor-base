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

package com.globalmentor.time;

import static java.util.concurrent.TimeUnit.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link RecurringTime}.
 * @author Garret Wilson
 */
public class RecurringTimeTest {

	//TODO write high-level unit tests

	//## recur strategies

	@Test
	void testRecurStrategyByInterval() {
		final long startTime = 1234;
		final AtomicLong fakeTime = new AtomicLong(startTime);
		final TimeUnit timeUnit = SECONDS;
		final ElapsingTime elapsingTime = ElapsingTime.fromNow(fakeTime::get, timeUnit);
		final RecurringTime.RecurStrategy strategy = RecurringTime.AbstractElapsedTimeRecurStrategy.byInterval(5);
		assertThat(strategy.shouldRecur(elapsingTime), is(false));
		assertThat("Recurs starting now.", strategy.recur(elapsingTime), is(ElapsingTime.since(fakeTime.get(), fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 1);
		assertThat(strategy.shouldRecur(elapsingTime), is(false));
		assertThat("Recurs starting now.", strategy.recur(elapsingTime), is(ElapsingTime.since(fakeTime.get(), fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 4);
		assertThat(strategy.shouldRecur(elapsingTime), is(false));
		assertThat("Recurs starting now.", strategy.recur(elapsingTime), is(ElapsingTime.since(fakeTime.get(), fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 5);
		assertThat(strategy.shouldRecur(elapsingTime), is(true));
		assertThat("Recurs starting now.", strategy.recur(elapsingTime), is(ElapsingTime.since(fakeTime.get(), fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 6);
		assertThat(strategy.shouldRecur(elapsingTime), is(true));
		assertThat("Recurs starting now.", strategy.recur(elapsingTime), is(ElapsingTime.since(fakeTime.get(), fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 9);
		assertThat(strategy.shouldRecur(elapsingTime), is(true));
		assertThat("Recurs starting now.", strategy.recur(elapsingTime), is(ElapsingTime.since(fakeTime.get(), fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 10);
		assertThat(strategy.shouldRecur(elapsingTime), is(true));
		assertThat("Recurs starting now.", strategy.recur(elapsingTime), is(ElapsingTime.since(fakeTime.get(), fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 11);
		assertThat(strategy.shouldRecur(elapsingTime), is(true));
		assertThat("Recurs starting now.", strategy.recur(elapsingTime), is(ElapsingTime.since(fakeTime.get(), fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 15);
		assertThat(strategy.shouldRecur(elapsingTime), is(true));
		assertThat("Recurs starting now.", strategy.recur(elapsingTime), is(ElapsingTime.since(fakeTime.get(), fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 17);
		assertThat(strategy.shouldRecur(elapsingTime), is(true));
		assertThat("Recurs starting now.", strategy.recur(elapsingTime), is(ElapsingTime.since(fakeTime.get(), fakeTime::get, timeUnit)));
	}

	@Test
	void testRecurStrategyByPeriod() {
		final long startTime = 1234;
		final AtomicLong fakeTime = new AtomicLong(startTime);
		final TimeUnit timeUnit = SECONDS;
		final ElapsingTime elapsingTime = ElapsingTime.fromNow(fakeTime::get, timeUnit);
		final RecurringTime.RecurStrategy strategy = RecurringTime.AbstractElapsedTimeRecurStrategy.byPeriod(5);
		assertThat(strategy.shouldRecur(elapsingTime), is(false));
		assertThat("Recurs starting at last period.", strategy.recur(elapsingTime), is(ElapsingTime.since(startTime, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 1);
		assertThat(strategy.shouldRecur(elapsingTime), is(false));
		assertThat("Recurs starting at last period.", strategy.recur(elapsingTime), is(ElapsingTime.since(startTime, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 4);
		assertThat(strategy.shouldRecur(elapsingTime), is(false));
		assertThat("Recurs starting at last period.", strategy.recur(elapsingTime), is(ElapsingTime.since(startTime, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 5);
		assertThat(strategy.shouldRecur(elapsingTime), is(true));
		assertThat("Recurs starting at last period.", strategy.recur(elapsingTime), is(ElapsingTime.since(startTime + 5, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 6);
		assertThat(strategy.shouldRecur(elapsingTime), is(true));
		assertThat("Recurs starting at last period.", strategy.recur(elapsingTime), is(ElapsingTime.since(startTime + 5, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 9);
		assertThat(strategy.shouldRecur(elapsingTime), is(true));
		assertThat("Recurs starting at last period.", strategy.recur(elapsingTime), is(ElapsingTime.since(startTime + 5, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 10);
		assertThat(strategy.shouldRecur(elapsingTime), is(true));
		assertThat("Recurs starting at last period.", strategy.recur(elapsingTime), is(ElapsingTime.since(startTime + 10, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 11);
		assertThat(strategy.shouldRecur(elapsingTime), is(true));
		assertThat("Recurs starting at last period.", strategy.recur(elapsingTime), is(ElapsingTime.since(startTime + 10, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 15);
		assertThat(strategy.shouldRecur(elapsingTime), is(true));
		assertThat("Recurs starting at last period.", strategy.recur(elapsingTime), is(ElapsingTime.since(startTime + 15, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 17);
		assertThat(strategy.shouldRecur(elapsingTime), is(true));
		assertThat("Recurs starting at last period.", strategy.recur(elapsingTime), is(ElapsingTime.since(startTime + 15, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 5678);
		assertThat(strategy.shouldRecur(elapsingTime), is(true));
		assertThat("Recurs starting at last period.", strategy.recur(elapsingTime), is(ElapsingTime.since(startTime + 5675, fakeTime::get, timeUnit)));
	}

}
