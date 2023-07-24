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
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.any;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link RecurringTime}.
 * @author Garret Wilson
 */
public class RecurringTimeTest {

	/**
	 * @see RecurringTime#pollRecurred()
	 * @see RecurringTime.RecurStrategy
	 */
	@Test
	void testPollRecurred() {
		final RecurringTime.RecurStrategy mockRecurStrategy = mock(RecurringTime.RecurStrategy.class);
		final RecurringTime testRecurringTime = new RecurringTime(ElapsingTime.fromNow(), mockRecurStrategy);
		when(mockRecurStrategy.recur(any(ElapsingTime.class))).thenReturn(mock(ElapsingTime.class));
		when(mockRecurStrategy.shouldRecur(any(ElapsingTime.class))).thenReturn(false);
		assertThat("Poll recurred returns false when not time to recur.", testRecurringTime.pollRecurred(), is(false));
		when(mockRecurStrategy.shouldRecur(any(ElapsingTime.class))).thenReturn(true);
		assertThat("Poll recurred returns true when time to recur.", testRecurringTime.pollRecurred(), is(true));
	}

	/**
	 * @see RecurringTime#getElapsingTime()
	 * @see RecurringTime.RecurStrategy
	 */
	@Test
	void testGetElapsingTime() {
		final RecurringTime.RecurStrategy mockRecurStrategy = mock(RecurringTime.RecurStrategy.class);
		final ElapsingTime startElapsingTime = ElapsingTime.fromNow();
		final RecurringTime testRecurringTime = new RecurringTime(startElapsingTime, mockRecurStrategy);
		final ElapsingTime mockRecurElapsingTime = mock(ElapsingTime.class);
		when(mockRecurStrategy.recur(any(ElapsingTime.class))).thenReturn(mockRecurElapsingTime);
		when(mockRecurStrategy.shouldRecur(any(ElapsingTime.class))).thenReturn(false);
		assertThat("Get elapsing time returns start elapsing time when not time to recur.", testRecurringTime.getElapsingTime(), is(startElapsingTime));
		when(mockRecurStrategy.shouldRecur(any(ElapsingTime.class))).thenReturn(true);
		assertThat("Get elapsing time returns recur elapsing time when time to recur.", testRecurringTime.getElapsingTime(), is(mockRecurElapsingTime));
	}

	//## recur strategies

	//### by interval

	/** @see RecurringTime.AbstractElapsedTimeRecurStrategy#byInterval(long) */
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

	/**
	 * @see RecurringTime#fromElapsingByInterval(ElapsingTime, long)
	 * @see RecurringTime#pollRecurred()
	 */
	@Test
	void testFromElapsingByIntervalPollRecurred() {
		final long startTime = 1234;
		final AtomicLong fakeTime = new AtomicLong(1234);
		final TimeUnit timeUnit = SECONDS;
		final ElapsingTime elapsingTime = ElapsingTime.fromNow(fakeTime::get, timeUnit);
		fakeTime.set(startTime + 2);
		final RecurringTime recurringTime = RecurringTime.fromElapsingByInterval(elapsingTime, 5);
		assertThat(recurringTime.pollRecurred(), is(false));
		assertThat(recurringTime.pollRecurred(), is(false));
		fakeTime.set(startTime + 4);
		assertThat(recurringTime.pollRecurred(), is(false));
		assertThat(recurringTime.pollRecurred(), is(false));
		fakeTime.set(startTime + 5);
		assertThat(recurringTime.pollRecurred(), is(true));
		assertThat("Poll state should revert.", recurringTime.pollRecurred(), is(false));
		fakeTime.set(startTime + 9);
		assertThat(recurringTime.pollRecurred(), is(false));
		assertThat(recurringTime.pollRecurred(), is(false));
		fakeTime.set(startTime + 10);
		assertThat(recurringTime.pollRecurred(), is(true));
		assertThat("Poll state should revert.", recurringTime.pollRecurred(), is(false));
		fakeTime.set(startTime + 11);
		assertThat(recurringTime.pollRecurred(), is(false));
		assertThat(recurringTime.pollRecurred(), is(false));
	}

	/**
	 * @see RecurringTime#fromElapsingByInterval(ElapsingTime, long)
	 * @see RecurringTime#getElapsingTime()
	 */
	@Test
	void testFromElapsingByIntervalGetElapsingTime() {
		final long startTime = 1234;
		final AtomicLong fakeTime = new AtomicLong(1234);
		final TimeUnit timeUnit = SECONDS;
		final ElapsingTime elapsingTime = ElapsingTime.fromNow(fakeTime::get, timeUnit);
		fakeTime.set(startTime + 2);
		final RecurringTime recurringTime = RecurringTime.fromElapsingByInterval(elapsingTime, 5);
		assertThat(recurringTime.getElapsingTime(), is(ElapsingTime.since(startTime, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 4);
		assertThat(recurringTime.getElapsingTime(), is(ElapsingTime.since(startTime, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 5);
		assertThat(recurringTime.getElapsingTime(), is(ElapsingTime.since(startTime + 5, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 9);
		assertThat(recurringTime.getElapsingTime(), is(ElapsingTime.since(startTime + 5, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 10);
		assertThat(recurringTime.getElapsingTime(), is(ElapsingTime.since(startTime + 10, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 11);
		assertThat(recurringTime.getElapsingTime(), is(ElapsingTime.since(startTime + 10, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 17);
		assertThat("By interval recurrence always starts at recur time.", recurringTime.getElapsingTime(),
				is(ElapsingTime.since(startTime + 17, fakeTime::get, timeUnit)));
	}

	//### by period

	/** @see RecurringTime.AbstractElapsedTimeRecurStrategy#byPeriod(long) */
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

	/**
	 * @see RecurringTime#fromElapsingByPeriod(ElapsingTime, long)
	 * @see RecurringTime#pollRecurred()
	 */
	@Test
	void testFromElapsingByPeriodPollRecurred() {
		final long startTime = 1234;
		final AtomicLong fakeTime = new AtomicLong(1234);
		final TimeUnit timeUnit = SECONDS;
		final ElapsingTime elapsingTime = ElapsingTime.fromNow(fakeTime::get, timeUnit);
		fakeTime.set(startTime + 2);
		final RecurringTime recurringTime = RecurringTime.fromElapsingByPeriod(elapsingTime, 5);
		assertThat(recurringTime.pollRecurred(), is(false));
		assertThat(recurringTime.pollRecurred(), is(false));
		fakeTime.set(startTime + 4);
		assertThat(recurringTime.pollRecurred(), is(false));
		assertThat(recurringTime.pollRecurred(), is(false));
		fakeTime.set(startTime + 5);
		assertThat(recurringTime.pollRecurred(), is(true));
		assertThat("Poll state should revert.", recurringTime.pollRecurred(), is(false));
		fakeTime.set(startTime + 9);
		assertThat(recurringTime.pollRecurred(), is(false));
		assertThat(recurringTime.pollRecurred(), is(false));
		fakeTime.set(startTime + 10);
		assertThat(recurringTime.pollRecurred(), is(true));
		assertThat("Poll state should revert.", recurringTime.pollRecurred(), is(false));
		fakeTime.set(startTime + 11);
		assertThat(recurringTime.pollRecurred(), is(false));
		assertThat(recurringTime.pollRecurred(), is(false));
		fakeTime.set(startTime + 17);
		assertThat(recurringTime.pollRecurred(), is(true));
		assertThat("Poll state should revert.", recurringTime.pollRecurred(), is(false));
	}

	/**
	 * @see RecurringTime#fromElapsingByPeriod(ElapsingTime, long)
	 * @see RecurringTime#getElapsingTime()
	 */
	@Test
	void testFromElapsingByPeriodGetElapsingTime() {
		final long startTime = 1234;
		final AtomicLong fakeTime = new AtomicLong(1234);
		final TimeUnit timeUnit = SECONDS;
		final ElapsingTime elapsingTime = ElapsingTime.fromNow(fakeTime::get, timeUnit);
		fakeTime.set(startTime + 2);
		final RecurringTime recurringTime = RecurringTime.fromElapsingByPeriod(elapsingTime, 5);
		assertThat(recurringTime.getElapsingTime(), is(ElapsingTime.since(startTime, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 4);
		assertThat(recurringTime.getElapsingTime(), is(ElapsingTime.since(startTime, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 5);
		assertThat(recurringTime.getElapsingTime(), is(ElapsingTime.since(startTime + 5, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 9);
		assertThat(recurringTime.getElapsingTime(), is(ElapsingTime.since(startTime + 5, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 10);
		assertThat(recurringTime.getElapsingTime(), is(ElapsingTime.since(startTime + 10, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 11);
		assertThat(recurringTime.getElapsingTime(), is(ElapsingTime.since(startTime + 10, fakeTime::get, timeUnit)));
		fakeTime.set(startTime + 17);
		assertThat("By period recurrence starts at last period recur time.", recurringTime.getElapsingTime(),
				is(ElapsingTime.since(startTime + 15, fakeTime::get, timeUnit)));
	}

}
