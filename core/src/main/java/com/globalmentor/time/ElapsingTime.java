/*
 * Copyright © 2022 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.time;

import static java.util.Objects.*;
import static java.util.concurrent.TimeUnit.*;

import java.time.Duration;
import java.util.concurrent.TimeUnit;
import java.util.function.*;

import javax.annotation.*;

/**
 * Lightweight class for measuring elapsing time.
 * @apiNote This is a thin wrapper over calling {@link System#nanoTime()} and performing subtraction to measure elapsed time as needed. It is is meant to
 *          replace boilerplate and reduce the risk of errors through repetition; it is not intended to compete with more full-function classes that allow
 *          starting and stopping counting, or provide new pluggable composite types for mocking time sources (although this class does use a general
 *          {@link Supplier} of time).
 * @implSpec By default this class uses {@link System#nanoTime()} as its time supplier, but any time supplier can be used with any {@link TimeUnit}.
 * @author Garret Wilson
 * @see <a href="https://stackoverflow.com/q/180158">How do I time a method's execution in Java?</a>
 * @see <a href="https://guava.dev/releases/snapshot-jre/api/docs/com/google/common/base/Stopwatch.html">Guava <code>StopWatch</code></a>
 * @see <a href="https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/time/StopWatch.html">Apache Commons Lang
 *      <code>StopWatch</code></a>
 * @see System#nanoTime()
 */
public final class ElapsingTime {

	private final long startTime;

	private final LongSupplier timeSupplier;

	private final TimeUnit timeUnit;

	/**
	 * Start time, supplier, and unit constructor.
	 * @param startTime The start time in the given unit and in terms of the given time supplier.
	 * @param timeSupplier The time source for measuring elapsed time since the start time.
	 * @param timeUnit The unit in which time is measured.
	 */
	private ElapsingTime(final long startTime, @Nonnull final LongSupplier timeSupplier, @Nonnull final TimeUnit timeUnit) {
		this.startTime = startTime;
		this.timeSupplier = requireNonNull(timeSupplier);
		this.timeUnit = requireNonNull(timeUnit);
	}

	/**
	 * Creates a means of measuring elapsed time since the given time.
	 * @implSpec This method uses {@link System#nanoTime()} as the source of nanosecond time.
	 * @param startNanoTime The start time in nanoseconds, in terms of {@link System#nanoTime()}.
	 * @return A new elapsing time measurer.
	 */
	public static ElapsingTime sinceNanoTime(final long startNanoTime) {
		return since(startNanoTime, System::nanoTime, NANOSECONDS);
	}

	/**
	 * Creates a means of measuring elapsed time since the given time in the given unit, using the given time supplier.
	 * @param startTime The start time in the given unit and in terms of the time supplier.
	 * @param timeSupplier The time source for measuring elapsed time since the start time.
	 * @param timeUnit The unit in which time is measured.
	 * @return A new elapsing time measurer.
	 */
	public static ElapsingTime since(final long startTime, @Nonnull final LongSupplier timeSupplier, @Nonnull final TimeUnit timeUnit) {
		return new ElapsingTime(startTime, timeSupplier, timeUnit);
	}

	/**
	 * Creates a means of measuring elapsed time from the current time.
	 * @implSpec This method uses {@link System#nanoTime()} as the source of nanosecond time.
	 * @return A new elapsing time measurer.
	 */
	public static ElapsingTime sinceNow() {
		return sinceNow(System::nanoTime, NANOSECONDS);
	}

	/**
	 * Creates a means of measuring elapsed time from the current time, using the given time supplier.
	 * @implSpec This implementation will use the value retrieved from the time supplier as the start time.
	 * @param timeSupplier The time source for measuring elapsed time since the start time.
	 * @param timeUnit The unit in which time is measured.
	 * @return A new elapsing time measurer.
	 */
	public static ElapsingTime sinceNow(@Nonnull final LongSupplier timeSupplier, @Nonnull final TimeUnit timeUnit) {
		return new ElapsingTime(timeSupplier.getAsLong(), timeSupplier, timeUnit);
	}

	/** @return The amount of time elapsed until now in the time unit of record. */
	private long getElapsedTime() {
		return timeSupplier.getAsLong() - startTime;
	}

	/**
	 * Retrieves the time elapsed until now in a specified unit.
	 * @param asTimeUnit The unit in which elapsed time is to be returned.
	 * @return The amount of time elapsed until the current time in the requested unit.
	 */
	public long get(@Nonnull final TimeUnit asTimeUnit) {
		return asTimeUnit.convert(getElapsedTime(), timeUnit);
	}

	/** @return The time elapsed until the current time. */
	public Duration get() {
		return Duration.ofNanos(get(NANOSECONDS)); //TODO change to `Duration.of(getElapsedTime(), timeUnit.toChronoUnit())` in Java 9+
	}

}
