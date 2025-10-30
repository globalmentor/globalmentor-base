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

import static com.globalmentor.java.Conditions.checkArgument;
import static java.util.Objects.*;
import static java.util.concurrent.TimeUnit.*;

import java.time.Duration;
import java.util.concurrent.TimeUnit;
import java.util.function.*;

import org.jspecify.annotations.*;

/**
 * Lightweight class for measuring elapsing time.
 * @apiNote This is a thin wrapper over calling {@link System#nanoTime()} and performing subtraction to measure elapsed time as needed. It is is meant to
 *          replace boilerplate and reduce the risk of errors through repetition; it is not intended to compete with more full-function classes that allow
 *          starting and stopping counting, or provide new pluggable composite types for mocking time sources (although this class does use a general
 *          {@link Supplier} of time).
 * @implSpec This implementation considers two instances equal if they have the same start time and time unit, without regard to the time supplier, which is
 *           considered an implementation detail.
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

	/** @return The time source for measuring elapsed time since the start time, in terms of {@link #getTimeUnit()}. */
	LongSupplier getTimeSupplier() {
		return timeSupplier;
	}

	private final TimeUnit timeUnit;

	/** @return The time unit this instant is using to track time. */
	TimeUnit getTimeUnit() {
		return timeUnit;
	}

	/**
	 * Start time, supplier, and unit constructor.
	 * @param startTime The start time in the given unit and in terms of the given time supplier.
	 * @param timeSupplier The time source for measuring elapsed time since the start time, in the indicated unit.
	 * @param timeUnit The unit in which time is measured.
	 */
	private ElapsingTime(final long startTime, @NonNull final LongSupplier timeSupplier, @NonNull final TimeUnit timeUnit) {
		this.startTime = startTime;
		this.timeSupplier = requireNonNull(timeSupplier);
		this.timeUnit = requireNonNull(timeUnit);
	}

	/**
	 * Creates a means of measuring elapsed time since the given time.
	 * @implSpec This method uses {@link System#nanoTime()} as the source of nanosecond time.
	 * @param startNanoTime The start time in nanoseconds, in terms of {@link System#nanoTime()}.
	 * @return A new elapsing time measurer elapsing since the given start time.
	 * @see #fromNow()
	 */
	public static ElapsingTime sinceNanoTime(final long startNanoTime) {
		return since(startNanoTime, System::nanoTime, NANOSECONDS);
	}

	/**
	 * Creates a means of measuring elapsed time since the given time in the given unit, using the given time supplier.
	 * @param startTime The start time in the given unit and in terms of the time supplier.
	 * @param timeSupplier The time source for measuring elapsed time since the start time.
	 * @param timeUnit The unit in which time is measured.
	 * @return A new elapsing time measurer elapsing since the given start time.
	 * @see #fromNow(LongSupplier, TimeUnit)
	 */
	public static ElapsingTime since(final long startTime, @NonNull final LongSupplier timeSupplier, @NonNull final TimeUnit timeUnit) {
		return new ElapsingTime(startTime, timeSupplier, timeUnit);
	}

	/**
	 * Creates a means of measuring elapsed time from the current time.
	 * @apiNote This method is analogous to {@link #sinceNanoTime(long)}, and is named "from" because "since" typically designates a starting time in the past.
	 * @implSpec This method uses {@link System#nanoTime()} as the source of nanosecond time.
	 * @return A new elapsing time measurer elapsing from now.
	 */
	public static ElapsingTime fromNow() {
		return fromNow(System::nanoTime, NANOSECONDS);
	}

	/**
	 * Copy static factory "wither" method that creates a new elapsing time from the current time, using the same time supplier and time unit as this elapsing
	 * time instance.
	 * @apiNote This method is equivalent to calling {@link #withElapsed(Duration)} with {@link Duration#ZERO}.
	 * @implSpec This implementation delegates to {@link #fromNow(LongSupplier, TimeUnit)}.
	 * @return A new elapsing time measurer elapsing from now.
	 */
	public ElapsingTime withElapsingFromNow() {
		return fromNow(timeSupplier, getTimeUnit());
	}

	/**
	 * Copy static factory "wither" method that creates a new elapsing time from the current time plus or minus the given elapsing time, using the same time
	 * supplier and time unit as this elapsing time instance.
	 * @apiNote The provided elapsed time can be positive, negative, or even zero. A positive elapsed time indicates that a the resulting elapsed time <em>has
	 *          already started elapsing in the past</em>, at a time equal to now <em>minus</em> the given positive elapsed time. A negative elapsed time produces
	 *          an elapsing time that has not yet started elapsing, and which will produce negative elapsing time durations until the given elapsed time has
	 *          passed.
	 * @implSpec This implementation delegates to {@link #since(long, LongSupplier, TimeUnit)}.
	 * @param elapsed The elapsed of time the resulting elapsing time will conceptually have already elapsing when returned, using the time unit of this elapsing
	 *          time.
	 * @return A new elapsing time measurer that conceptually has already been elapsing since the given elapsed time.
	 * @see #getTimeUnit()
	 */
	ElapsingTime withElapsed(@NonNull final long elapsed) {
		return since(timeSupplier.getAsLong() - elapsed, timeSupplier, getTimeUnit());
	}

	/**
	 * Copy static factory "wither" method that creates a new elapsing time from the current time plus or minus the given duration, using the same time supplier
	 * and time unit as this elapsing time instance.
	 * @apiNote The provided duration can be positive, negative, or even {@link Duration#ZERO}. A positive duration indicates that a the resulting elapsed time
	 *          <em>has already started elapsing in the past</em>, at a time equal to now <em>minus</em> the given positive duration. A negative duration produces
	 *          an elapsing time that has not yet started elapsing, and which will produce negative elapsing time durations until the given duration has passed.
	 * @implSpec This implementation delegates to {@link #withElapsed(long)}.
	 * @implNote This method will also throw an {@link IllegalArgumentException} if an intermediate conversion causes an overflow, which will be mitigated in Java
	 *           11.
	 * @param duration The duration of time the resulting elapsing time will conceptually have already elapsing when returned.
	 * @return A new elapsing time measurer that conceptually has already been elapsing since the given duration.
	 * @throws IllegalArgumentException if the duration would overflow when converting to the unit of this elapsing time.
	 */
	public ElapsingTime withElapsed(@NonNull final Duration duration) {
		final long durationInTimeUnit = getTimeUnit().convert(duration);
		checkArgument(durationInTimeUnit != Long.MIN_VALUE && durationInTimeUnit != Long.MAX_VALUE, "Duration %s will cause an overflow.", duration);
		return withElapsed(durationInTimeUnit);
	}

	/**
	 * Creates a means of measuring elapsed time from the current time, using the given time supplier.
	 * @apiNote This method is analogous to {@link #since(long, LongSupplier, TimeUnit)}, and is named "from" because "since" typically designates a starting time
	 *          in the past.
	 * @implSpec This implementation will use the value retrieved from the time supplier as the start time.
	 * @param timeSupplier The time source for measuring elapsed time since the start time.
	 * @param timeUnit The unit in which time is measured.
	 * @return A new elapsing time measurer.
	 */
	public static ElapsingTime fromNow(@NonNull final LongSupplier timeSupplier, @NonNull final TimeUnit timeUnit) {
		return new ElapsingTime(timeSupplier.getAsLong(), timeSupplier, timeUnit);
	}

	/** @return The amount of time elapsed until now in the time unit of record. */
	long getElapsedTime() {
		return timeSupplier.getAsLong() - startTime;
	}

	/**
	 * Retrieves the time elapsed until now in a specified unit.
	 * @param asTimeUnit The unit in which elapsed time is to be returned.
	 * @return The amount of time elapsed until the current time in the requested unit.
	 */
	public long get(@NonNull final TimeUnit asTimeUnit) {
		return asTimeUnit.convert(getElapsedTime(), getTimeUnit());
	}

	/**
	 * Returns the time elapsed until the current time.
	 * @return The time elapsed until the current time.
	 * @throws ArithmeticException if a numeric overflow occurs.
	 */
	public Duration get() {
		return Duration.of(getElapsedTime(), getTimeUnit().toChronoUnit());
	}

	/**
	 * Converts a duration to the time units of this elapsing time.
	 * @param duration The duration to convert.
	 * @return The duration in terms of this elapsing time unit.
	 * @see #getTimeUnit()
	 */
	long convertToTimeUnit(@NonNull final Duration duration) {
		return duration.get(getTimeUnit().toChronoUnit());
	}

	@Override
	public int hashCode() {
		return hash(startTime, timeUnit);
	}

	@Override
	public boolean equals(final Object object) {
		if(this == object) {
			return true;
		}
		if(!(object instanceof ElapsingTime)) {
			return false;
		}
		final ElapsingTime elapsingTime = (ElapsingTime)object;
		return startTime == elapsingTime.startTime && getTimeUnit().equals(elapsingTime.getTimeUnit());
	}

	@Override
	public String toString() {
		return "…" + get().toString() + "…";
	}

	/**
	 * Converts a {@link TimeUnit} to the equivalent {@link ChronoUnit}.
	 * @apiNote This method is equivalent to the similarly-named {@link TimeUnit} method introduced in Java 9. This method will be deprecated and removed once
	 *          this library is upgraded to require Java 9+.
	 * @param timeUnit Some time unit.
	 * @return The converted {@link ChronoUnit} equivalent to the given {@link TimeUnit}.
	 */
	//TODO delete	
	//	static ChronoUnit toChronoUnit(@NonNull final TimeUnit timeUnit) { //TODO switch to `timeUnit.toChronoUnit()` in Java 9+
	//		switch(timeUnit) {
	//			case NANOSECONDS:
	//				return ChronoUnit.NANOS;
	//			case MICROSECONDS:
	//				return ChronoUnit.MICROS;
	//			case MILLISECONDS:
	//				return ChronoUnit.MILLIS;
	//			case SECONDS:
	//				return ChronoUnit.SECONDS;
	//			case MINUTES:
	//				return ChronoUnit.MINUTES;
	//			case HOURS:
	//				return ChronoUnit.HOURS;
	//			case DAYS:
	//				return ChronoUnit.DAYS;
	//			default:
	//				throw new AssertionError();
	//		}
	//	}

}
