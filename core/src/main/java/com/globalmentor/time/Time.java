/*
 * Copyright © 2011-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.time.Milliseconds.*;

import java.util.Date;

/**
 * Immutable class for working with time, which also contains constants and methods for working with time in general, without regard to leap years, time zones,
 * and the like.
 * 
 * @author Garret Wilson
 * @deprecated to be replaced with new Java time classes such as {@link java.time.Instant}.
 */
@Deprecated
public class Time extends Date {

	/** The granularity of time. */
	public enum Resolution {

		NANOSECONDS(-1000), MILLISECONDS(1), SECONDS(fromSeconds(1)), MINUTES(fromMinutes(1)), HOURS(fromHours(1)), DAYS(fromDays(1));

		private final long milliseconds;

		/** @return The number of milliseconds of this resolution. */
		public long getMilliseconds() {
			return milliseconds;
		}

		/**
		 * Constructor.
		 * @param milliseconds The number of milliseconds of this resolution.
		 */
		private Resolution(final long milliseconds) {
			this.milliseconds = milliseconds;
		}

	}

	/** The number of milliseconds in a second. */
	public static int MILLISECONDS_PER_SECOND = 1000;

	/** The number of seconds in a minute. */
	public static int SECONDS_PER_MINUTE = 60;

	/** The number of minutes in an hour. */
	public static int MINUTES_PER_HOUR = 60;

	/** The number of hours in a day. */
	public static int HOURS_PER_DAY = 24;

	/**
	 * Default constructor with the current time.
	 * @see System#currentTimeMillis()
	 */
	public Time() {
		this(System.currentTimeMillis());
	}

	/**
	 * Date constructor.
	 * 
	 * @param date The date containing the number of milliseconds past the epoch.
	 */
	public Time(final Date date) {
		this(date.getTime());
	}

	/**
	 * Milliseconds constructor.
	 * 
	 * @param milliseconds The number of milliseconds past the epoch.
	 */
	public Time(final long milliseconds) {
		super(milliseconds);
	}

	/**
	 * Adds a duration to this time.
	 * @param duration The duration to add.
	 * @return A time object representing this time plus the given duration.
	 * @throws NullPointerException if the given duration is <code>null</code>.
	 */
	public Time add(final Duration duration) {
		final long time = duration.getTime(); //if the duration is 0, we can just return this time
		return time == 0 ? this : new Time(getTime() + time);
	}

	/**
	 * Subtracts a duration from this time.
	 * @param duration The duration to subtract.
	 * @return A time object representing this time minus the given duration.
	 * @throws NullPointerException if the given duration is <code>null</code>.
	 */
	public Time subtract(final Duration duration) {
		final long time = duration.getTime(); //if the duration is 0, we can just return this time
		return time == 0 ? this : new Time(getTime() - time);
	}

	/**
	 * Subtracts a time from this time. This operation may result in a negative duration.
	 * @param time The time to subtract.
	 * @return The duration between the two times, that is, this time minus the given time.
	 * @throws NullPointerException if the given time is <code>null</code>.
	 */
	public Duration subtract(final Time time) {
		return Duration.of(getTime() - time.getTime());
	}

	/**
	 * Retrieves the ceiling of this time by the given resolution. A resolution of {@link Resolution#MILLISECONDS} will simply return a time object equal to this
	 * time object. A resolution of {@link Resolution#SECONDS}, for example, will effectively drop the milliseconds, rounding down to seconds.
	 * @param resolution The resolution at which rounding should occur.
	 * @return A time instance indicating the ceiling time at the specified resolution.
	 * @see Math#ceil(double)
	 */
	public Time ceil(final Resolution resolution) {
		if(resolution == Resolution.MILLISECONDS) {
			return this;
		}
		return new Time((long)Math.ceil(getTime() / resolution.getMilliseconds()) * resolution.getMilliseconds());
	}

	/**
	 * Retrieves the floor of this time by the given resolution. A resolution of {@link Resolution#MILLISECONDS} will simply return a time object equal to this
	 * time object. A resolution of {@link Resolution#SECONDS}, for example, will effectively drop the milliseconds, rounding down to seconds.
	 * @param resolution The resolution at which rounding should occur.
	 * @return A time instance indicating the floor time at the specified resolution.
	 * @see Math#floor(double)
	 */
	public Time floor(final Resolution resolution) {
		if(resolution == Resolution.MILLISECONDS) {
			return this;
		}
		return new Time((long)Math.floor(getTime() / resolution.getMilliseconds()) * resolution.getMilliseconds());
	}

	/**
	 * Rounds this time at the given resolution. A resolution of {@link Resolution#MILLISECONDS} will simply return a time object equal to this time object. A
	 * resolution of {@link Resolution#SECONDS}, for example, will effectively drop the milliseconds, rounding down to seconds.
	 * @param resolution The resolution at which rounding should occur.
	 * @return A time instance indicating the rounded time at the specified resolution.
	 * @see Math#round(double)
	 */
	public Time round(final Resolution resolution) {
		if(resolution == Resolution.MILLISECONDS) {
			return this;
		}
		return new Time((long)Math.round(getTime() / resolution.getMilliseconds()) * resolution.getMilliseconds());
	}

	/** {@inheritDoc} This version adds the current time in milliseconds. */
	@Override
	public String toString() {
		return super.toString() + " (" + getTime() + ")";
	}
}
