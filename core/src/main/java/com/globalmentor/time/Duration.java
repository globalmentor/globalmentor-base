/*
 * Copyright © 2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import com.globalmentor.java.Longs;

/**
 * Indicates a duration of time in milliseconds. Negative durations are allowed.
 * 
 * @author Garret Wilson
 */
public class Duration implements Comparable<Duration> {

	/** The shared duration instance indicating a zero duration. */
	public static final Duration NO_DURATION = new Duration(0);

	/** The shared duration instance indicating the max possible duration. */
	public static final Duration MAX_DURATION = new Duration(Long.MAX_VALUE);

	/** The duration time in milliseconds. */
	private final long time;

	/** @return The duration time in milliseconds. */
	public long getTime() {
		return time;
	}

	/**
	 * Duration time constructor.
	 * @param time The duration time in milliseconds.
	 */
	private Duration(final long time) {
		this.time = time;
	}

	/**
	 * Duration time factory.
	 * @param time The duration time in milliseconds.
	 * @return A duration representing the given time.
	 */
	public static Duration of(final long time) {
		if(time == 0) {
			return NO_DURATION;
		}
		return new Duration(time);
	}

	/**
	 * Adds a duration to this duration.
	 * @param duration The duration to add.
	 * @return A duration object representing this duration plus the given duration.
	 * @throws NullPointerException if the given duration is <code>null</code>.
	 */
	public Duration add(final Duration duration) {
		final long time = duration.getTime(); //if the duration is 0, we can just return this duration
		return time == 0 ? this : Duration.of(getTime() + time);
	}

	/**
	 * Subtracts a duration from this duration.
	 * @param duration The duration to subtract.
	 * @return A duration object representing this duration minus the given duration.
	 * @throws NullPointerException if the given duration is <code>null</code>.
	 */
	public Duration subtract(final Duration duration) {
		final long time = duration.getTime(); //if the duration is 0, we can just return this duration
		return time == 0 ? this : Duration.of(getTime() - time);
	}

	@Override
	public int hashCode() {
		return Longs.hashCode(time);
	}

	@Override
	public boolean equals(final Object object) {
		if(object == this) {
			return true;
		}
		if(!(object instanceof Duration)) {
			return false;
		}
		return getTime() == ((Duration)object).getTime();
	}

	@Override
	public int compareTo(final Duration duration) {
		return Longs.compare(getTime(), duration.getTime());
	}

	@Override
	public String toString() {
		return String.valueOf(getTime());
	}
}
