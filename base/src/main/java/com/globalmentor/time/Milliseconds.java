/*
 * Copyright © 2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.time.Time.*;

/**
 * Utility class containing helper methods for working with time in the form of milliseconds.
 * 
 * @author Garret Wilson
 */
public class Milliseconds {

	/**
	 * Determines the number of milliseconds from the given number of seconds.
	 * @param seconds The number of seconds to convert.
	 * @return The number of milliseconds for the given number of seconds.
	 * @see Time#MILLISECONDS_PER_SECOND
	 */
	public static long fromSeconds(final long seconds) {
		return seconds * MILLISECONDS_PER_SECOND;
	}

	/**
	 * Determines the number of milliseconds from the given number of minutes.
	 * @param minutes The number of minutes to convert.
	 * @return The number of milliseconds for the given number of minutes.
	 * @see Time#SECONDS_PER_MINUTE
	 */
	public static long fromMinutes(final long minutes) {
		return fromSeconds(minutes * SECONDS_PER_MINUTE);
	}

	/**
	 * Determines the number of milliseconds from the given number of hours.
	 * @param hours The number of hours to convert.
	 * @return The number of milliseconds for the given number of hours.
	 * @see Time#MINUTES_PER_HOUR
	 */
	public static long fromHours(final long hours) {
		return fromMinutes(hours * MINUTES_PER_HOUR);
	}

	/**
	 * Determines the number of milliseconds from the given number of days.
	 * @param days The number of hours to convert.
	 * @return The number of milliseconds for the given number of days.
	 * @see Time#HOURS_PER_DAY
	 */
	public static long fromDays(final long days) {
		return fromHours(days * HOURS_PER_DAY);
	}

}
