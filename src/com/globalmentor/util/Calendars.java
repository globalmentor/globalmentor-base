/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.util;

import java.util.Calendar;

import static java.util.Calendar.*;

/**Constants and utilities for working with dates and times.
@author Garret Wilson
*/
public class Calendars
{

	/**The number of days in a week.*/
	public final static int WEEK_DAY_COUNT=7;

	/**Clears the time-related calendar fields:
	<ol>
		<li>{@link Calendar#HOUR_OF_DAY}</li>
		<li>{@link Calendar#HOUR}</li>
		<li>{@link Calendar#AM_PM}</li>
		<li>{@link Calendar#MINUTE}</li>
		<li>{@link Calendar#SECOND}</li>
		<li>{@link Calendar#MILLISECOND}</li>
	</ol>
	@param calendar The calendar the time of which to reset.
	@return The calendar being modified.
	*/
	public static Calendar clearTime(final Calendar calendar)
	{
		calendar.clear(HOUR_OF_DAY);	//clear the time-related fields
		calendar.clear(HOUR);	//clear the 12-hour time field as well, because otherwise it may be used if the hour-of-day field is unset
		calendar.clear(AM_PM);	//clear the AM/PM designation just to be sure and to be thorough
		calendar.clear(MINUTE);
		calendar.clear(SECOND);
		calendar.clear(MILLISECOND);
		return calendar;	//return the calendar
	}

}
