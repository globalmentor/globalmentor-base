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

	/**Clears the date-related calendar fields:
	<ol>
		<li>{@link Calendar#ERA}</li>
		<li>{@link Calendar#YEAR}</li>
		<li>{@link Calendar#MONTH}</li>
		<li>{@link Calendar#WEEK_OF_YEAR}</li>
		<li>{@link Calendar#WEEK_OF_MONTH}</li>
		<li>{@link Calendar#DAY_OF_MONTH}</li>
		<li>{@link Calendar#DAY_OF_YEAR}</li>
		<li>{@link Calendar#DAY_OF_WEEK}</li>
		<li>{@link Calendar#DAY_OF_WEEK_IN_MONTH}</li>
	</ol>
	@param calendar The calendar the time of which to reset.
	@return The calendar being modified.
	@throws NullPointerException if the given calendar is <code>null</code>.
	*/
	public static Calendar clearDate(final Calendar calendar)
	{
		calendar.clear(ERA);	//clear the time-related fields
		calendar.clear(YEAR);
		calendar.clear(MONTH);
		calendar.clear(WEEK_OF_YEAR);
		calendar.clear(WEEK_OF_MONTH);
		calendar.clear(DAY_OF_MONTH);
		calendar.clear(DAY_OF_YEAR);
		calendar.clear(DAY_OF_WEEK);
		calendar.clear(DAY_OF_WEEK_IN_MONTH);
		return calendar;	//return the calendar
	}

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
	@throws NullPointerException if the given calendar is <code>null</code>.
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

	/**Sets the following time-related calendar fields from a given calendar:
	<ol>
		<li>{@link Calendar#HOUR_OF_DAY}</li>
		<li>{@link Calendar#MINUTE}</li>
		<li>{@link Calendar#SECOND}</li>
		<li>{@link Calendar#MILLISECOND}</li>
	</ol>
	@param calendar The calendar the time of which to set.
	@param fromCalendar The calendar from which to get the time.
	@throws NullPointerException if either of the given calendars is <code>null</code>.
	*/
	public static Calendar setTime(final Calendar calendar, final Calendar fromCalendar)
	{
		return setTime(calendar, fromCalendar.get(HOUR_OF_DAY), fromCalendar.get(MINUTE), fromCalendar.get(SECOND), fromCalendar.get(MILLISECOND));	//set the time fields from the given calendar and return the modified calendar
	}

	/**Sets the time-related calendar fields:
	<ol>
		<li>{@link Calendar#HOUR_OF_DAY}</li>
		<li>{@link Calendar#MINUTE}</li>
		<li>{@link Calendar#SECOND}</li>
		<li>{@link Calendar#MILLISECOND}</li>
	</ol>
	@param calendar The calendar the time of which to set.
	@param hour The hour of the day.
	@param minute The minute of the hour.
	@param second The second of the minute.
	@param millisecond The millisecond of the second.
	@return The calendar being modified.
	@throws NullPointerException if the given calendar is <code>null</code>.
	*/
	public static Calendar setTime(final Calendar calendar, final int hour, final int minute, final int second, final int millisecond)
	{
		calendar.set(HOUR_OF_DAY, hour);	//clear the time-related fields
		calendar.set(MINUTE, minute);
		calendar.set(SECOND, second);
		calendar.set(MILLISECOND, millisecond);
		return calendar;	//return the calendar
	}

}
