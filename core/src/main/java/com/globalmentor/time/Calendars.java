/*
 * Copyright © 1996-2013 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.util.*;

import static com.globalmentor.java.Longs.*;
import static java.util.Calendar.*;
import static java.util.concurrent.TimeUnit.*;

/**
 * Constants and utilities for working with dates and times.
 * @author Garret Wilson
 */
public class Calendars {

	/** The number of days in a week. */
	public static final int WEEK_DAY_COUNT = 7;

	/**
	 * Adds or subtracts the specified amount of time to the given calendar field, based on the calendar's rules, and returns the given calendar.
	 * <p>
	 * This is equivalent to calling {@link Calendar#add(int, int)} and returning the given calendar.
	 * </p>
	 * @param <C> The type of the calendar.
	 * @param calendar The calendar to which to add the value.
	 * @param field The calendar field.
	 * @param amount The amount of date or time to be added to the field.
	 * @return The calendar after the modifications on the given amount of time.
	 * @see Calendar#add(int, int)
	 */
	public static <C extends Calendar> C add(final C calendar, final int field, final int amount) {
		calendar.add(field, amount);
		return calendar;
	}

	/**
	 * Clears the date-related calendar fields:
	 * <ol>
	 * <li>{@link Calendar#ERA}</li>
	 * <li>{@link Calendar#YEAR}</li>
	 * <li>{@link Calendar#MONTH}</li>
	 * <li>{@link Calendar#WEEK_OF_YEAR}</li>
	 * <li>{@link Calendar#WEEK_OF_MONTH}</li>
	 * <li>{@link Calendar#DAY_OF_MONTH}</li>
	 * <li>{@link Calendar#DAY_OF_YEAR}</li>
	 * <li>{@link Calendar#DAY_OF_WEEK}</li>
	 * <li>{@link Calendar#DAY_OF_WEEK_IN_MONTH}</li>
	 * </ol>
	 * @param <C> The type of the calendar.
	 * @param calendar The calendar the time of which to reset.
	 * @return The calendar being modified.
	 * @throws NullPointerException if the given calendar is <code>null</code>.
	 */
	public static <C extends Calendar> C clearDate(final C calendar) {
		calendar.clear(ERA); //clear the time-related fields
		calendar.clear(YEAR);
		calendar.clear(MONTH);
		calendar.clear(WEEK_OF_YEAR);
		calendar.clear(WEEK_OF_MONTH);
		calendar.clear(DAY_OF_MONTH);
		calendar.clear(DAY_OF_YEAR);
		calendar.clear(DAY_OF_WEEK);
		calendar.clear(DAY_OF_WEEK_IN_MONTH);
		return calendar; //return the calendar
	}

	/**
	 * Clears the time-related calendar fields:
	 * <ol>
	 * <li>{@link Calendar#HOUR_OF_DAY}</li>
	 * <li>{@link Calendar#HOUR}</li>
	 * <li>{@link Calendar#AM_PM}</li>
	 * <li>{@link Calendar#MINUTE}</li>
	 * <li>{@link Calendar#SECOND}</li>
	 * <li>{@link Calendar#MILLISECOND}</li>
	 * </ol>
	 * @param <C> The type of the calendar.
	 * @param calendar The calendar the time of which to reset.
	 * @return The calendar being modified.
	 * @throws NullPointerException if the given calendar is <code>null</code>.
	 */
	public static <C extends Calendar> C clearTime(final C calendar) {
		calendar.clear(HOUR_OF_DAY); //clear the time-related fields
		calendar.clear(HOUR); //clear the 12-hour time field as well, because otherwise it may be used if the hour-of-day field is unset
		calendar.clear(AM_PM); //clear the AM/PM designation just to be sure and to be thorough
		calendar.clear(MINUTE);
		calendar.clear(SECOND);
		calendar.clear(MILLISECOND);
		return calendar; //return the calendar
	}

	/**
	 * Sets the following time-related calendar fields from a given calendar:
	 * <ol>
	 * <li>{@link Calendar#HOUR_OF_DAY}</li>
	 * <li>{@link Calendar#MINUTE}</li>
	 * <li>{@link Calendar#SECOND}</li>
	 * <li>{@link Calendar#MILLISECOND}</li>
	 * </ol>
	 * @param <C> The type of the calendar.
	 * @param calendar The calendar the time of which to set.
	 * @param fromCalendar The calendar from which to get the time.
	 * @return The calendar after the time being set up.
	 * @throws NullPointerException if either of the given calendars is <code>null</code>.
	 */
	public static <C extends Calendar> C setTime(final C calendar, final Calendar fromCalendar) {
		return setTime(calendar, fromCalendar.get(HOUR_OF_DAY), fromCalendar.get(MINUTE), fromCalendar.get(SECOND), fromCalendar.get(MILLISECOND)); //set the time fields from the given calendar and return the modified calendar
	}

	/**
	 * Sets the date and time-related fields, including milliseconds.
	 * @param <C> The type of the calendar.
	 * @param calendar The calendar to be set up the date time.
	 * @param year The year.
	 * @param month The zero-based month.
	 * @param date The day of the month.
	 * @param hour The hour of the day.
	 * @param minute The minute of the hour.
	 * @param second The second of the minute.
	 * @param millisecond The millisecond of the second.
	 * @return The calendar being modified.
	 * @throws NullPointerException if the given calendar is <code>null</code>.
	 * @see Calendar#set(int, int, int, int, int, int)
	 */
	public static <C extends Calendar> C setDateTime(final C calendar, final int year, final int month, final int date, final int hour, final int minute,
			final int second, final int millisecond) {
		calendar.set(year, month, date, hour, minute, second); //set the calendar
		calendar.set(MILLISECOND, millisecond); //set the milliseconds
		return calendar; //return the calendar
	}

	/**
	 * Sets the time-related calendar fields:
	 * <ol>
	 * <li>{@link Calendar#HOUR_OF_DAY}</li>
	 * <li>{@link Calendar#MINUTE}</li>
	 * <li>{@link Calendar#SECOND}</li>
	 * <li>{@link Calendar#MILLISECOND}</li>
	 * </ol>
	 * @param <C> The type of the calendar.
	 * @param calendar The calendar the time of which to set.
	 * @param hour The hour of the day.
	 * @param minute The minute of the hour.
	 * @param second The second of the minute.
	 * @param millisecond The millisecond of the second.
	 * @return The calendar being modified.
	 * @throws NullPointerException if the given calendar is <code>null</code>.
	 */
	public static <C extends Calendar> C setTime(final C calendar, final int hour, final int minute, final int second, final int millisecond) {
		calendar.set(HOUR_OF_DAY, hour); //clear the time-related fields
		calendar.set(MINUTE, minute);
		calendar.set(SECOND, second);
		calendar.set(MILLISECOND, millisecond);
		return calendar; //return the calendar
	}

	/**
	 * Determines the difference between the two calendars. This is equivalent conceptually to <code>calendar1Days - calendar2Days</code>. If the first calendar
	 * date is smaller than the second, a negative number will be returned. The time zones of each calendar are left as they are.
	 * <p>
	 * This method correctly works with leap days.
	 * </p>
	 * @param calendar1 The first calendar of the difference.
	 * @param calendar2 The first calendar of the difference.
	 * @return The difference between the calendars, in days.
	 * @throws IllegalArgumentException if the number of days between the calendars would be greater than an integer.
	 */
	public static int getDayDifference(final Calendar calendar1, final Calendar calendar2) {
		final boolean isNegative = calendar1.compareTo(calendar2) < 0;
		final Calendar lowCalendar, highCalendar;
		if(isNegative) { //arrange copies of the calendars in order from largest to smallest
			lowCalendar = (Calendar)calendar1.clone();
			highCalendar = (Calendar)calendar2.clone();
		} else {
			highCalendar = (Calendar)calendar1.clone();
			lowCalendar = (Calendar)calendar2.clone();
		}
		clearTime(lowCalendar); //clear the time values, just keeping the dates
		clearTime(highCalendar);
		//divide the difference by the number of milliseconds in one day
		int dayDifference = toInt((calendar1.getTimeInMillis() - calendar2.getTimeInMillis()) / DAYS.toMillis(1));
		dayDifference = Math.min(dayDifference - 2, 0); //back up a couple of days to account for
		assert add((Calendar)lowCalendar.clone(), DAY_OF_YEAR, dayDifference).compareTo(highCalendar) <= 0 : "Day overshoot.";
		while(add((Calendar)lowCalendar.clone(), DAY_OF_YEAR, dayDifference).compareTo(highCalendar) < 0) { //go forward until adding the number of days reaches the end calendar 
			++dayDifference;
		}
		return isNegative ? -dayDifference : dayDifference; //account for calendar order
	}

}
