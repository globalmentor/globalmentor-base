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

package com.globalmentor.iso.datetime;

import java.util.*;

import com.globalmentor.model.*;

import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.model.Count.*;
import static java.util.Calendar.*;

/**
 * Utilities for working with ISO dates.
 * @author Garret Wilson
 */
public class ISODates
{

	/**
	 * Calculates the counts of calendar days before that intersect a set of ranges.
	 * @param ranges The ranges used for counting.
	 * @return A map of all calendar days and the corresponding total number of intersections for each with the ranges.
	 * @throws IllegalArgumentException if the lower bound of one of the ranges is above its upper bound.
	 */
	public static SortedMap<ISODate, Count> getDayCounts(final Set<Range<ISODate>> ranges)
	{
		final SortedMap<ISODate, Count> dayCounts = new TreeMap<ISODate, Count>();
		for(final Range<ISODate> range : ranges) //fill the day counts from the ranges
		{
			final GregorianCalendar rangeCalendar = range.getLowerBound().toCalendar(); //start at the bottom of the range
			final GregorianCalendar rangeUpperBound = range.getUpperBound().toCalendar();
			checkArgument(rangeCalendar.compareTo(rangeUpperBound) <= 0, "Calendar range {0} cannot be greater than {1}.", rangeCalendar, rangeUpperBound);
			while(rangeCalendar.compareTo(rangeUpperBound) <= 0) //sweep the range until we go past the upper end of the range
			{
				incrementCounterMapCount(dayCounts, new ISODate(rangeCalendar));
				rangeCalendar.add(DAY_OF_YEAR, 1); //go to the next day in the range
			}
		}
		return dayCounts;
	}

	/**
	 * Calculates the totals for a particular date and a number of calendar days before that date.
	 * <p>
	 * For example, passing a calendar date of 2000-01-01 with a day count of 365, will return, for each day in the previous year, the number of days that
	 * intersect one of the ranges.
	 * </p>
	 * @param calendar The current calendar to use for calculations.
	 * @param windowSize The number of days back to include in each total.
	 * @param ranges The ranges used for intersection.
	 * @return A map of all calendar days and the total number of intersection with the ranges within that period.
	 */
	public static SortedMap<ISODate, Long> getDayTotals(final ISODate date, final int windowSize, final Map<ISODate, Count> dayCounts)
	{
		return getDayTotals(date, windowSize, windowSize, dayCounts);
	}

	/**
	 * Calculates the totals for a particular date and a number of calendar days before that date, including the given number of days in the history.
	 * <p>
	 * For example, passing a calendar date of 2000-01-01 with a window size of 365 and a history count of 730, will return, for 730 days prior to the given date,
	 * the number of days that intersect one of the ranges within the window of 365 days before each date.
	 * </p>
	 * @param calendar The current calendar to use for calculations.
	 * @param windowSize The number of days back to include in each total.
	 * @param historyCount The number of day totals to include.
	 * @param ranges The ranges used for intersection.
	 * @return A map of all calendar days and the total number of intersection with the ranges within the indicated history period.
	 */
	public static SortedMap<ISODate, Long> getDayTotals(final ISODate date, final int windowSize, final int historyCount, final Map<ISODate, Count> dayCounts)
	{
		final GregorianCalendar dayCalendar = date.toCalendar();
		final SortedMap<ISODate, Long> dayTotals = new TreeMap<ISODate, Long>();
		for(int i = 0; i < historyCount; ++i) //calculate all the day totals in the past
		{
			final GregorianCalendar totalCalendar = (GregorianCalendar)dayCalendar.clone(); //use a separate calendar to calculate the totals for this day
			long total = 0; //calculate the total for this date
			for(int j = 0; j < windowSize; ++j) //look at previous days relative to the current calendar date
			{
				final Count currentDayCount = dayCounts.get(new ISODate(totalCalendar)); //get the count for this day
				if(currentDayCount != null)
				{
					total += currentDayCount.getCount();
				}
				totalCalendar.add(DAY_OF_YEAR, -1); //go back a day and continue calculating the total
			}
			dayTotals.put(new ISODate(dayCalendar), Long.valueOf(total)); //store the total for this day
			dayCalendar.add(DAY_OF_YEAR, -1); //go back a day and calculate that day's total
		}
		return dayTotals;
	}

}
