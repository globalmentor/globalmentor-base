/*
 * Copyright © 2013 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.collections.Sets.immutableSetOf;
import static com.globalmentor.time.TimeZones.*;
import static org.hamcrest.Matchers.*;
import static org.junit.Assert.*;

import java.util.*;

import org.junit.Test;

import com.globalmentor.iso.datetime.ISODate;
import com.globalmentor.model.Range;

/**
 * Tests of calendar utilities.
 * 
 * @author Garret Wilson
 * @see Calendars
 * 
 */
public class CalendarsTest
{

	/** @see Calendars#getDayDifference(Calendar, Calendar) */
	@Test
	public void testGetDayDifference()
	{
		assertThat(Calendars.getDayDifference(new ISODate(2002, 03, 04).toCalendar(GMT), new ISODate(2002, 03, 04).toCalendar(GMT)), is(0)); //same day
		assertThat(Calendars.getDayDifference(new ISODate(2002, 03, 04).toCalendar(GMT), new ISODate(2002, 03, 03).toCalendar(GMT)), is(1)); //yesterday
		assertThat(Calendars.getDayDifference(new ISODate(2002, 03, 04).toCalendar(GMT), new ISODate(2001, 03, 04).toCalendar(GMT)), is(365)); //a year ago
		assertThat(Calendars.getDayDifference(new ISODate(2001, 02, 03).toCalendar(GMT), new ISODate(2000, 02, 03).toCalendar(GMT)), is(366)); //a leap year ago
	}

	/** @see Calendars#getDaysTotals(Calendar, int, Set) */
	@SuppressWarnings("unchecked")
	@Test
	public void testGetDayTotals()
	{
		Map<Calendar, Integer> dayTotals;
		//no ranges
		dayTotals = Calendars.getDaysTotals(new ISODate(2002, 03, 04).toCalendar(GMT), 365, Collections.<Range<Calendar>> emptySet());
		for(final Map.Entry<Calendar, Integer> dayTotalEntry : dayTotals.entrySet())
		{
			assertThat(dayTotalEntry.getValue(), is(0));
		}
		//a single day
		dayTotals = Calendars.getDaysTotals(new ISODate(2002, 03, 04).toCalendar(GMT), 365,
				immutableSetOf(new Range<Calendar>(new ISODate(2002, 03, 02).toCalendar(GMT), new ISODate(2002, 03, 02).toCalendar(GMT))));
		assertThat(dayTotals.get(new ISODate(2002, 03, 01).toCalendar(GMT)), is(0));
		assertThat(dayTotals.get(new ISODate(2002, 03, 02).toCalendar(GMT)), is(1));
		assertThat(dayTotals.get(new ISODate(2002, 03, 03).toCalendar(GMT)), is(1));
		//a whole year, one range
		dayTotals = Calendars.getDaysTotals(new ISODate(2002, 03, 04).toCalendar(GMT), 365,
				immutableSetOf(new Range<Calendar>(new ISODate(2001, 03, 05).toCalendar(GMT), new ISODate(2002, 03, 04).toCalendar(GMT))));
		assertThat(dayTotals.get(new ISODate(2001, 03, 05).toCalendar(GMT)), is(1));
		assertThat(dayTotals.get(new ISODate(2001, 03, 12).toCalendar(GMT)), is(8));
		assertThat(dayTotals.get(new ISODate(2002, 03, 04).toCalendar(GMT)), is(365));
	}

}
