/*
 * Copyright © 2013 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.*;

import org.junit.jupiter.api.Test;

import java.time.*;
import java.util.*;

/**
 * Tests of calendar utilities.
 * 
 * @author Garret Wilson
 * @see Calendars
 * 
 */
public class CalendarsTest {

	/** @see Calendars#getDayDifference(Calendar, Calendar) */
	@Test
	public void testGetDayDifference() {
		assertThat(Calendars.getDayDifference(GregorianCalendar.from(ZonedDateTime.of(LocalDate.of(2002, 03, 04).atStartOfDay(), ZoneOffset.UTC)),
				GregorianCalendar.from(ZonedDateTime.of(LocalDate.of(2002, 03, 04).atStartOfDay(), ZoneOffset.UTC))), is(0)); //same day
		assertThat(Calendars.getDayDifference(GregorianCalendar.from(ZonedDateTime.of(LocalDate.of(2002, 03, 04).atStartOfDay(), ZoneOffset.UTC)),
				GregorianCalendar.from(ZonedDateTime.of(LocalDate.of(2002, 03, 03).atStartOfDay(), ZoneOffset.UTC))), is(1)); //yesterday
		assertThat(Calendars.getDayDifference(GregorianCalendar.from(ZonedDateTime.of(LocalDate.of(2002, 03, 04).atStartOfDay(), ZoneOffset.UTC)),
				GregorianCalendar.from(ZonedDateTime.of(LocalDate.of(2001, 03, 04).atStartOfDay(), ZoneOffset.UTC))), is(365)); //a year ago
		assertThat(Calendars.getDayDifference(GregorianCalendar.from(ZonedDateTime.of(LocalDate.of(2001, 02, 03).atStartOfDay(), ZoneOffset.UTC)),
				GregorianCalendar.from(ZonedDateTime.of(LocalDate.of(2000, 02, 03).atStartOfDay(), ZoneOffset.UTC))), is(366)); //a leap year ago
	}

}
