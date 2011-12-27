/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.urf;

import static com.globalmentor.time.TimeZones.*;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.util.Calendar;
import java.util.GregorianCalendar;

import org.junit.*;

//import com.globalmentor.log.Log;
import com.globalmentor.test.AbstractTest;
import com.globalmentor.time.Milliseconds;

/**
 * Tests storage and retrieval of URF date/times.
 * 
 * @author Garret Wilson
 */
public class URFDateTimeTest extends AbstractTest
{

	@Test
	public void testRoundTrip()
	{
		final long startTime = System.currentTimeMillis(); //start now
		final long endTime = startTime + Milliseconds.fromHours(11); //check times for a few hours
		for(long time = startTime; time < endTime; time += 17) //check every 17 milliseconds
		{
			final Calendar calendar = new GregorianCalendar(GMT); //get a calendar in UTC
			calendar.setTimeInMillis(time); //set the calendar to our time
			final URFDateTime datetime1 = new URFDateTime(time);
			assertThat(datetime1.getYear(), equalTo(calendar.get(Calendar.YEAR)));
			assertThat(datetime1.getMonth(), equalTo(calendar.get(Calendar.MONTH) + 1)); //Calendar's months are one-based
			assertThat(datetime1.getDay(), equalTo(calendar.get(Calendar.DAY_OF_MONTH)));
			assertThat(datetime1.getURFTime().getHours(), equalTo(calendar.get(Calendar.HOUR_OF_DAY)));
			assertThat(datetime1.getURFTime().getMinutes(), equalTo(calendar.get(Calendar.MINUTE)));
			assertThat(datetime1.getURFTime().getSeconds(), equalTo(calendar.get(Calendar.SECOND)));
			assertThat(datetime1.getURFTime().getMicroseconds(), equalTo(calendar.get(Calendar.MILLISECOND) * 1000));
			assertThat("Constructed datetime's milliseconds doesn't equal the input time.", datetime1.getTime(), equalTo(time));
			final String datetime1String = datetime1.toString();
			//Log.debug(datetime1String);
			final URFDateTime datetime2 = URFDateTime.valueOf(datetime1String); //create a new datetime from the string representation
			assertThat(datetime2.getYear(), equalTo(calendar.get(Calendar.YEAR)));
			assertThat(datetime2.getMonth(), equalTo(calendar.get(Calendar.MONTH) + 1)); //Calendar's months are one-based
			assertThat(datetime2.getDay(), equalTo(calendar.get(Calendar.DAY_OF_MONTH)));
			assertThat(datetime2.getURFTime().getHours(), equalTo(calendar.get(Calendar.HOUR_OF_DAY)));
			assertThat(datetime2.getURFTime().getMinutes(), equalTo(calendar.get(Calendar.MINUTE)));
			assertThat(datetime2.getURFTime().getSeconds(), equalTo(calendar.get(Calendar.SECOND)));
			assertThat(datetime2.getURFTime().getMicroseconds(), equalTo(calendar.get(Calendar.MILLISECOND) * 1000));
			final String datetime2String = datetime1.toString();
			assertThat("Round-trip datetime string equal beginning datetime string.", datetime2String, equalTo(datetime2String));
			//Log.debug(datetime2String);
			assertThat("Round-trip datetime doesn't equal beginning time.", datetime2.getTime(), equalTo(time));
		}
	}

}
