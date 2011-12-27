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

package com.globalmentor.time;

import java.util.*;

import static com.globalmentor.iso.ISO8601.*;
import com.globalmentor.java.Integers;
import static com.globalmentor.java.Objects.*;

/**Constant values and utilities for working with time zones.
@author Garret Wilson
*/
public class TimeZones
{
	/**The ID for indicating the GMT zone.*/
	public final static String GMT_ID="GMT";

	/**The shared time zone representing Greenwich Mean Time (GMT).*/
	public final static TimeZone GMT=TimeZone.getTimeZone(GMT_ID);
	
	/**Retrieves a time zone based upon a UTC offset for the given date.
	This method cannot return with certain a time zone valid for other dates,
	as sufficient information both as to location and to daylight saving is not provided.
	This method cannot handle offsets that do not fall on whole minutes. 
	@param date The date for which a time zone will be calculated.
	@param offset The UTC offset for which a time zone will be retrieved, in milliseconds.
	@return A time zone appropriate for the given UTC offset for the given date.
	@throws NullPointerException if the given date is <code>null</code>.
	@throws IllegalArgumentException if an offset was provided for which no time zone could be found.
	*/
	public static TimeZone getTimeZone(final Date date, int offset)
	{
		final StringBuilder timeZoneIDStringBuilder=new StringBuilder();	//create a new string builder
		timeZoneIDStringBuilder.append(GMT_ID);	//start with "GMT"
		if(offset!=0)	//if this is not exactly GMT
		{
			final boolean negative=offset<0;	//see if the offset is negative
			if(negative)	//if the offset is negative
			{
				offset=-offset;	//use the postive offset in our calculations
			}
			final int hours=offset/(60*60*1000);	//get the hours
			final int minutes=(offset-(hours*60*60*1000))/(60*1000);	//determine the minutes
			if(hours*60*60*1000+minutes*60*1000!=offset)	//if there were milliseconds, we can't deal with those
			{
				throw new IllegalArgumentException("Cannot get time zone for millisecond-level offset "+offset);
			}
			return getTimeZone(date, negative ? -hours : hours, minutes);	//return the time zone for hours and minutes offset
		}
		return TimeZone.getTimeZone(timeZoneIDStringBuilder.toString());	//look up the time zone and return it
	}

	/**Retrieves a time zone based upon a UTC offset days and minutes for the given date.
	This method cannot return with certain a time zone valid for other dates,
	as sufficient information both as to location and to daylight saving is not provided.
	@param date The date for which a time zone will be calculated.
	@param utcOffsetHours The UTC offset hours.
	@param utcOffsetMinutes The UTC offset minutes, or -1 if there is no UTC offset hours or minutes specified.
	@return A time zone appropriate for the given UTC offset for the given date.
	@throws NullPointerException if the given date is <code>null</code>.
	@throws IllegalArgumentException if an offset was provided for which no time zone could be found.
	*/
	public static TimeZone getTimeZone(final Date date, final int utcOffsetHours, final int utcOffsetMinutes)
	{
		checkInstance(date, "Date cannot be null.");
		return getTimeZone(utcOffsetHours, utcOffsetMinutes);	//currently we ignore the date
	}

	/**Retrieves a time zone based upon a UTC offset days and minutes for the given date.
	This method cannot return with certain a time zone valid for other dates,
	as sufficient information both as to location and to daylight saving is not provided.
	@param utcOffsetHours The UTC offset hours.
	@param utcOffsetMinutes The UTC offset minutes, or -1 if there is no UTC offset hours or minutes specified.
	@return A general time zone appropriate for the given UTC offset.
	@throws IllegalArgumentException if an offset was provided for which no time zone could be found.
	*/
	public static TimeZone getTimeZone(final int utcOffsetHours, final int utcOffsetMinutes)
	{
		final StringBuilder timeZoneIDStringBuilder=new StringBuilder();	//create a new string builder
		timeZoneIDStringBuilder.append(GMT_ID);	//start with "GMT"
		if(utcOffsetHours!=0 || utcOffsetMinutes>0)	//if this is not exactly GMT
		{
			if(utcOffsetHours>=0)	//if there is a nonnegative hour offset
			{
				timeZoneIDStringBuilder.append('+');	//show that  this is a positive offset
			}
			timeZoneIDStringBuilder.append(Integer.toString(utcOffsetHours));	//append the offset hours
			timeZoneIDStringBuilder.append(TIME_DELIMITER);	//append ':'
			timeZoneIDStringBuilder.append(Integers.toString(utcOffsetMinutes, 10, 2));	//append the offset minutes, using two digits
		}
		return TimeZone.getTimeZone(timeZoneIDStringBuilder.toString());	//look up the time zone and return it
	}
}
