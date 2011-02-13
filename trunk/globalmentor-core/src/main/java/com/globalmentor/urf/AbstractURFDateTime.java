/*
 * Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.util.*;

import com.globalmentor.java.Integers;
import static com.globalmentor.iso.ISO8601.*;

/**The abstract base type for <code>urf.Date</code> and <code>urf.DateTime</code> types.
If there is no explicit UTC offset (i.e. this is a floating value), the time is stored internally in terms of UTC.
@author Garret Wilson
*/
public abstract class AbstractURFDateTime extends Date implements URFTemporal
{

	/**The year.*/
	private final int year;

		/**@return The year.*/
		public final int getYear() {return year;}

	/**The month.*/
	private final int month;

		/**@return The month.*/
		public final int getMonth() {return month;}

	/**The day.*/
	private final int day;

		/**@return The day.*/
		public final int getDay() {return day;}

	/**The time, or <code>null</code> if there is a date with no time (not even midnight)*/
	private final URFTime time;

		/**@return The time, or <code>null</code> if there is a date with no time (not even midnight)*/
		public URFTime getURFTime() {return time;}

	/**Date components and time constructor with a date specified in epoch terms.
	@param year The year, 0-9999.
	@param month The month, 1-12.
	@param day The day, 1-31.
	@param time The time, or <code>null</code> if there is a date with no time (not even midnight).
	@param date The milliseconds since January 1, 1970, 00:00:00 GMT.
	@exception IllegalArgumentException if one of the given arguments is outside the allowed range.
	*/
/*TODO del
	protected AbstractURFDateTime(final int year, final int month, final int day, final URFTime time, final long date)
	{
		super(date);	//construct the parent class with the date in epoch terms
		this.year=checkRange(year, 0, 9999);
		this.month=checkRange(month, 1, 12);
		this.day=checkRange(day, 1, 31);
		this.time=time;
	}
*/

	/**Date components and time constructor in terms of UTC.
	@param year The year, 0-9999.
	@param month The month, 1-12.
	@param day The day, 1-31.
	@param time The time, or <code>null</code> if there is a date with no time (not even midnight).
	@exception IllegalArgumentException if one of the given arguments is outside the allowed range.
	*/
/*TODO del
	protected AbstractURFDateTime(final int year, final int month, final int day, final URFTime time)
	{
		this(year, month, day, time, URFTemporalComponents.createCalendar(year, month, day, time!=null ? time : URFTime.MIDNIGHT_UTC, Locale.ENGLISH).getTimeInMillis());	//construct class with the time in milleconds the given information represents, using midnight UTC if no time was given; the locale shouldn't matter for just determining the time in milliseconds, so use a locale that is likely to be available
	}
*/

	/**Temporal components constructor.
	@param temporalComponents The components of the time information.
	@param useTime <code>true</code> if the time should be used, or <code>false</code> if the given type components should be ignored.
	@throws NullPointerException if the temporal components is null.
	*/
	protected AbstractURFDateTime(final URFTemporalComponents temporalComponents, final boolean useTime)
	{
		super(temporalComponents.getTime());	//construct the parent class with the date time in milliseconds
		this.year=temporalComponents.getYear();
		this.month=temporalComponents.getMonth();
		this.day=temporalComponents.getDay();
		this.time=useTime ? temporalComponents.asTime() : null;	//store time if requested and if possible
	}

	/**Appends the canonical lexical representation of this date time to a string builder in the form "YYYY-MM-DDThh:mm:ss[.s+]+/-hh:mm".
	@param stringBuild The string builder to which the lexical representation will be appended.
	@return The string builder.
	*/
	public StringBuilder append(final StringBuilder stringBuilder)
	{
		stringBuilder.append(Integers.toString(getYear(), 10, 4));	//append the year, using four digits
		stringBuilder.append(DATE_DELIMITER);	//append the date delimiter
		stringBuilder.append(Integers.toString(getMonth(), 10, 2));	//append the month, using two digits
		stringBuilder.append(DATE_DELIMITER);	//append the date delimiter
		stringBuilder.append(Integers.toString(getDay(), 10, 2));	//append the day, using two digits
		final URFTime time=getURFTime();	//get the time, if any
		if(time!=null)	//if there is a time
		{
			stringBuilder.append(TIME_BEGIN);	//indicate that the time is beginning
			time.append(stringBuilder);	//append the time
		}
		return stringBuilder;	//return the string builder				
	}

	/**Returns a calendar representing this date and time.
	If this object has no time information, midnight UTC will be assumed.
	@param locale The locale for which a calendar should be returned.
	@return A calendar representing this date time in the given locale.
	@exception NullPointerException if the given locale is <code>null</code>.
	*/
	public Calendar toCalendar(final Locale locale)
	{
		return URFTemporalComponents.createCalendar(getYear(), getMonth(), getDay(), getURFTime(), locale);
	}

	/**Returns a date that represents this temporal information in the given time zone.
	@param timeZone The time zone which the date should represent.
	@return The date this object represents in relation to the given time zone.
	@throws NullPointerException if the given time zone is <code>null</code>.
	*/
	public abstract Date toDate(final TimeZone timeZone);

	
	/**Returns the canonical lexical representation of this date time in the form "YYYY-MM-DDThh:mm:ss[.s+]+/-hh:mm".
	@return The canonical lexical representation of this date time.
	*/
	public String toString()
	{
		return append(new StringBuilder()).toString();	//append the lexical representation to a new string builder and return the resulting string
	}

}
