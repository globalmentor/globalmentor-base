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

import java.util.Date;

import com.globalmentor.java.Integers;
import com.globalmentor.text.*;

import static com.globalmentor.iso.ISO8601.*;
import static com.globalmentor.java.Integers.*;
import static com.globalmentor.java.StringBuilders.*;
import static com.globalmentor.urf.URF.*;

/**The class representing an <code>urf.Time</code> type.
@author Garret Wilson
*/
public class URFTime implements URFTemporal
{

	/**The shared time representing midnight UTC, the time 00:00:00+00:00.*/
	public final static URFTime MIDNIGHT_UTC=new URFTime(0, 0, 0, 0, URFUTCOffset.UTC);

	/**The hours.*/
	private final int hours;

		/**@return The hours.*/
		public final int getHours() {return hours;}

	/**The minutes.*/
	private final int minutes;

		/**@return The minutes.*/
		public final int getMinutes() {return minutes;}

	/**The seconds.*/
	private final int seconds;

		/**@return The seconds.*/
		public final int getSeconds() {return seconds;}

	/**The microseconds.*/
	private final int microseconds;

		/**@return The microseconds.*/
		public final int getMicroseconds() {return microseconds;}

	/**The UTC offset, or <code>null</code> if no UTC offset is known.*/
	private final URFUTCOffset utcOffset;

		/**@return The UTC offset, or <code>null</code> if no UTC offset is known.*/
		public URFUTCOffset getUTCOffset() {return utcOffset;}

	/**Full Constructor.
	@param year The year, 0-9999.
	@param month The month, 1-12.
	@param day The day, 1-31.
	@param hours The hours, 0-23.
	@param minutes The minutes, 0-59.
	@param seconds The seconds, 0-60 (allowing leap-seconds; see ISO 8601:2004(E) 4.2.1).
	@param microseconds The microseconds, 0-999999
	@param utcOffset The UTC offset, or <code>null</code> if no UTC offset is known.
	@exception IllegalArgumentException if one of the given arguments is outside the allowed range.
	*/
	public URFTime(final int hours, final int minutes, final int seconds, final int microseconds, final URFUTCOffset utcOffset)
	{
		this.hours=checkRange(hours, 0, 23);
		this.minutes=checkRange(minutes, 0, 59);
		this.seconds=checkRange(seconds, 0, 60);	//allow leap-seconds (see ISO 8601:2004(E) 4.2.1)
		this.microseconds=checkRange(microseconds, 0, 999999);
		this.utcOffset=utcOffset;
	}

	/**Temporal component constructor.
	@param temporalcomponents The temporal components from which to construct the class.
	@exception NullPointerException if the given temporal components is <code>null</code>.
	*/
	protected URFTime(final URFTemporalComponents temporalComponents)
	{
		this(temporalComponents.getHours(), temporalComponents.getMinutes(), temporalComponents.getSeconds(), temporalComponents.getMicroseconds(), temporalComponents.asUTCOffset());
	}

	/**Date constructor.
	Any date-related information of the given date will be lost; only the time will be kept, in terms of midnight UTC.
	@param date The date representing the difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	@exception NullPointerException if the given date is <code>null</code>.
	*/
	public URFTime(final Date date)
	{
		this(new URFTemporalComponents(date));	//construct the class from temporal components
	}

	/**Millisecond time constructor.
	Any date-related information of the given time will be lost; only the time will be kept, in terms of midnight UTC.
	@param time The difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	*/
	public URFTime(final long time)
	{
		this(new URFTemporalComponents(time));	//construct the class from temporal components
	}

	/**Returns an URF time object holding the value of the specified string.
	@param string The string to be parsed as a time.
	@return An URF time object represented by the string.
	@exception NullPointerException if the given string is <code>null</code>
	@exception ArgumentSyntaxException if the given string does not have the correct syntax.
	*/
	public static URFTime valueOf(final String string) throws ArgumentSyntaxException
	{
		try
		{
			return new URFTime(URFTemporalComponents.parseDateTimeUTCOffset(string, false, true));	//parse temporal components with only a time and use that to create a new time object
		}
		catch(final SyntaxException syntaxException)	//if the syntax of the string was not correct
		{
			throw new ArgumentSyntaxException(syntaxException);
		}
	}

	/**Appends the canonical lexical representation of this time to a string builder in the form "hh:mm:ss[.s+]+/-hh:mm".
	@param stringBuild The string builder to which the lexical representation will be appended.
	@return The string builder.
	*/
	public StringBuilder append(final StringBuilder stringBuilder)
	{
		stringBuilder.append(Integers.toString(getHours(), 10, 2));	//append the hours, using two digits
		stringBuilder.append(TIME_DELIMITER);	//append ':'
		stringBuilder.append(Integers.toString(getMinutes(), 10, 2));	//append the minutes, using two digits
		stringBuilder.append(TIME_DELIMITER);	//append ':'
		stringBuilder.append(Integers.toString(getSeconds(), 10, 2));	//append the seconds, using two digits
		final int microseconds=getMicroseconds();	//get the microseconds
		if(microseconds>0)	//if microseconds are given
		{
			stringBuilder.append(DECIMAL_DELIMITER);	//indicate that subseconds are present
			assert microseconds<1000000 : "Unexpectedly more than a million microseconds.";	//TODO check this in the constructor
			stringBuilder.append(Integer.toString(microseconds));	//append the string form of the microseconds
			trimEnd(stringBuilder, '0');	//remove trailing zeros
		}
		final URFUTCOffset utcOffset=getUTCOffset();	//get the UTC offset, if any
		if(utcOffset!=null)	//if there is a UTC offset
		{
			utcOffset.append(stringBuilder);	//append the lexical form of the UTC offset to our string builder
		}
		return stringBuilder;	//return the string builder
	}

	/**Returns the canonical lexical representation of this time in the form "hh:mm:ss[.s+]+/-hh:mm".
	@return The canonical lexical representation of this time.
	*/
	public String toString()
	{
		return append(new StringBuilder()).toString();	//append the lexical representation to a new string builder and return the resulting string
	}

}
