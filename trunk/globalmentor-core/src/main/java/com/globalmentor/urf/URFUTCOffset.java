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

import java.util.TimeZone;

import com.globalmentor.java.Integers;
import com.globalmentor.text.*;

import static com.globalmentor.iso.ISO8601.*;
import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.model.TimeZones.*;

/**The class representing an <code>urf.UTCOffset</code> type.
@author Garret Wilson
*/
public class URFUTCOffset implements URFTemporal
{

	/**The shared UTC offset representing Coordinated Universal Time (UTC), +00:00.*/
	public final static URFUTCOffset UTC=new URFUTCOffset(0, 0);

	/**The shared time zone representing Greenwich Mean Time (GMT).*/
	public final static TimeZone GMT=TimeZone.getTimeZone(GMT_ID);

	/**The offset hours.*/
	private final int hours;

		/**@return The offset hours.*/
		public int getHours() {return hours;}

	/**The offset minutes.*/
	private final int minutes;

		/**@return The offset minutes.*/
		public int getMinutes() {return minutes;}

	/**Full Constructor.
	@param hours The offset hours.
	@param minutes The offset minutes.
	@exception IllegalArgumentException if the given offset minutes is negative.
	*/
	public URFUTCOffset(final int hours, final int minutes)
	{
		this.hours=hours;	//save the hours
		this.minutes=checkArgumentMinimum(minutes, 0);	//save the minutes, but don't allow negative offset minutes
	}

	/**Returns an URF UTC offset object holding the value of the specified string.
	@param string The string to be parsed as a UTC offset.
	@return An URF UTC offset object represented by the string.
	@exception NullPointerException if the given string is <code>null</code>
	@exception ArgumentSyntaxException if the given string does not have the correct syntax.
	*/
	public static URFUTCOffset valueOf(final String string) throws ArgumentSyntaxException
	{
		try
		{
			final URFTemporalComponents temporalComponents=URFTemporalComponents.parseDateTimeUTCOffset(string, false, false);	//parse temporal components for only the UTC offset
			return temporalComponents.asUTCOffset();	//return the UTC offset from the temporal components
		}
		catch(final SyntaxException syntaxException)	//if the syntax of the string was not correct
		{
			throw new ArgumentSyntaxException(syntaxException);
		}
	}

	/**Returns the UTC offset as a time zone.
	@return A time zone object representing the UTC offset.
	*/
	public TimeZone toTimeZone()
	{
		final StringBuilder timeZoneIDStringBuilder=new StringBuilder();	//create a new string builder
		timeZoneIDStringBuilder.append(GMT_ID);	//start with "GMT"
		if(hours!=0 || minutes>0)	//if this is not exactly GMT
		{
			if(hours>=0)	//if there is a nonnegative hour offset
			{
				timeZoneIDStringBuilder.append('+');	//show that  this is a positive offset
			}
			timeZoneIDStringBuilder.append(Integer.toString(hours));	//append the offset hours
			timeZoneIDStringBuilder.append(TIME_DELIMITER);	//append ':'
			timeZoneIDStringBuilder.append(Integers.toString(minutes, 10, 2));	//append the offset minutes, using two digits
		}
		return TimeZone.getTimeZone(timeZoneIDStringBuilder.toString());	//look up the time zone and return it
	}

	/**Appends the canonical lexical representation of this UTC offset to a string builder in the form "+/-hh:mm".
	@param stringBuild The string builder to which the lexical representation will be appended.
	@return The string builder.
	*/
	public StringBuilder append(final StringBuilder stringBuilder)
	{
		stringBuilder.append(hours<0 ? '-' : '+');	//show whether this is a a positive or negative offset
		stringBuilder.append(Integers.toString(Math.abs(hours), 10, 2));	//append the offset hours, using two digits
		stringBuilder.append(TIME_DELIMITER);	//append ':'
		stringBuilder.append(Integers.toString(minutes, 10, 2));	//append the offset minutes, using two digits
		return stringBuilder;	//return the string builder
	}

	/**Returns the canonical lexical representation of this UTC offset in the form "+/-hh:mm".
	@return The canonical lexical representation of this UTC offset.
	*/
	public String toString()
	{
		return append(new StringBuilder()).toString();	//append the lexical representation to a new string builder and return the resulting string
	}

}
