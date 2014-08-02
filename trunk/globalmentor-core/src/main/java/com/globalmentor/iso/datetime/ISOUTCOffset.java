/*
 * Copyright © 2007-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.util.TimeZone;

import com.globalmentor.java.Integers;
import com.globalmentor.text.*;

import static com.globalmentor.iso.datetime.ISO8601.*;
import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.time.TimeZones.*;

/**
 * The class representing an ISO UTC offset type.
 * @author Garret Wilson
 */
public class ISOUTCOffset implements ISOTemporal
{

	/** The shared UTC offset representing Coordinated Universal Time (UTC), +00:00. */
	public final static ISOUTCOffset UTC = new ISOUTCOffset(0, 0);

	/** The offset hours. */
	private final int hours;

	/** @return The offset hours. */
	public int getHours()
	{
		return hours;
	}

	/** The offset minutes. */
	private final int minutes;

	/** @return The offset minutes. */
	public int getMinutes()
	{
		return minutes;
	}

	/**
	 * Full Constructor.
	 * @param hours The offset hours.
	 * @param minutes The offset minutes.
	 * @throws IllegalArgumentException if the given offset minutes is negative.
	 */
	public ISOUTCOffset(final int hours, final int minutes)
	{
		this.hours = hours; //save the hours
		this.minutes = checkArgumentNotNegative(minutes); //save the minutes, but don't allow negative offset minutes
	}

	/**
	 * Returns an ISO UTC offset object holding the value of the specified string.
	 * @param string The string to be parsed as a UTC offset.
	 * @return An ISO UTC offset object represented by the string.
	 * @throws NullPointerException if the given string is <code>null</code>
	 * @throws ArgumentSyntaxException if the given string does not have the correct syntax.
	 */
	public static ISOUTCOffset valueOf(final String string) throws ArgumentSyntaxException
	{
		try
		{
			final ISOTemporalComponents temporalComponents = ISOTemporalComponents.parseDateTimeUTCOffset(string, false, false); //parse temporal components for only the UTC offset
			return temporalComponents.asUTCOffset(); //return the UTC offset from the temporal components
		}
		catch(final SyntaxException syntaxException) //if the syntax of the string was not correct
		{
			throw new ArgumentSyntaxException(syntaxException);
		}
	}

	/**
	 * Returns the UTC offset as a time zone.
	 * @return A time zone object representing the UTC offset.
	 */
	public TimeZone toTimeZone()
	{
		final StringBuilder timeZoneIDStringBuilder = new StringBuilder(); //create a new string builder
		timeZoneIDStringBuilder.append(GMT_ID); //start with "GMT"
		if(hours != 0 || minutes > 0) //if this is not exactly GMT
		{
			if(hours >= 0) //if there is a nonnegative hour offset
			{
				timeZoneIDStringBuilder.append('+'); //show that  this is a positive offset
			}
			timeZoneIDStringBuilder.append(Integer.toString(hours)); //append the offset hours
			timeZoneIDStringBuilder.append(TIME_DELIMITER); //append ':'
			timeZoneIDStringBuilder.append(Integers.toString(minutes, 10, 2)); //append the offset minutes, using two digits
		}
		return TimeZone.getTimeZone(timeZoneIDStringBuilder.toString()); //look up the time zone and return it
	}

	/**
	 * Appends the canonical lexical representation of this UTC offset to a string builder in the form "+/-hh:mm".
	 * @param stringBuild The string builder to which the lexical representation will be appended.
	 * @return The string builder.
	 */
	public StringBuilder append(final StringBuilder stringBuilder)
	{
		stringBuilder.append(hours < 0 ? '-' : '+'); //show whether this is a a positive or negative offset
		stringBuilder.append(Integers.toString(Math.abs(hours), 10, 2)); //append the offset hours, using two digits
		stringBuilder.append(TIME_DELIMITER); //append ':'
		stringBuilder.append(Integers.toString(minutes, 10, 2)); //append the offset minutes, using two digits
		return stringBuilder; //return the string builder
	}

	/**
	 * Returns the canonical lexical representation of this UTC offset in the form "+/-hh:mm".
	 * @return The canonical lexical representation of this UTC offset.
	 */
	public String toString()
	{
		return append(new StringBuilder()).toString(); //append the lexical representation to a new string builder and return the resulting string
	}

}
