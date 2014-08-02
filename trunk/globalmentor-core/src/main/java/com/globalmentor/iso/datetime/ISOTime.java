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

import java.util.Date;

import com.globalmentor.java.Integers;
import com.globalmentor.text.*;

import static com.globalmentor.iso.datetime.ISO8601.*;
import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.java.StringBuilders.*;

/**
 * The class representing an ISO time type.
 * @author Garret Wilson
 */
public class ISOTime implements ISOTemporal
{

	/** The shared time representing midnight UTC, the time 00:00:00+00:00. */
	public final static ISOTime MIDNIGHT_UTC = new ISOTime(0, 0, 0, 0, ISOUTCOffset.UTC);

	/** The hours, 0-23. */
	private final int hours;

	/** @return The hours, 0-23. */
	public final int getHours()
	{
		return hours;
	}

	/** The minutes, 0-59. */
	private final int minutes;

	/** @return The minutes, 0-59. */
	public final int getMinutes()
	{
		return minutes;
	}

	/** The seconds, 0-60 (allowing leap-seconds; see ISO 8601:2004(E) 4.2.1). */
	private final int seconds;

	/** @return The seconds, 0-60 (allowing leap-seconds; see ISO 8601:2004(E) 4.2.1). */
	public final int getSeconds()
	{
		return seconds;
	}

	/** The microseconds, 0-999999. */
	private final int microseconds;

	/** @return The microseconds, 0-999999. */
	public final int getMicroseconds()
	{
		return microseconds;
	}

	/** The UTC offset, or <code>null</code> if no UTC offset is known. */
	private final ISOUTCOffset utcOffset;

	/** @return The UTC offset, or <code>null</code> if no UTC offset is known. */
	public ISOUTCOffset getUTCOffset()
	{
		return utcOffset;
	}

	/**
	 * Full Constructor.
	 * @param hours The hours, 0-23.
	 * @param minutes The minutes, 0-59.
	 * @param seconds The seconds, 0-60 (allowing leap-seconds; see ISO 8601:2004(E) 4.2.1).
	 * @param microseconds The microseconds, 0-999999.
	 * @param utcOffset The UTC offset, or <code>null</code> if no UTC offset is known.
	 * @throws IllegalArgumentException if one of the given arguments is outside the allowed range.
	 */
	public ISOTime(final int hours, final int minutes, final int seconds, final int microseconds, final ISOUTCOffset utcOffset)
	{
		this.hours = checkArgumentRange(hours, 0, 23);
		this.minutes = checkArgumentRange(minutes, 0, 59);
		this.seconds = checkArgumentRange(seconds, 0, 60); //allow leap-seconds (see ISO 8601:2004(E) 4.2.1)
		this.microseconds = checkArgumentRange(microseconds, 0, 999999);
		this.utcOffset = utcOffset;
	}

	/**
	 * Temporal component constructor.
	 * @param temporalcomponents The temporal components from which to construct the class.
	 * @throws NullPointerException if the given temporal components is <code>null</code>.
	 */
	protected ISOTime(final ISOTemporalComponents temporalComponents)
	{
		this(temporalComponents.getHours(), temporalComponents.getMinutes(), temporalComponents.getSeconds(), temporalComponents.getMicroseconds(),
				temporalComponents.asUTCOffset());
	}

	/**
	 * Date constructor. Any date-related information of the given date will be lost; only the time will be kept, in terms of midnight UTC.
	 * @param date The date representing the difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	 * @throws NullPointerException if the given date is <code>null</code>.
	 */
	public ISOTime(final Date date)
	{
		this(new ISOTemporalComponents(date)); //construct the class from temporal components
	}

	/**
	 * Millisecond time constructor. Any date-related information of the given time will be lost; only the time will be kept, in terms of midnight UTC.
	 * @param time The difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	 */
	public ISOTime(final long time)
	{
		this(new ISOTemporalComponents(time)); //construct the class from temporal components
	}

	/** @return <code>true</code> if this time represents midnight at the beginning of the day (00:00:00:00) in whatever UTC offset, if any, is indicated. */
	public boolean isMidnight()
	{
		return getHours() == 0 && getMinutes() == 0 && getSeconds() == 0 && getMicroseconds() == 0;
	}

	/**
	 * Returns an ISO time object holding the value of the specified string.
	 * @param string The string to be parsed as a time.
	 * @return An ISO time object represented by the string.
	 * @throws NullPointerException if the given string is <code>null</code>
	 * @throws ArgumentSyntaxException if the given string does not have the correct syntax.
	 */
	public static ISOTime valueOf(final String string) throws ArgumentSyntaxException
	{
		try
		{
			return new ISOTime(ISOTemporalComponents.parseDateTimeUTCOffset(string, false, true)); //parse temporal components with only a time and use that to create a new time object
		}
		catch(final SyntaxException syntaxException) //if the syntax of the string was not correct
		{
			throw new ArgumentSyntaxException(syntaxException);
		}
	}

	/**
	 * Appends the canonical lexical representation of this time to a string builder in the form "hh:mm:ss[.s+]+/-hh:mm".
	 * @param stringBuild The string builder to which the lexical representation will be appended.
	 * @return The string builder.
	 */
	public StringBuilder append(final StringBuilder stringBuilder)
	{
		stringBuilder.append(Integers.toString(getHours(), 10, 2)); //append the hours, using two digits
		stringBuilder.append(TIME_DELIMITER); //append ':'
		stringBuilder.append(Integers.toString(getMinutes(), 10, 2)); //append the minutes, using two digits
		stringBuilder.append(TIME_DELIMITER); //append ':'
		stringBuilder.append(Integers.toString(getSeconds(), 10, 2)); //append the seconds, using two digits
		final int microseconds = getMicroseconds(); //get the microseconds
		if(microseconds > 0) //if microseconds are given
		{
			stringBuilder.append(TIME_SUBSECONDS_DELIMITER); //indicate that subseconds are present
			assert microseconds < 1000000 : "Unexpectedly more than a million microseconds."; //TODO check this in the constructor
			appendForceLength(stringBuilder, Integer.toString(microseconds), 6, '0', 0); //append the string form of the microseconds, making sure the microseconds have a full six digits
			trimEnd(stringBuilder, '0'); //remove trailing zeros
		}
		final ISOUTCOffset utcOffset = getUTCOffset(); //get the UTC offset, if any
		if(utcOffset != null) //if there is a UTC offset
		{
			utcOffset.append(stringBuilder); //append the lexical form of the UTC offset to our string builder
		}
		return stringBuilder; //return the string builder
	}

	/**
	 * Returns the canonical lexical representation of this time in the form "hh:mm:ss[.s+]+/-hh:mm".
	 * @return The canonical lexical representation of this time.
	 */
	public String toString()
	{
		return append(new StringBuilder()).toString(); //append the lexical representation to a new string builder and return the resulting string
	}

}
