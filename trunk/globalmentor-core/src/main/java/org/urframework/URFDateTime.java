/*
 * Copyright © 2007-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package org.urframework;

import java.util.*;

import com.globalmentor.iso.ISO8601;
import com.globalmentor.text.*;
import com.globalmentor.time.Calendars;

/**
 * The class representing an <code>urf.DateTime</code> type. If there is no explicit UTC offset (i.e. this is a floating value), the time is stored internally
 * in terms of UTC.
 * @author Garret Wilson
 */
public class URFDateTime extends AbstractURFDateTime
{

	/** Default constructor of a floating date time with the current time in terms of UTC. */
	public URFDateTime()
	{
		this(System.currentTimeMillis()); //construct the class with the current time in milliseconds
	}

	/**
	 * Current time constructor in terms of UTC.
	 * @param timeZone The time zone in which the time should be interpreted.
	 * @throws NullPointerException if the given time zone is <code>null</code>.
	 * @throws IllegalArgumentException if a time zone was provided with an unsupported offset for the given time.
	 */
	public URFDateTime(final TimeZone timeZone)
	{
		this(System.currentTimeMillis(), timeZone); //construct the class with the current time in milliseconds
	}

	/**
	 * Temporal component constructor.
	 * @param temporalcomponents The temporal components from which to construct the class.
	 * @exception NullPointerException if the given temporal components is <code>null</code>.
	 */
	protected URFDateTime(final URFTemporalComponents temporalComponents)
	{
		super(temporalComponents, true); //construct the parent class, using the time information
	}

	/**
	 * Date components and time constructor.
	 * @param year The year, 0-9999.
	 * @param month The month, 1-12.
	 * @param day The day, 1-31.
	 * @param time The time.
	 * @exception NullPointerException if the given time is <code>null</code>.
	 * @exception IllegalArgumentException if one of the given arguments is outside the allowed range.
	 */
	public URFDateTime(final int year, final int month, final int day, final URFTime time)
	{
		this(new URFTemporalComponents(year, month, day, time.getHours(), time.getMinutes(), time.getSeconds(), time.getMicroseconds(),
				time.getUTCOffset() != null ? time.getUTCOffset().getHours() : -1, time.getUTCOffset() != null ? time.getUTCOffset().getMinutes() : -1)); //construct the class with only a date in UTC
	}

	/**
	 * Full Constructor.
	 * @param year The year, 0-9999.
	 * @param month The month, 1-12.
	 * @param day The day, 1-31.
	 * @param hours The hours, 0-23.
	 * @param minutes The minutes, 0-59.
	 * @param seconds The seconds, 0-60 (allowing leap-seconds; see ISO 8601:2004(E) 4.2.1).
	 * @param microseconds The microseconds, 0-999999.
	 * @param utcOffset The UTC offset, or <code>null</code> if no UTC offset is known.
	 * @exception IllegalArgumentException if one of the given arguments is outside the allowed range.
	 */
	public URFDateTime(final int year, final int month, final int day, final int hours, final int minutes, final int seconds, final int microseconds,
			final URFUTCOffset utcOffset)
	{
		this(year, month, day, new URFTime(hours, minutes, seconds, microseconds, utcOffset));
	}

	/**
	 * Date constructor in terms of UTC.
	 * @param date The date representing the difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	 * @exception NullPointerException if the given date is <code>null</code>.
	 */
	public URFDateTime(final Date date)
	{
		this(new URFTemporalComponents(date)); //construct the class from temporal components
	}

	/**
	 * Date constructor.
	 * @param date The date representing the difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	 * @param timeZone The time zone in which the time should be interpreted.
	 * @throws NullPointerException if the given date and/or time zone is <code>null</code>.
	 * @throws IllegalArgumentException if a time zone was provided with an unsupported offset for the given time.
	 */
	public URFDateTime(final Date date, final TimeZone timeZone)
	{
		this(new URFTemporalComponents(date, timeZone)); //construct the class from temporal components
	}

	/**
	 * Millisecond time constructor in terms of UTC.
	 * @param time The difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	 */
	public URFDateTime(final long time)
	{
		this(new URFTemporalComponents(time)); //construct the class from temporal components
	}

	/**
	 * Millisecond time constructor in terms of UTC.
	 * @param time The difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	 * @param timeZone The time zone in which the time should be interpreted.
	 * @throws NullPointerException if the given time zone is <code>null</code>.
	 * @throws IllegalArgumentException if a time zone was provided with an unsupported offset for the given time.
	 */
	public URFDateTime(final long time, final TimeZone timeZone)
	{
		this(new URFTemporalComponents(time, timeZone)); //construct the class from temporal components
	}

	/**
	 * Returns whether the time, if any, represents midnight at the beginning of the day (00:00:00:00) in whatever UTC offset, if any, is indicated.
	 * <p>
	 * If this method returns <code>true</code>, it indicates that {@link #getURFTime()} is not <code>null</code>, as this date and time can only indicate
	 * midnight when there is an URF time component present.
	 * </p>
	 * @return <code>true</code> if this time represents midnight at the beginning of the day (00:00:00:00) in whatever UTC offset, if any, is indicated.
	 * @see URFTime#isMidnight()
	 */
	public boolean isMidnight()
	{
		final URFTime urfTime = getURFTime();
		return urfTime != null && urfTime.isMidnight(); //see if there is a time and if it is midnight
	}

	/**
	 * Returns a date that represents this temporal information in the given time zone.
	 * @param timeZone The time zone which the date should represent.
	 * @return The date this object represents in relation to the given time zone.
	 * @throws NullPointerException if the given time zone is <code>null</code>.
	 */
	public Date toDate(final TimeZone timeZone)
	{
		final Calendar calendar = new GregorianCalendar(timeZone); //create a Gregorian calendar for the given time zone
		calendar.clear(); //clear the calendar
		calendar.set(getYear(), getMonth() - 1, getDay()); //set the calendar date, compensating for Calendar's zero-based month
		final URFTime time = getURFTime(); //get the URF time, if any
		if(time != null) //if we have time
		{
			Calendars.setTime(calendar, time.getHours(), time.getMinutes(), time.getSeconds(), time.getMicroseconds() / 1000); //set the time
		}
		return calendar.getTime(); //return the calendar time
	}

	/** {@inheritDoc} */
	@Override
	public URFDate toURFDate()
	{
		return new URFDate(getYear(), getMonth(), getDay());
	}

	/**
	 * Returns an URF date time object holding the value of the specified string.
	 * @param string The string to be parsed as a date time.
	 * @return An URF date time object represented by the string.
	 * @exception NullPointerException if the given string is <code>null</code>
	 * @exception ArgumentSyntaxException if the given string does not have the correct syntax.
	 */
	public static URFDateTime valueOf(final String string) throws ArgumentSyntaxException
	{
		try
		{
			return new URFDateTime(URFTemporalComponents.parseDateTimeUTCOffset(string, true, true)); //parse temporal components for both the date and the time and use that to create a new date time object
		}
		catch(final SyntaxException syntaxException) //if the syntax of the string was not correct
		{
			throw new ArgumentSyntaxException(syntaxException);
		}
	}

	/**
	 * Returns an URF date time object holding the value of the specified string.
	 * <p>
	 * Lenient parsing makes the following allowances:
	 * <ul>
	 * <li>Seconds are considered optional.</li>
	 * <li>Whitespace before and after the date/time is allowed.</li>
	 * <li>The looser RFC 3339 Internet timestamp format is allowed, allowing the UTC designator, {@value ISO8601#UTC_DESIGNATOR}, for example.</li>
	 * </ul>
	 * </p>
	 * @param string The string to be parsed as a date time.
	 * @return An URF date time object represented by the string.
	 * @exception NullPointerException if the given string is <code>null</code>
	 * @exception ArgumentSyntaxException if the given string does not have the correct syntax.
	 */
	public static URFDateTime valueOfLenient(final String string) throws ArgumentSyntaxException
	{
		try
		{
			return new URFDateTime(URFTemporalComponents.parseDateTimeUTCOffset(string, true, true, true, true, true)); //parse temporal components for both the date and the time and use that to create a new date time object, leniently accepting input
		}
		catch(final SyntaxException syntaxException) //if the syntax of the string was not correct
		{
			throw new ArgumentSyntaxException(syntaxException);
		}
	}

	/**
	 * Returns an URF date time object holding the value of the specified string.
	 * <p>
	 * Liberal parsing makes the following allowances:
	 * <ul>
	 * <li>Seconds are considered optional.</li>
	 * <li>Whitespace before and after the date/time is allowed.</li>
	 * <li>The looser RFC 3339 Internet timestamp format is allowed, allowing the UTC designator, {@value ISO8601#UTC_DESIGNATOR}, for example.</li>
	 * <li>Delimiters are optional.</li>
	 * </ul>
	 * </p>
	 * @param string The string to be parsed as a date time.
	 * @return An URF date time object represented by the string.
	 * @exception NullPointerException if the given string is <code>null</code>
	 * @exception ArgumentSyntaxException if the given string does not have the correct syntax.
	 */
	public static URFDateTime valueOfLiberal(final String string) throws ArgumentSyntaxException
	{
		try
		{
			return new URFDateTime(URFTemporalComponents.parseDateTimeUTCOffset(string, true, true, true, true, false)); //parse temporal components for both the date and the time and use that to create a new date time object, liberally accepting input
		}
		catch(final SyntaxException syntaxException) //if the syntax of the string was not correct
		{
			throw new ArgumentSyntaxException(syntaxException);
		}
	}

	/**
	 * Returns an URF date time object holding the value of the specified string. The looser RFC 3339 Internet timestamp format is allowed, which is used in
	 * "W3C Date and Time Formats" as well as portions of WebDAV.
	 * @param string The string to be parsed as a date time.
	 * @return An URF date time object represented by the string.
	 * @exception NullPointerException if the given string is <code>null</code>
	 * @exception ArgumentSyntaxException if the given string does not have the correct syntax.
	 * @see <a href="http://www.ietf.org/rfc/rfc3339.txt">RFC 3339</a>
	 * @see <a href="http://www.ietf.org/rfc/rfc2518.txt">RFC 2518</a>
	 * @see <a href="http://www.w3.org/TR/NOTE-datetime">W3C Date and Time Formats</a>
	 */
	public static URFDateTime valueOfTimestamp(final String string) throws ArgumentSyntaxException
	{
		try
		{
			return new URFDateTime(URFTemporalComponents.parseDateTimeUTCOffset(string, true, true, true, false, true)); //parse temporal components for both the date and the time, allowing RFC 3339 format, and use that to create a new date time object
		}
		catch(final SyntaxException syntaxException) //if the syntax of the string was not correct
		{
			throw new ArgumentSyntaxException(syntaxException);
		}
	}

}
