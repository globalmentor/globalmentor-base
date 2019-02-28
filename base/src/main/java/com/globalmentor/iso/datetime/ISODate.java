/*
 * Copyright © 2007-2013 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.util.*;

import com.globalmentor.text.*;

/**
 * The class representing an ISO date type. If there is no explicit UTC offset (i.e. this is a floating value), the time is stored internally in terms of UTC.
 * @author Garret Wilson
 */
public class ISODate extends AbstractISODateTime {

	/**
	 * Temporal component constructor.
	 * @param temporalComponents The temporal components from which to construct the class.
	 * @throws NullPointerException if the given temporal components is <code>null</code>.
	 */
	protected ISODate(final ISOTemporalComponents temporalComponents) {
		super(temporalComponents, false); //construct the parent class, ignoring the time information
	}

	/**
	 * Date components constructor. The underlying {@link Date} will be constructed in terms of UTC.
	 * @param year The year, 0-9999.
	 * @param month The month, 1-12.
	 * @param day The day, 1-31.
	 * @throws IllegalArgumentException if one of the given arguments is outside the allowed range.
	 */
	public ISODate(final int year, final int month, final int day) {
		this(new ISOTemporalComponents(year, month, day, -1, -1, -1, -1, 0, 0)); //construct the parent class with only a date in UTC
	}

	/**
	 * Calendar constructor in terms of UTC. Any time-related information of the given calendar will be lost; only the date will be kept, in terms of the
	 * calendar's time zone.
	 * @param calendar The calendar representing the difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC; and a time
	 *          zone.
	 * @throws NullPointerException if the given calendar is <code>null</code>.
	 */
	public ISODate(final GregorianCalendar calendar) {
		this(calendar.getTime(), calendar.getTimeZone());
	}

	/**
	 * Date constructor in terms of UTC. Any time-related information of the given date will be lost; only the date will be kept, in terms of midnight UTC.
	 * @param date The date representing the difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	 * @throws NullPointerException if the given date is <code>null</code>.
	 */
	public ISODate(final Date date) {
		this(new ISOTemporalComponents(date)); //construct the class from temporal components
	}

	/**
	 * Date constructor in terms the given time zone. Any time-related information of the given date will be lost; only the date will be kept, in terms of the
	 * given time zone.
	 * @param date The date representing the difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	 * @param timeZone The time zone in which the time should be interpreted.
	 * @throws NullPointerException if the given date and/or time zone is <code>null</code>.
	 * @throws IllegalArgumentException if a time zone was provided with an unsupported offset for the given time.
	 */
	public ISODate(final Date date, final TimeZone timeZone) {
		this(new ISOTemporalComponents(date, timeZone)); //construct the class from temporal components
	}

	/**
	 * Millisecond time constructor in terms of UTC. Any time-related information of the given time will be lost; only the date will be kept, in terms of midnight
	 * UTC.
	 * @param time The difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	 */
	public ISODate(final long time) {
		this(new ISOTemporalComponents(time)); //construct the class from temporal components
	}

	/**
	 * Millisecond time constructor in terms of the given time zone. Any time-related information of the given time will be lost; only the date will be kept, in
	 * terms of the given time zone.
	 * @param time The difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	 * @param timeZone The time zone in which the time should be interpreted.
	 * @throws NullPointerException if the given time zone is <code>null</code>.
	 * @throws IllegalArgumentException if a time zone was provided with an unsupported offset for the given time.
	 */
	public ISODate(final long time, final TimeZone timeZone) {
		this(new ISOTemporalComponents(time, timeZone)); //construct the class from temporal components
	}

	/**
	 * Returns a date that represents this temporal information in the given time zone.
	 * @param timeZone The time zone which the date should represent.
	 * @return The date this object represents in relation to the given time zone.
	 * @throws NullPointerException if the given time zone is <code>null</code>.
	 * @see #toCalendar(TimeZone)
	 */
	public Date toDate(final TimeZone timeZone) {
		return toCalendar(timeZone).getTime(); //return the calendar time
	}

	/**
	 * Returns a calendar that represents this temporal information in the given time zone.
	 * @param timeZone The time zone which the date should represent.
	 * @return The calendar this object represents in relation to the given time zone.
	 * @throws NullPointerException if the given time zone is <code>null</code>.
	 */
	public GregorianCalendar toCalendar(final TimeZone timeZone) {
		final GregorianCalendar calendar = new GregorianCalendar(timeZone); //create a Gregorian calendar for the given time zone
		calendar.clear(); //clear the calendar
		calendar.set(getYear(), getMonth() - 1, getDay()); //set the calendar date, compensating for Calendar's zero-based month
		return calendar; //return the calendar
	}

	/** {@inheritDoc} */
	@Override
	public ISODate toISODate() {
		return this;
	}

	/**
	 * Returns an ISO date object holding the value of the specified string.
	 * @param string The string to be parsed as a date.
	 * @return An ISO date object represented by the string.
	 * @throws NullPointerException if the given string is <code>null</code>
	 * @throws ArgumentSyntaxException if the given string does not have the correct syntax.
	 */
	public static ISODate valueOf(final String string) throws ArgumentSyntaxException {
		try {
			return new ISODate(ISOTemporalComponents.parseDateTimeUTCOffset(string, true, false)); //parse temporal components with only a date and use that to create a new date object
		} catch(final SyntaxException syntaxException) { //if the syntax of the string was not correct
			throw new ArgumentSyntaxException(syntaxException);
		}
	}

	/**
	 * Returns an ISO date object holding the value of the specified string.
	 * <p>
	 * Lenient parsing makes the following allowances:
	 * </p>
	 * <ul>
	 * <li>Seconds are considered optional.</li>
	 * <li>Whitespace before and after the date/time is allowed.</li>
	 * <li>The looser RFC 3339 Internet timestamp format is allowed, allowing the UTC designator, {@value ISO8601#UTC_DESIGNATOR}, for example.</li>
	 * </ul>
	 * @param string The string to be parsed as a date.
	 * @return An ISO date object represented by the string.
	 * @throws NullPointerException if the given string is <code>null</code>
	 * @throws ArgumentSyntaxException if the given string does not have the correct syntax.
	 */
	public static ISODate valueOfLenient(final String string) throws ArgumentSyntaxException {
		try {
			return new ISODate(ISOTemporalComponents.parseDateTimeUTCOffset(string, true, false, true, true, true)); //parse temporal components with only a date and use that to create a new date object, leniently accepting input
		} catch(final SyntaxException syntaxException) { //if the syntax of the string was not correct
			throw new ArgumentSyntaxException(syntaxException);
		}
	}

	/**
	 * Returns an ISO date object holding the value of the specified string.
	 * <p>
	 * Liberal parsing makes the following allowances:
	 * </p>
	 * <ul>
	 * <li>Seconds are considered optional.</li>
	 * <li>Whitespace before and after the date/time is allowed.</li>
	 * <li>The looser RFC 3339 Internet timestamp format is allowed, allowing the UTC designator, {@value ISO8601#UTC_DESIGNATOR}, for example.</li>
	 * <li>Delimiters are optional.</li>
	 * </ul>
	 * @param string The string to be parsed as a date.
	 * @return An ISO date object represented by the string.
	 * @throws NullPointerException if the given string is <code>null</code>
	 * @throws ArgumentSyntaxException if the given string does not have the correct syntax.
	 */
	public static ISODate valueOfLiberal(final String string) throws ArgumentSyntaxException {
		try {
			return new ISODate(ISOTemporalComponents.parseDateTimeUTCOffset(string, true, false, true, true, false)); //parse temporal components with only a date and use that to create a new date object, liberally accepting input
		} catch(final SyntaxException syntaxException) { //if the syntax of the string was not correct
			throw new ArgumentSyntaxException(syntaxException);
		}
	}

}
