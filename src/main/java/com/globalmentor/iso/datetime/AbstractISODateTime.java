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

import com.globalmentor.java.Integers;
import com.globalmentor.text.ArgumentSyntaxException;
import com.globalmentor.text.SyntaxException;
import com.globalmentor.time.Time;

import static com.globalmentor.iso.datetime.ISO8601.*;

/**
 * The abstract base type for ISO date and date time types. If there is no explicit UTC offset (i.e. this is a floating value), the time is stored internally in
 * terms of UTC.
 * @author Garret Wilson
 */
public abstract class AbstractISODateTime extends Time implements ISOTemporal {

	/** The year, 0-9999. */
	private final int year;

	/** @return The year, 0-9999. */
	public final int getYear() {
		return year;
	}

	/** The month, 1-12. */
	private final int month;

	/** @return The month, 1-12. */
	public final int getMonth() {
		return month;
	}

	/** The day, 1-31. */
	private final int day;

	/** @return The day, 1-31. */
	public final int getDay() {
		return day;
	}

	/** The time, or <code>null</code> if there is a date with no time (not even midnight) */
	private final ISOTime time;

	/** @return The time, or <code>null</code> if there is a date with no time (not even midnight) */
	public ISOTime getISOTime() {
		return time;
	}

	/**
	 * Temporal components constructor.
	 * @param temporalComponents The components of the time information.
	 * @param useTime <code>true</code> if the time should be used, or <code>false</code> if the given type components should be ignored.
	 * @throws NullPointerException if the temporal components is null.
	 */
	protected AbstractISODateTime(final ISOTemporalComponents temporalComponents, final boolean useTime) {
		super(temporalComponents.getTime()); //construct the parent class with the date time in milliseconds
		this.year = temporalComponents.getYear();
		this.month = temporalComponents.getMonth();
		this.day = temporalComponents.getDay();
		this.time = useTime ? temporalComponents.asTime() : null; //store time if requested and if possible
	}

	/**
	 * Appends the canonical lexical representation of this date time to a string builder in the form "YYYY-MM-DDThh:mm:ss[.s+]+/-hh:mm".
	 * @param stringBuilder The string builder to which the lexical representation will be appended.
	 * @return The string builder.
	 */
	public StringBuilder append(final StringBuilder stringBuilder) {
		stringBuilder.append(Integers.toString(getYear(), 10, 4)); //append the year, using four digits
		stringBuilder.append(DATE_DELIMITER); //append the date delimiter
		stringBuilder.append(Integers.toString(getMonth(), 10, 2)); //append the month, using two digits
		stringBuilder.append(DATE_DELIMITER); //append the date delimiter
		stringBuilder.append(Integers.toString(getDay(), 10, 2)); //append the day, using two digits
		final ISOTime time = getISOTime(); //get the time, if any
		if(time != null) { //if there is a time
			stringBuilder.append(TIME_BEGIN); //indicate that the time is beginning
			time.append(stringBuilder); //append the time
		}
		return stringBuilder; //return the string builder				
	}

	/**
	 * Returns an ISO date or date time object holding the value of the specified string.
	 * <p>
	 * Lenient parsing makes the following allowances:
	 * </p>
	 * <ul>
	 * <li>Seconds are considered optional.</li>
	 * <li>Whitespace before and after the date/time is allowed.</li>
	 * <li>The looser RFC 3339 Internet timestamp format is allowed, allowing the UTC designator, {@value ISO8601#UTC_DESIGNATOR}, for example.</li>
	 * <li>If time is present, an {@link ISODateTime} is returned; otherwise, an {@link ISODate} is returned.</li>
	 * </ul>
	 * @param string The string to be parsed as a date time.
	 * @return An ISO date time object represented by the string.
	 * @throws NullPointerException if the given string is <code>null</code>
	 * @throws ArgumentSyntaxException if the given string does not have the correct syntax.
	 */
	public static AbstractISODateTime valueOfLenient(final String string) throws ArgumentSyntaxException {
		try {
			final ISOTemporalComponents temporalComponents = ISOTemporalComponents.parseDateTimeUTCOffset(string, true, null, true, true, true); //parse temporal components for both the date and the time and use that to create a new date time object, leniently accepting input
			return temporalComponents.hasTimeComponents() ? new ISODateTime(temporalComponents) : new ISODate(temporalComponents); //return an ISO date or an ISO date time, depending on which components were present
		} catch(final SyntaxException syntaxException) { //if the syntax of the string was not correct
			throw new ArgumentSyntaxException(syntaxException);
		}
	}

	/**
	 * Returns an ISO date or date time object holding the value of the specified string.
	 * <p>
	 * Liberal parsing makes the following allowances:
	 * </p>
	 * <ul>
	 * <li>Seconds are considered optional.</li>
	 * <li>Whitespace before and after the date/time is allowed.</li>
	 * <li>The looser RFC 3339 Internet timestamp format is allowed, allowing the UTC designator, {@value ISO8601#UTC_DESIGNATOR}, for example.</li>
	 * <li>If time is present, an {@link ISODateTime} is returned; otherwise, an {@link ISODate} is returned.</li>
	 * <li>Delimiters are optional.</li>
	 * </ul>
	 * @param string The string to be parsed as a date time.
	 * @return An ISO date time object represented by the string.
	 * @throws NullPointerException if the given string is <code>null</code>
	 * @throws ArgumentSyntaxException if the given string does not have the correct syntax.
	 */
	public static AbstractISODateTime valueOfLiberal(final String string) throws ArgumentSyntaxException {
		try {
			final ISOTemporalComponents temporalComponents = ISOTemporalComponents.parseDateTimeUTCOffset(string, true, null, true, true, false); //parse temporal components for both the date and the time and use that to create a new date time object, liberally accepting input
			return temporalComponents.hasTimeComponents() ? new ISODateTime(temporalComponents) : new ISODate(temporalComponents); //return an ISO date or an ISO date time , depending on which components were present
		} catch(final SyntaxException syntaxException) { //if the syntax of the string was not correct
			throw new ArgumentSyntaxException(syntaxException);
		}
	}

	/**
	 * Returns a calendar representing this date and time for the default locale. If this object has no time information, midnight UTC will be assumed.
	 * @return A calendar representing this date time in the default locale.
	 * @see Locale#getDefault()
	 */
	public GregorianCalendar toCalendar() {
		return toCalendar(Locale.getDefault());
	}

	/**
	 * Returns a calendar representing this date and time. If this object has no time information, midnight UTC will be assumed.
	 * @param locale The locale for which a calendar should be returned.
	 * @return A calendar representing this date time in the given locale.
	 * @throws NullPointerException if the given locale is <code>null</code>.
	 */
	public GregorianCalendar toCalendar(final Locale locale) {
		return ISOTemporalComponents.createCalendar(getYear(), getMonth(), getDay(), getISOTime(), locale);
	}

	/**
	 * Returns a date that represents this temporal information in the given time zone.
	 * @param timeZone The time zone which the date should represent.
	 * @return The date this object represents in relation to the given time zone.
	 * @throws NullPointerException if the given time zone is <code>null</code>.
	 */
	public abstract Date toDate(final TimeZone timeZone);

	/**
	 * Returns the date portion of this date and time.
	 * @return A date and time object with only the date part of this instance.
	 */
	public abstract ISODate toISODate();

	/**
	 * Returns the canonical lexical representation of this date time in the form "YYYY-MM-DDThh:mm:ss[.s+]+/-hh:mm".
	 * @return The canonical lexical representation of this date time.
	 */
	public String toString() {
		return append(new StringBuilder()).toString(); //append the lexical representation to a new string builder and return the resulting string
	}

}