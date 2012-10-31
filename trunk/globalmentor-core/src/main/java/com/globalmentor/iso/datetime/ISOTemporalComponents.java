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

import java.io.*;
import java.util.*;
import static java.util.Calendar.*;

import com.globalmentor.io.ParseIOException;

import static com.globalmentor.java.Characters.*;
import com.globalmentor.text.SyntaxException;

import static com.globalmentor.io.ReaderParser.*;
import static com.globalmentor.iso.datetime.ISO8601.*;
import static com.globalmentor.java.Objects.*;
import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.java.Strings.*;
import static com.globalmentor.time.Calendars.*;
import static com.globalmentor.time.TimeZones.*;

/**
 * A lightweight structure for transferring components of ISO 8601 types.
 * @author Garret Wilson
 */
public class ISOTemporalComponents
{

	/** The year (0-9999), or -1 if there is no year specified. */
	private final int year;

	/** @return The year (0-9999), or -1 if there is no year specified. */
	public final int getYear()
	{
		return year;
	}

	/** The month (1-12), or -1 if there is no month specified. */
	private final int month;

	/** @return The month (1-12), or -1 if there is no month specified. */
	public final int getMonth()
	{
		return month;
	}

	/** The day (1-31), or -1 if there is no day specified. */
	private final int day;

	/** @return The day (1-31), or -1 if there is no day specified. */
	public final int getDay()
	{
		return day;
	}

	/** The hours (0-23), or -1 if there is no hours specified. */
	private final int hours;

	/** @return The hours (0-23), or -1 if there is no hours specified. */
	public final int getHours()
	{
		return hours;
	}

	/** The minutes (0-59), or -1 if there is no minutes specified. */
	private final int minutes;

	/** @return The minutes (0-59), or -1 if there is no minutes specified. */
	public final int getMinutes()
	{
		return minutes;
	}

	/** The seconds (0-60, allowing leap-seconds; see ISO 8601:2004(E) 4.2.1), or -1 if there is no seconds specified. */
	private final int seconds;

	/** @return The seconds (0-60, allowing leap-seconds; see ISO 8601:2004(E) 4.2.1), or -1 if there is no seconds specified. */
	public final int getSeconds()
	{
		return seconds;
	}

	/** The microseconds (0-999999), or -1 if there is no microseconds specified. */
	private final int microseconds;

	/** @return The microseconds (0-999999), or -1 if there is no microseconds specified. */
	public final int getMicroseconds()
	{
		return microseconds;
	}

	/** The UTC offset hours. */
	private final int utcOffsetHours;

	/** @return The UTC offset hours. */
	public final int getUTCOffsetHours()
	{
		return utcOffsetHours;
	}

	/** The UTC offset minutes, or -1 if there is no UTC offset hours or minutes specified. */
	private final int utcOffsetMinutes;

	/** @return The UTC offset minutes, or -1 if there is no UTC offset hours or minutes specified. */
	public final int getUTCOffsetMinutes()
	{
		return utcOffsetMinutes;
	}

	/** The difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC. */
	private final long time;

	/** @return The difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC. */
	public long getTime()
	{
		return time;
	}

	/** @return <code>true</code> if there are sufficient components to create an {@link ISODate}. */
	public boolean hasDateComponents()
	{
		return getYear() >= 0 && getMonth() >= 0 && getDay() >= 0;
	}

	/** @return <code>true</code> if there are sufficient components to create an {@link ISOTime}. */
	public boolean hasTimeComponents()
	{
		return getHours() >= 0 && getMinutes() >= 0 && getSeconds() >= 0 && getMicroseconds() >= 0;
	}

	/**
	 * Full constructor.
	 * @param year The year (0-9999), or -1 if there is no year specified.
	 * @param month The month (1-12), or -1 if there is no month specified.
	 * @param day The day (1-31), or -1 if there is no day specified.
	 * @param hours The hours (0-23), or -1 if there is no hours specified.
	 * @param minutes The minutes (0-59), or -1 if there is no minutes specified.
	 * @param seconds The seconds (0-60, allowing leap-seconds; see ISO 8601:2004(E) 4.2.1), or -1 if there is no seconds specified.
	 * @param microseconds The microseconds (0-999999), or -1 if there is no microseconds specified.
	 * @param utcOffsetHours The UTC offset hours.
	 * @param utcOffsetMinutes The UTC offset minutes, or -1 if there is no UTC offset hours or minutes specified.
	 */
	public ISOTemporalComponents(final int year, final int month, final int day, final int hours, final int minutes, final int seconds, final int microseconds,
			final int utcOffsetHours, final int utcOffsetMinutes)
	{
		this.year = year;
		this.month = month;
		this.day = day;
		this.hours = hours;
		this.minutes = minutes;
		this.seconds = seconds;
		this.microseconds = microseconds;
		this.utcOffsetHours = utcOffsetHours;
		this.utcOffsetMinutes = utcOffsetMinutes;

		final Calendar calendar = new GregorianCalendar(utcOffsetHours == 0 && utcOffsetMinutes == 0 ? TimeZone.getTimeZone(GMT_ID) : getTimeZone(utcOffsetHours,
				utcOffsetMinutes)); //get Gregorian calendar using the time zone from the UTC offset, defaulting to a GMT time zone
		calendar.clear(); //clear the calendar
		if(year >= 0 && month >= 0 && day >= 0) //if there is date information
		{
			if(hours >= 0 && minutes >= 0 && seconds >= 0 && microseconds >= 0) //if date and time is available
			{
				setDateTime(calendar, checkArgumentRange(year, 0, 9999), checkArgumentRange(month, 1, 12) - 1, checkArgumentRange(day, 1, 31),
						checkArgumentRange(hours, 0, 23), checkArgumentRange(minutes, 0, 59), checkArgumentRange(seconds, 0, 60),
						checkArgumentRange(microseconds, 0, 999999) / 1000); //set the calendar's date and the time, allowing leap-seconds (see ISO 8601:2004(E) 4.2.1); compensate for Calendar's zero-based month
			}
			else
			//if no time is available
			{
				calendar.set(checkArgumentRange(year, 0, 9999), checkArgumentRange(month, 1, 12) - 1, checkArgumentRange(day, 1, 31)); //set the calendar's date
			}
		}
		else if(hours >= 0 && minutes >= 0 && seconds >= 0 && microseconds >= 0) //if only time is available
		{
			setTime(calendar, checkArgumentRange(hours, 0, 23), checkArgumentRange(minutes, 0, 59), checkArgumentRange(seconds, 0, 60),
					checkArgumentRange(microseconds, 0, 999999) / 1000); //set the calendar's time, converting the microseconds to milliseconds
		}
		this.time = calendar.getTimeInMillis(); //get the time from the calendar
	}

	/**
	 * Date constructor in terms of UTC.
	 * @param date The date representing the difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	 * @exception NullPointerException if the given date is <code>null</code>.
	 */
	public ISOTemporalComponents(final Date date)
	{
		this(date.getTime()); //calculate the components from the time
	}

	/**
	 * Date and time zone constructor.
	 * @param date The date representing the difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	 * @param timeZone The time zone in which the time should be interpreted.
	 * @throws NullPointerException if the given date and/or time zone is <code>null</code>.
	 * @throws IllegalArgumentException if a time zone was provided with an unsupported offset for the given time.
	 */
	public ISOTemporalComponents(final Date date, final TimeZone timeZone)
	{
		this(date.getTime(), timeZone); //calculate the components from the time
	}

	/**
	 * Millisecond time constructor in terms of UTC.
	 * @param time The difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	 */
	public ISOTemporalComponents(final long time)
	{
		this(time, TimeZone.getTimeZone(GMT_ID)); //construct the class using UTC
	}

	/**
	 * Millisecond time and time zone constructor. This method cannot handle offsets that do not fall on whole minutes.
	 * @param time The difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	 * @param timeZone The time zone in which the time should be interpreted.
	 * @throws NullPointerException if the given time zone is <code>null</code>.
	 * @throws IllegalArgumentException if a time zone was provided with an unsupported offset for the given time.
	 */
	public ISOTemporalComponents(final long time, final TimeZone timeZone)
	{
		final Calendar calendar = new GregorianCalendar(timeZone); //create a new Gregorian calendar for the given time zone
		calendar.setTimeInMillis(time); //set the time of the calendar to the given time
		this.year = calendar.get(YEAR); //get the components from the calendar
		this.month = calendar.get(MONTH) + 1; //compensate for Calendar's zero-based month
		this.day = calendar.get(DAY_OF_MONTH);
		this.hours = calendar.get(HOUR_OF_DAY);
		this.minutes = calendar.get(MINUTE);
		this.seconds = calendar.get(SECOND);
		this.microseconds = calendar.get(MILLISECOND) * 1000; //convert milliseconds to microseconds
		int offset = timeZone.getOffset(time); //determine the offset for the given time zone
		final boolean negative = offset < 0; //see if the offset is negative
		if(negative) //if the offset is negative
		{
			offset = -offset; //use the postive offset in our calculations
		}
		final int hours = offset / (60 * 60 * 1000); //get the hours
		final int minutes = (offset - (hours * 60 * 60 * 1000)) / (60 * 1000); //determine the minutes
		if(hours * 60 * 60 * 1000 + minutes * 60 * 1000 != offset) //if there were milliseconds, we can't deal with those
		{
			throw new IllegalArgumentException("Cannot get UTF offset for millisecond-level offset " + offset);
		}
		this.utcOffsetHours = negative ? -hours : hours; //set the UTC offset hours and minutes
		this.utcOffsetMinutes = minutes;
		this.time = time; //store the time
	}

	/**
	 * Returns the temporal components as time information.
	 * @return A time object representing the time components, or <code>null</code> if time components are not represented.
	 */
	public ISOTime asTime()
	{
		return hours >= 0 && minutes >= 0 && seconds >= 0 && microseconds >= 0 ? new ISOTime(this) : null; //if we have time information, return a time; otherwise return null
	}

	/**
	 * Returns the temporal components as an ISO time.
	 * @return A time object representing the time and optional UTC offset.
	 */
	public ISOTime toTime()
	{
		return new ISOTime(hours, minutes, seconds, microseconds, asUTCOffset()); //construct a time from the components
	}

	/**
	 * Returns the temporal components as UTC offset information.
	 * @return A UTC offset object representing the UTC offset components, or <code>null</code> if UTC offset components are not represented.
	 */
	public ISOUTCOffset asUTCOffset()
	{
		return utcOffsetMinutes >= 0 ? (utcOffsetHours == 0 && utcOffsetMinutes == 0 ? ISOUTCOffset.UTC : new ISOUTCOffset(utcOffsetHours, utcOffsetMinutes))
				: null; //if we have UTC offset information, return a new UTC offset, using the shared zero offset instance if possible
	}

	/**
	 * Creates a calendar representing the given temporal component information in the given locale.
	 * @param year The year, 0-9999.
	 * @param month The month, 1-12.
	 * @param day The day, 1-31.
	 * @param time The time.
	 * @param locale The locale for the calendar.
	 * @exception NullPointerException if the given time and/or locale is <code>null</code>.
	 * @exception IllegalArgumentException if one of the given arguments is outside the allowed range.
	 */
	public static Calendar createCalendar(final int year, final int month, final int day, final ISOTime time, final Locale locale)
	{
		return createCalendar(year, month, day, time.getHours(), time.getMinutes(), time.getSeconds(), time.getMicroseconds(), time.getUTCOffset(), locale); //create a calendar using the time components
	}

	/**
	 * Creates a calendar representing the given temporal component information.
	 * @param year The year, 0-9999.
	 * @param month The month, 1-12.
	 * @param day The day, 1-31.
	 * @param hours The hours, 0-23.
	 * @param minutes The minutes, 0-59.
	 * @param seconds The seconds, 0-60 (allowing leap-seconds; see ISO 8601:2004(E) 4.2.1).
	 * @param microseconds The microseconds, 0-999999
	 * @param utcOffset The UTC offset, or <code>null</code> if no UTC offset is known.
	 * @param locale The locale for the calendar.
	 * @exception NullPointerException if the given locale is <code>null</code>.
	 * @exception IllegalArgumentException if one of the given arguments is outside the allowed range.
	 */
	public static Calendar createCalendar(final int year, final int month, final int day, final int hours, final int minutes, final int seconds,
			final int microseconds, final ISOUTCOffset utcOffset, final Locale locale)
	{
		final Calendar calendar = new GregorianCalendar(utcOffset != null ? utcOffset.toTimeZone() : GMT, checkInstance(locale, "Locale cannot be null.")); //get Gregorian calendar for the locale using the time zone from the UTC offset, defaulting to a GMT time zone
		calendar.clear(); //clear the calendar
		return setDateTime(calendar, checkArgumentRange(year, 0, 9999), checkArgumentRange(month, 1, 12) - 1, checkArgumentRange(day, 1, 31),
				checkArgumentRange(hours, 0, 23), checkArgumentRange(minutes, 0, 59), checkArgumentRange(seconds, 0, 60),
				checkArgumentRange(microseconds, 0, 999999) / 1000); //set the calendar's date and the time, allowing leap-seconds (see ISO 8601:2004(E) 4.2.1); compensate for Calendar's zero-based month
	}

	/**
	 * Parses a date, time, date time, and/or UTC offset lexical form from a string. The first character must be that of the beginning date/time character, and
	 * there must be no characters after the date/time representation.
	 * @param string The string the contents of which to be parsed.
	 * @param hasDate Whether this lexical representation has a date component.
	 * @param hasTime Whether this lexical representation has a time component.
	 * @return The temporal components parsed from the reader.
	 * @exception NullPointerException if the given string is <code>null</code>.
	 * @exception SyntaxException if the date/time is not of the correct format.
	 */
	static ISOTemporalComponents parseDateTimeUTCOffset(final String string, final boolean hasDate, final boolean hasTime) throws SyntaxException
	{
		return parseDateTimeUTCOffset(string, hasDate, hasTime, false, false, true); //parse the temporal components, requiring strict ISO format
	}

	/**
	 * Parses a date, time, date time, and/or UTC offset lexical form from a string. Unless lenient parsing is requested, the first character must be that of the
	 * beginning date/time character, and there must be no characters after the date/time representation.
	 * <p>
	 * Lenient parsing makes the following allowances:
	 * <ul>
	 * <li>Seconds are considered optional.</li>
	 * <li>Whitespace before and after the date/time is allowed.</li>
	 * <li>The looser RFC 3339 Internet timestamp format is allowed, allowing the UTC designator, {@value ISO8601#UTC_DESIGNATOR}, for example.</li>
	 * </ul>
	 * </p>
	 * @param string The string the contents of which to be parsed.
	 * @param hasDate Whether this lexical representation has a date component.
	 * @param hasTime Whether this lexical representation has a time component, or <code>null</code> if time is optional.
	 * @param allowTimestampFormat Whether the looser RFC 3339 Internet timestamp format is allowed, allowing the UTC designator, {@value ISO8601#UTC_DESIGNATOR},
	 *          for example.
	 * @param lenient If the date/time components should be parsed leniently, allowing any non-ambiguous representation of the values.
	 * @return The temporal components parsed from the reader.
	 * @exception NullPointerException if the given string is <code>null</code>.
	 * @exception SyntaxException if the date/time is not of the correct format.
	 */
	static ISOTemporalComponents parseDateTimeUTCOffset(final String string, final boolean hasDate, final Boolean hasTime, final boolean allowTimestampFormat,
			final boolean lenient, final boolean requireDelimiters) throws SyntaxException
	{
		try
		{
			final Reader reader = new StringReader(string); //create a new string reader from the string
			if(lenient) //if we're parsing leniently
			{
				skip(reader, WHITESPACE_CHARACTERS); //skip whitespace
			}
			final ISOTemporalComponents temporalComponents = parseDateTimeUTCOffset(reader, hasDate, hasTime, allowTimestampFormat, lenient, requireDelimiters); //parse the date/time components
			if(lenient) //if we're parsing leniently
			{
				skip(reader, WHITESPACE_CHARACTERS); //skip whitespace
			}
			checkReaderEnd(reader); //make sure we're at the end of the reader
			return temporalComponents; //return the temporal components
		}
		catch(final IOException ioException) //if there is an I/O exception (likely from a parse error)
		{
			throw new SyntaxException(ioException);
		}
	}

	/**
	 * Parses a date, time, date time, and/or UTC offset lexical form from a reader. This method restricts the syntax to a IS0 8601 subset. The current position
	 * must be that of the beginning date/time character. The new position will be that immediately after the last date/time character. If neither a date nor a
	 * time are requested, a UTC offset is required. Otherwise, a UTC offset is allowed unless only a date is requested.
	 * @param reader The reader the contents of which to be parsed.
	 * @param hasDate Whether this lexical representation has a date component.
	 * @param hasTime Whether this lexical representation has a time component.
	 * @return The temporal components parsed from the reader.
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 * @exception ParseIOException if the reader has no more characters before the current date/time is completely parsed.
	 * @exception SyntaxException if the date/time is not of the correct format.
	 */
	public static ISOTemporalComponents parseDateTimeUTCOffset(final Reader reader, final boolean hasDate, final boolean hasTime) throws IOException,
			ParseIOException, SyntaxException
	{
		return parseDateTimeUTCOffset(reader, hasDate, hasTime, false, false, true); //parse the temporal components, requiring strict ISO format
	}

	/**
	 * Parses a date, time, date time, and/or UTC offset lexical form from a reader. The current position must be that of the beginning date/time character. The
	 * new position will be that immediately after the last date/time character. If neither a date nor a time are requested, a UTC offset is required. Otherwise,
	 * a UTC offset is allowed unless only a date is requested.
	 * <p>
	 * Lenient parsing makes the following allowances:
	 * <ul>
	 * <li>Seconds are considered optional.</li>
	 * <li>The looser RFC 3339 Internet timestamp format is allowed, allowing the UTC designator, {@value ISO8601#UTC_DESIGNATOR}, for example.</li>
	 * </ul>
	 * </p>
	 * @param reader The reader the contents of which to be parsed.
	 * @param hasDate Whether this lexical representation has a date component.
	 * @param hasTime Whether this lexical representation has a time component, or <code>null</code> if time is optional.
	 * @param allowTimestampFormat Whether the looser RFC 3339 Internet timestamp format is allowed, allowing the UTC designator, {@value ISO8601#UTC_DESIGNATOR},
	 *          for example.
	 * @param lenient If the date/time components should be parsed leniently, allowing any non-ambiguous representation of the values.
	 * @param requireDelimiters Whether delimiters are required.
	 * @return The temporal components parsed from the reader.
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 * @exception ParseIOException if the reader has no more characters before the current date/time is completely parsed.
	 * @exception SyntaxException if the date/time is not of the correct format.
	 * @see <a href="http://www.ietf.org/rfc/rfc3339.txt">RFC 3339</a>
	 * @see <a href="http://www.ietf.org/rfc/rfc2518.txt">RFC 2518</a>
	 * @see <a href="http://www.w3.org/TR/NOTE-datetime">W3C Date and Time Formats</a>
	 */
	static ISOTemporalComponents parseDateTimeUTCOffset(final Reader reader, final boolean hasDate, Boolean hasTime, final boolean allowTimestampFormat,
			final boolean lenient, final boolean requireDelimiters) throws IOException, ParseIOException, SyntaxException
	{
		final int year;
		final int month;
		final int day;
		final int hours;
		final int minutes;
		final int seconds;
		final int microseconds;
		final int utcOffsetHours;
		final int utcOffsetMinutes;
		try
		{
			if(hasDate) //if we should parse a date
			{
				year = Integer.parseInt(readStringCheck(reader, 4, '0', '9')); //read the year
				if(requireDelimiters || peek(reader) == DATE_DELIMITER)
				{
					check(reader, DATE_DELIMITER); //check the date delimiter
				}
				month = Integer.parseInt(readStringCheck(reader, 2, '0', '9')); //read the month
				if(requireDelimiters || peek(reader) == DATE_DELIMITER)
				{
					check(reader, DATE_DELIMITER); //check the date delimiter
				}
				day = Integer.parseInt(readStringCheck(reader, 2, '0', '9')); //read the day
				if(hasTime == null) //if we should check to see if there is a time
				{
					hasTime = peekEnd(reader) == TIME_BEGIN; //determine whether we have time based upon the presence of the introductory time delimiter
				}
			}
			else
			//if we shouldn't parse a date
			{
				year = -1; //set the date values to invalid
				month = -1;
				day = -1;
			}
			if(hasTime) //if we should parse a time
			{
				if(hasDate) //if there is both a date and a time
				{
					check(reader, TIME_BEGIN); //check the beginning of the time section
				}
				hours = Integer.parseInt(readStringCheck(reader, 2, '0', '9')); //read the hours
				if(requireDelimiters || peek(reader) == TIME_DELIMITER)
				{
					check(reader, TIME_DELIMITER); //check the time delimiter
				}
				minutes = Integer.parseInt(readStringCheck(reader, 2, '0', '9')); //read the minutes
				if(!lenient || peek(reader) == TIME_DELIMITER) //if there are seconds (seconds are only optional if we are parsing leniently)
				{
					check(reader, TIME_DELIMITER); //check the time delimiter
					seconds = Integer.parseInt(readStringCheck(reader, 2, '0', '9')); //read the seconds
				}
				else if(!requireDelimiters && isPeek(reader, '0', '9')) //if we don't require delimiters and there is a digit
				{
					seconds = Integer.parseInt(readStringCheck(reader, 2, '0', '9')); //read the seconds
				}
				else
				//if this is a lenient parsing and there are no seconds
				{
					seconds = 0; //conclude no seconds
				}
				if(confirm(reader, TIME_SUBSECONDS_DELIMITER)) //if there are subseconds
				{
					microseconds = Integer.parseInt(makeStringLength(readMinimum(reader, 1, '0', '9'), 6, '0', -1)); //read all subseconds, converting the precision to six digits
				}
				else
				//if there are no microseconds
				{
					microseconds = 0;
				}
			}
			else
			//if we shouldn't parse a time
			{
				hours = -1; //set the time values to invalid
				minutes = -1;
				seconds = -1;
				microseconds = -1;
			}
			if(hasDate && !hasTime) //the only type not to parse a UTC offset is a date with no time
			{
				utcOffsetHours = -1; //set the UTC offset values to invalid
				utcOffsetMinutes = -1;
			}
			else
			//if we should at least allow a UTC offset
			{
				final int utcOffsetDelimiter = peekEnd(reader); //peek the next character
				if(utcOffsetDelimiter == '+' || utcOffsetDelimiter == '-') //if this is the start of a UTC offset
				{
					check(reader, (char)utcOffsetDelimiter); //read the delimiter
					final StringBuilder utcOffsetStringBuilder = new StringBuilder(3); //create a new string builder for just enough room for a sign and the offset hours
					if(utcOffsetDelimiter == '-') //if this was the negative sign (don't append the positive sign, because Integer.parseInt doesn't allow it)
					{
						utcOffsetStringBuilder.append((char)utcOffsetDelimiter); //append the negative sign
					}
					utcOffsetStringBuilder.append(readStringCheck(reader, 2, '0', '9')); //read the UTC offset hours
					utcOffsetHours = Integer.parseInt(utcOffsetStringBuilder.toString()); //parse the UTC offset hours
					check(reader, TIME_DELIMITER); //check the time delimiter
					utcOffsetMinutes = Integer.parseInt(readStringCheck(reader, 2, '0', '9')); //read the UTC offset minutes
				}
				else if(allowTimestampFormat && utcOffsetDelimiter == UTC_DESIGNATOR) //if we allow the UTC designator, and this character is the UTC designator
				{
					check(reader, UTC_DESIGNATOR); //read the UTC designator
					utcOffsetHours = 0; //Zulu time is equivalent to +00:00
					utcOffsetMinutes = 0;
				}
				else
				//if we shouldn't parse a UTC offset
				{
					if(!hasDate && !hasTime) //if neither a date nor a time were requested, require a UTC offset
					{
						checkReaderEnd(reader); //make sure we're not at the end of the reader
						throw new ParseIOException(reader, "Expected one of " + Arrays.toString(SIGNS) + "; found " + (char)utcOffsetDelimiter + ".");
					}
					utcOffsetHours = -1; //set the UTC offset values to invalid
					utcOffsetMinutes = -1;
				}
			}
		}
		catch(final NumberFormatException numberFormatException) //if a  number wasn't formatted correctly
		{
			throw new SyntaxException(numberFormatException);
		}
		return new ISOTemporalComponents(year, month, day, hours, minutes, seconds, microseconds, utcOffsetHours, utcOffsetMinutes);
	}
}
