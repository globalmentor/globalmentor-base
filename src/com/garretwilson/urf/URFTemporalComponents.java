package com.garretwilson.urf;

import java.io.*;
import java.util.*;

import static java.util.Calendar.*;

import com.garretwilson.io.ParseIOException;
import com.garretwilson.lang.IntegerUtilities;

import static com.garretwilson.io.ReaderParser.*;
import static com.garretwilson.lang.StringUtilities.*;
import com.garretwilson.text.SyntaxException;
import static com.garretwilson.util.TimeZoneConstants.*;

import static com.garretwilson.urf.URF.*;

/**A lightweight structure for transferring components of URF temporal types.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class URFTemporalComponents
{

	/**The year, or -1 if there is no year specified.*/
	private final int year;

		/**@return The year, or -1 if there is no year specified.*/
		public final int getYear() {return year;}

	/**The month, or -1 if there is no month specified.*/
	private final int month;

		/**@return The month, or -1 if there is no month specified.*/
		public final int getMonth() {return month;}

	/**The day, or -1 if there is no day specified.*/
	private final int day;

		/**@return The day, or -1 if there is no day specified.*/
		public final int getDay() {return day;}

	/**The hours, or -1 if there is no hours specified.*/
	private final int hours;

		/**@return The hours, or -1 if there is no hours specified.*/
		public final int getHours() {return hours;}

	/**The minutes, or -1 if there is no minutes specified.*/
	private final int minutes;

		/**@return The minutes, or -1 if there is no minutes specified.*/
		public final int getMinutes() {return minutes;}

	/**The seconds, or -1 if there is no seconds specified.*/
	private final int seconds;

		/**@return The seconds, or -1 if there is no seconds specified.*/
		public final int getSeconds() {return seconds;}

	/**The microseconds, or -1 if there is no microseconds specified.*/
	private final int microseconds;

		/**@return The microseconds, or -1 if there is no microseconds specified.*/
		public final int getMicroseconds() {return microseconds;}

	/**The UTC offset hours.*/
	private final int utcOffsetHours;

		/**@return The UTC offset hours.*/
		public final int getUTCOffsetHours() {return utcOffsetHours;}

	/**The UTC offset minutes, or -1 if there is no UTC offset hours or minutes specified.*/
	private final int utcOffsetMinutes;

		/**@return The UTC offset minutes, or -1 if there is no UTC offset hours or minutes specified.*/
		public final int getUTCOffsetMinutes() {return utcOffsetMinutes;}

	/**Full constructor.
	@param year The year, or -1 if there is no year specified.
	@param month The month, or -1 if there is no month specified.
	@param day The day, or -1 if there is no day specified.
	@param hours The hours , or -1 if there is no hours specified.
	@param minutes The minutes, or -1 if there is no minutes specified.
	@param seconds The seconds, or -1 if there is no seconds specified.
	@param microseconds The microseconds, or -1 if there is no microseconds specified.
	@param utcOffsetHours The UTC offset hours.
	@param utcOffsetMinutes The UTC offset minutes, or -1 if there is no UTC offset hours or minutes specified.
	*/
	public URFTemporalComponents(final int year, final int month, final int day, final int hours, final int minutes, final int seconds, final int microseconds, final int utcOffsetHours, final int utcOffsetMinutes)
	{
		this.year=year;
		this.month=month;
		this.day=day;
		this.hours=hours;
		this.minutes=minutes;
		this.seconds=seconds;
		this.microseconds=microseconds;
		this.utcOffsetHours=utcOffsetHours;
		this.utcOffsetMinutes=utcOffsetMinutes;
	}

	/**Date constructor.
	@param date The date representing the difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	@exception NullPointerException if the given date is <code>null</code>.
	*/
	public URFTemporalComponents(final Date date)
	{
		this(date.getTime());	//calculate the components from the time
	}

	/**Millisecond time constructor.
	@param time The difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	*/
	public URFTemporalComponents(final long time)
	{
		final Calendar calendar=new GregorianCalendar(TimeZone.getTimeZone(GMT_ID));	//create a new Gregorian calendar for the UTC time zone
		calendar.setTimeInMillis(time);	//set the time of the calendar to the given time
		this.year=calendar.get(YEAR);	//get the components from the calendar
		this.month=calendar.get(MONTH);
		this.day=calendar.get(DAY_OF_MONTH);
		this.hours=calendar.get(HOUR_OF_DAY);
		this.minutes=calendar.get(MINUTE);
		this.seconds=calendar.get(SECOND);
		this.microseconds=calendar.get(MILLISECOND)*1000;	//convert milliseconds to microseconds
		this.utcOffsetHours=0;	//this is UTC, so the offset is zero
		this.utcOffsetMinutes=0;
	}

	/**Returns the temporal components as UTC offset information.
	@return A UTC offset object representing the UTC offset components, or <code>null</code> if UTC offset components are not represented.
	*/
	public URFUTCOffset asUTCOffset()
	{
		return utcOffsetMinutes>=0 ? (utcOffsetHours==0 && utcOffsetMinutes==0 ? URFUTCOffset.UTC_OFFSET_UTC : new URFUTCOffset(utcOffsetHours, utcOffsetMinutes)) : null;	//if we have UTC offset information, return a new UTC offset, using the shared zero offset instance if possible
	}

	/**Creates a calendar representing the given temporal component information.
	@param year The year, 0-9999.
	@param month The month, 1-12.
	@param day The day, 1-31.
	@param hours The hours, 0-23.
	@param minutes The minutes, 0-59.
	@param seconds The seconds, 0-59.
	@param microseconds The microseconds, 0-999999
	@param utcOffset The UTC offset, or <code>null</code> if no UTC offset is known.
	*/
	public static Calendar createCalendar(final int year, final int month, final int day, final int hours, final int minutes, final int seconds, final int microseconds, final URFUTCOffset utcOffset)
	{
		final Calendar calendar=new GregorianCalendar(utcOffset!=null ? utcOffset.toTimeZone() : URFUTCOffset.TimeZone_GMT);	//get Gregorian calendar using the time zone from the UTC offset, defaulting to a GMT time zone
		calendar.clear();	//clear the calendar
		calendar.set(year, month, day, hours, minutes, seconds);	//set the calendar's date and the time
		calendar.set(MILLISECOND, microseconds/1000);	//set the calendar's milliseconds, converting the microseconds to milliseconds
		return calendar;	//return the calendar we created
	}

 	/**Parses a date/time lexical form from a string.
	The first characer must be that of the beginning date/time character, and there must be no characters after the date/time representation.
	@param string The string the contents of which to be parsed.
	@param hasDate Whether this lexical representation has a date component.
	@param hasTime  Whether this lexical representation has a time component.
	@return The temporal components parsed from the reader.
	@exception NullPointerException if the given string is <code>null</code>.
	@exception SyntaxException if the date/time is not of the correct format.
	*/
	public static URFTemporalComponents parseDateTime(final String string, final boolean hasDate, final boolean hasTime) throws SyntaxException
	{
		try
		{
			final Reader reader=new StringReader(string);	//create a new string reader from the string
			final URFTemporalComponents temporalComponents=parseDateTimeUTCOffset(reader, hasDate, hasTime);	//parse the date/time components
			checkReaderEnd(reader);	//make sure we're at the end of the reader
			return temporalComponents;	//return the temporal components
		}
		catch(final IOException ioException)	//if there is an I/O exception (likely from a parse error)
		{
			throw new SyntaxException(ioException);
		}
	}

 	/**Parses a date, time, date time, and/or UTC offset lexical form from a reader.
	The current position must be that of the beginning date/time character.
	The new position will be that immediately after the last date/time character.
	If neither a date nor a time are requested, a UTC offset is required.
	Otherwise, a UTC offset is allowed unless only a date is requested.
	@param reader The reader the contents of which to be parsed.
	@param hasDate Whether this lexical representation has a date component.
	@param hasTime  Whether this lexical representation has a time component.
	@return The temporal components parsed from the reader.
	@exception NullPointerException if the given reader is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	@exception ParseIOException if the reader has no more characters before the current date/time is completely parsed.
	@exception SyntaxException if the date/time is not of the correct format.
	*/
	public static URFTemporalComponents parseDateTimeUTCOffset(final Reader reader, final boolean hasDate, final boolean hasTime) throws IOException, ParseIOException, SyntaxException
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
			if(hasDate)	//if we should parse a date
			{
				year=Integer.parseInt(readStringCheck(reader, 4, '0', '9')); //read the year
				check(reader, DATE_DELIMITER);	//check the date delimiter
				month=Integer.parseInt(readStringCheck(reader, 2, '0', '9')); //read the month
				check(reader, DATE_DELIMITER);	//check the date delimiter
				day=Integer.parseInt(readStringCheck(reader, 2, '0', '9')); //read the day
			}
			else	//if we shouldn't parse a date
			{
				year=-1;	//set the date values to invalid
				month=-1;
				day=-1;
			}
			if(hasTime)	//if we should parse a time
			{
				if(hasDate)	//if there is both a date and a time
				{
					check(reader, TIME_BEGIN);	//check the begining of the time section
				}
				hours=Integer.parseInt(readStringCheck(reader, 2, '0', '9')); //read the hours
				check(reader, TIME_DELIMITER);	//check the time delimiter
				minutes=Integer.parseInt(readStringCheck(reader, 2, '0', '9')); //read the minutes
				check(reader, TIME_DELIMITER);	//check the time delimiter
				seconds=Integer.parseInt(readStringCheck(reader, 2, '0', '9')); //read the seconds
				if(confirm(reader, DECIMAL_DELIMITER))	//if there are subseconds
				{
					microseconds=Integer.parseInt(makeStringLength(readMinimum(reader, 1, '0', '9'), 6, '0', -1)); //read all subseconds, converting the precision to six digits
				}
				else	//if there are no microseconds
				{
					microseconds=-1;
				}
			}
			else	//if we shouldn't parse a time
			{
				hours=-1;	//set the time values to invalid
				minutes=-1;
				seconds=-1;
				microseconds=-1;
			}
			if(hasDate && !hasTime)	//the only type not to parse a UTC offset is a date with no time
			{
				utcOffsetHours=-1;	//set the UTC offset values to invalid
				utcOffsetMinutes=-1;
			}
			else	//if we should at least allow a UTC offset
			{
				final int sign=peek(reader);	//peek the next character
				if(sign=='+' || sign=='-')	//if this is the start of a UTC offset
				{
					final StringBuilder utcOffsetStringBuilder=new StringBuilder(3);	//create a new string builder for just enough room for a sign and the offset hours
					utcOffsetStringBuilder.append(sign);	//append the sign to the string
					utcOffsetStringBuilder.append(readStringCheck(reader, 2, '0', '9')); //read the UTC offset hours
					utcOffsetHours=Integer.parseInt(utcOffsetStringBuilder.toString());	//parse the UTC offset hours
					check(reader, TIME_DELIMITER);	//check the time delimiter
					utcOffsetMinutes=Integer.parseInt(readStringCheck(reader, 2, '0', '9')); //read the UTC offset minutes
				}
				else	//if we shouldn't parse a UTC offset
				{
					if(!hasDate && !hasTime)	//if neither a date nor a time were requested, require a UTC offset
					{
						checkReaderEnd(reader);	//make sure we're not at the end of the reader
						throw new ParseIOException(reader, "Expected one of "+Arrays.toString(SIGNS)+"; found "+(char)sign+".");
					}
					utcOffsetHours=-1;	//set the UTC offset values to invalid
					utcOffsetMinutes=-1;
				}
			}
		}
		catch(final NumberFormatException numberFormatException)	//if a  number wasn't formatted correctly
		{
			throw new SyntaxException(numberFormatException);
		}
		return new URFTemporalComponents(year, month, day, hours, minutes, seconds, microseconds, utcOffsetHours, utcOffsetMinutes);
	}
}
