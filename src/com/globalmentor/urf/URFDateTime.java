package com.globalmentor.urf;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;

import com.globalmentor.text.*;
import com.globalmentor.util.Calendars;

/**The class representing an <code>urf.DateTime</code> type.
If there is no explicit UTC offset (i.e. this is a floating value), the time is stored internally in terms of UTC.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class URFDateTime extends AbstractURFDateTime
{

	/**Default constructor of a floating date time with the current time in terms of UTC.*/
	public URFDateTime()
	{
		this(System.currentTimeMillis());	//construct the class with the current time in milliseconds
	}

	/**Temporal component constructor.
	@param temporalcomponents The temporal components from which to construct the class.
	@exception NullPointerException if the given temporal components is <code>null</code>.
	*/
	protected URFDateTime(final URFTemporalComponents temporalComponents)
	{
		super(temporalComponents, true);	//construct the parent class, using the time information
	}

	/**Date components and time constructor.
	@param year The year, 0-9999.
	@param month The month, 1-12.
	@param day The day, 1-31.
	@param time The time.
	@exception NullPointerException if the given time is <code>null</code>.
	@exception IllegalArgumentException if one of the given arguments is outside the allowed range.
	*/
	public URFDateTime(final int year, final int month, final int day, final URFTime time)
	{
		this(new URFTemporalComponents(year, month, day, time.getHours(), time.getMinutes(), time.getSeconds(), time.getMicroseconds(), time.getUTCOffset()!=null ? time.getUTCOffset().getHours() : -1, time.getUTCOffset()!=null ? time.getUTCOffset().getMinutes() : -1));	//construct the class with only a date in UTC
	}

	/**Full Constructor.
	@param year The year, 0-9999.
	@param month The month, 1-12.
	@param day The day, 1-31.
	@param hours The hours, 0-23.
	@param minutes The minutes, 0-59.
	@param seconds The seconds, 0-60 (allowing leap-seconds; see ISO 8601:2004(E) 4.2.1).
	@param microseconds The microseconds, 0-999999.
	@param utcOffset The UTC offset, or <code>null</code> if no UTC offset is known.
	@exception IllegalArgumentException if one of the given arguments is outside the allowed range.
	*/
	public URFDateTime(final int year, final int month, final int day, final int hours, final int minutes, final int seconds, final int microseconds, final URFUTCOffset utcOffset)
	{
		this(year, month, day, new URFTime(hours, minutes, seconds, microseconds, utcOffset));
	}

	/**Date constructor in terms of UTC.
	@param date The date representing the difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	@exception NullPointerException if the given date is <code>null</code>.
	*/
	public URFDateTime(final Date date)
	{
		this(new URFTemporalComponents(date));	//construct the class from temporal components
	}

	/**Date constructor.
	@param date The date representing the difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	@param timeZone The time zone in which the time should be interpreted.
	@throws NullPointerException if the given date and/or time zone is <code>null</code>.
	@throws IllegalArgumentException if a time zone was provided with an unsupported offset for the given time.
	*/
	public URFDateTime(final Date date, final TimeZone timeZone)
	{
		this(new URFTemporalComponents(date, timeZone));	//construct the class from temporal components
	}

	/**Millisecond time constructor in terms of UTC.
	@param time The difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	*/
	public URFDateTime(final long time)
	{
		this(new URFTemporalComponents(time));	//construct the class from temporal components
	}

	/**Millisecond time constructor in terms of UTC.
	@param time The difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	@param timeZone The time zone in which the time should be interpreted.
	@throws NullPointerException if the given time zone is <code>null</code>.
	@throws IllegalArgumentException if a time zone was provided with an unsupported offset for the given time.
	*/
	public URFDateTime(final long time, final TimeZone timeZone)
	{
		this(new URFTemporalComponents(time, timeZone));	//construct the class from temporal components
	}

	/**Returns a date that represents this temporal information in the given time zone.
	@param timeZone The time zone which the date should represent.
	@return The date this object represents in relation to the given time zone.
	@throws NullPointerException if the given time zone is <code>null</code>.
	*/
	public Date toDate(final TimeZone timeZone)
	{
		final Calendar calendar=new GregorianCalendar(timeZone);	//create a Gregorian calendar for the given time zone
		calendar.clear();	//clear the calendar
		calendar.set(getYear(), getMonth()-1, getDay());	//set the calendar date, compensating for Calendar's zero-based month
		final URFTime time=getURFTime();	//get the URF time, if any
		if(time!=null)	//if we have time
		{
			Calendars.setTime(calendar, time.getHours(), time.getMinutes(), time.getSeconds(), time.getMicroseconds()/1000);	//set the time
		}
		return calendar.getTime();	//return the calendar time
	}

	/**Returns an URF date time object holding the value of the specified string.
	@param string The string to be parsed as a date time.
	@return An URF date time object represented by the string.
	@exception NullPointerException if the given string is <code>null</code>
	@exception ArgumentSyntaxException if the given string does not have the correct syntax.
	*/
	public static URFDateTime valueOf(final String string) throws ArgumentSyntaxException
	{
		try
		{
			return new URFDateTime(URFTemporalComponents.parseDateTimeUTCOffset(string, true, true));	//parse temporal components for both the date and the time and use that to create a new date time object
		}
		catch(final SyntaxException syntaxException)	//if the syntax of the string was not correct
		{
			throw new ArgumentSyntaxException(syntaxException);
		}
	}

	/**Returns an URF date time object holding the value of the specified string.
	The looser RFC 3339 Internet timestamp format is allowed, which is used in "W3C Date and Time Formats"
	as well as portions of WebDAV.
	@param string The string to be parsed as a date time.
	@return An URF date time object represented by the string.
	@exception NullPointerException if the given string is <code>null</code>
	@exception ArgumentSyntaxException if the given string does not have the correct syntax.
	@see <a href="http://www.ietf.org/rfc/rfc3339.txt">RFC 3339</a>
	@see <a href="http://www.ietf.org/rfc/rfc2518.txt">RFC 2518</a>
	@see <a href="http://www.w3.org/TR/NOTE-datetime">W3C Date and Time Formats</a>
	*/
	public static URFDateTime valueOfTimestamp(final String string) throws ArgumentSyntaxException
	{
		try
		{
			return new URFDateTime(URFTemporalComponents.parseDateTimeUTCOffset(string, true, true, true));	//parse temporal components for both the date and the time, allowing RFC 3339 format, and use that to create a new date time object
		}
		catch(final SyntaxException syntaxException)	//if the syntax of the string was not correct
		{
			throw new ArgumentSyntaxException(syntaxException);
		}
	}

}
