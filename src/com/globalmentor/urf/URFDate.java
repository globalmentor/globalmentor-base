package com.globalmentor.urf;

import java.util.Date;
import java.util.TimeZone;

import com.globalmentor.text.*;

/**The class representing an <code>urf.Date</code> type.
If there is no explicit UTC offset (i.e. this is a floating value), the time is stored internally in terms of UTC.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class URFDate extends AbstractURFDateTime
{

	/**Temporal component constructor.
	@param temporalcomponents The temporal components from which to construct the class.
	@exception NullPointerException if the given temporal components is <code>null</code>.
	*/
	private URFDate(final URFTemporalComponents temporalComponents)
	{
		super(temporalComponents);	//construct the parent class
	}

	/**Date components constructor.
	The underlying {@link Date} will be constructed in terms of UTC.
	@param year The year, 0-9999.
	@param month The month, 1-12.
	@param day The day, 1-31.
	@exception IllegalArgumentException if one of the given arguments is outside the allowed range.
	*/
	public URFDate(final int year, final int month, final int day)
	{
		this(new URFTemporalComponents(year, month, day, -1, -1, -1, -1, 0, 0));	//construct the parent class with only a date in UTC
	}

	/**Date constructor in terms of UTC.
	Any time-related information of the given date will be lost; only the date will be kept, in terms of midnight UTC.
	@param date The date representing the difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	@exception NullPointerException if the given date is <code>null</code>.
	*/
	public URFDate(final Date date)
	{
		this(new URFTemporalComponents(date));	//construct the class from temporal components
	}

	/**Date constructor in terms the gigen time zone.
	Any time-related information of the given date will be lost; only the date will be kept, in terms of midnight UTC.
	@param date The date representing the difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	@param timeZone The time zone in which the time should be interpreted.
	@throws NullPointerException if the given date and/or time zone is <code>null</code>.
	@throws IllegalArgumentException if a time zone was provided with an unsupported offset for the given time.
	*/
	public URFDate(final Date date, final TimeZone timeZone)
	{
		this(new URFTemporalComponents(date, timeZone));	//construct the class from temporal components
	}

	/**Millisecond time constructor in terms of UTC.
	Any time-related information of the given time will be lost; only the date will be kept, in terms of midnight UTC.
	@param time The difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	*/
	public URFDate(final long time)
	{
		this(new URFTemporalComponents(time));	//construct the class from temporal components
	}

	/**Millisecond time constructor in terms of the given time zone.
	Any time-related information of the given time will be lost; only the date will be kept, in terms of the given time zone.
	@param time The difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	@param timeZone The time zone in which the time should be interpreted.
	@throws NullPointerException if the given time zone is <code>null</code>.
	@throws IllegalArgumentException if a time zone was provided with an unsupported offset for the given time.
	*/
	public URFDate(final long time, final TimeZone timeZone)
	{
		this(new URFTemporalComponents(time, timeZone));	//construct the class from temporal components
	}

	/**Returns an URF date object holding the value of the specified string.
	@param string The string to be parsed as a date.
	@return An URF date object represented by the string.
	@exception NullPointerException if the given string is <code>null</code>
	@exception ArgumentSyntaxException if the given string does not have the correct syntax.
	*/
	public static URFDate valueOf(final String string) throws ArgumentSyntaxException
	{
		try
		{
			return new URFDate(URFTemporalComponents.parseDateTimeUTCOffset(string, true, false));	//parse temporal components with only a date and use that to create a new date object
		}
		catch(final SyntaxException syntaxException)	//if the syntax of the string was not correct
		{
			throw new ArgumentSyntaxException(syntaxException);
		}
	}

}
