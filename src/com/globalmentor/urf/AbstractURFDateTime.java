package com.globalmentor.urf;

import java.util.*;


import com.globalmentor.java.Integers;

import static com.globalmentor.iso.ISO8601.*;
import static com.globalmentor.java.Integers.*;

/**The abstract base type for <code>urf.Date</code> and <code>urf.DateTime</code> types.
If there is no explicit UTC offset (i.e. this is a floating value), the time is stored internally in terms of UTC.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public abstract class AbstractURFDateTime extends Date implements URFTemporal
{

	/**The year.*/
	private final int year;

		/**@return The year.*/
		public final int getYear() {return year;}

	/**The month.*/
	private final int month;

		/**@return The month.*/
		public final int getMonth() {return month;}

	/**The day.*/
	private final int day;

		/**@return The day.*/
		public final int getDay() {return day;}

	/**The time, or <code>null</code> if there is a date with no time (not even midnight)*/
	private final URFTime time;

		/**@return The time, or <code>null</code> if there is a date with no time (not even midnight)*/
		public URFTime getURFTime() {return time;}

	/**Date components and time constructor with a date specified in epoch terms.
	@param year The year, 0-9999.
	@param month The month, 1-12.
	@param day The day, 1-31.
	@param time The time, or <code>null</code> if there is a date with no time (not even midnight).
	@param date The milliseconds since January 1, 1970, 00:00:00 GMT.
	@exception IllegalArgumentException if one of the given arguments is outside the allowed range.
	*/
/*TODO del
	protected AbstractURFDateTime(final int year, final int month, final int day, final URFTime time, final long date)
	{
		super(date);	//construct the parent class with the date in epoch terms
		this.year=checkRange(year, 0, 9999);
		this.month=checkRange(month, 1, 12);
		this.day=checkRange(day, 1, 31);
		this.time=time;
	}
*/

	/**Date components and time constructor in terms of UTC.
	@param year The year, 0-9999.
	@param month The month, 1-12.
	@param day The day, 1-31.
	@param time The time, or <code>null</code> if there is a date with no time (not even midnight).
	@exception IllegalArgumentException if one of the given arguments is outside the allowed range.
	*/
/*TODO del
	protected AbstractURFDateTime(final int year, final int month, final int day, final URFTime time)
	{
		this(year, month, day, time, URFTemporalComponents.createCalendar(year, month, day, time!=null ? time : URFTime.MIDNIGHT_UTC, Locale.ENGLISH).getTimeInMillis());	//construct class with the time in milleconds the given information represents, using midnight UTC if no time was given; the locale shouldn't matter for just determining the time in milliseconds, so use a locale that is likely to be available
	}
*/

	/**Temporal components constructor.
	@param temporalComponents The components of the time information.
	@param useTime <code>true</code> if the time should be used, or <code>false</code> if the given type components should be ignored.
	@throws NullPointerException if the temporal components is null.
	*/
	protected AbstractURFDateTime(final URFTemporalComponents temporalComponents, final boolean useTime)
	{
		super(temporalComponents.getTime());	//construct the parent class with the date time in milliseconds
		this.year=temporalComponents.getYear();
		this.month=temporalComponents.getMonth();
		this.day=temporalComponents.getDay();
		this.time=useTime ? temporalComponents.asTime() : null;	//store time if requested and if possible
	}

	/**Appends the canonical lexical representation of this date time to a string builder in the form "YYYY-MM-DDThh:mm:ss[.s+]+/-hh:mm".
	@param stringBuild The string builder to which the lexical representation will be appended.
	@return The string builder.
	*/
	public StringBuilder append(final StringBuilder stringBuilder)
	{
		stringBuilder.append(Integers.toString(getYear(), 10, 4));	//append the year, using four digits
		stringBuilder.append(DATE_DELIMITER);	//append the date delimiter
		stringBuilder.append(Integers.toString(getMonth(), 10, 2));	//append the month, using two digits
		stringBuilder.append(DATE_DELIMITER);	//append the date delimiter
		stringBuilder.append(Integers.toString(getDay(), 10, 2));	//append the day, using two digits
		final URFTime time=getURFTime();	//get the time, if any
		if(time!=null)	//if there is a time
		{
			stringBuilder.append(TIME_BEGIN);	//indicate that the time is beginning
			time.append(stringBuilder);	//append the time
		}
		return stringBuilder;	//return the string builder				
	}

	/**Returns a calendar representing this date and time.
	If this object has no time information, midnight UTC will be assumed.
	@param locale The locale for which a calendar should be returned.
	@return A calendar representing this date time in the given locale.
	@exception NullPointerException if the given locale is <code>null</code>.
	*/
	public Calendar toCalendar(final Locale locale)
	{
		return URFTemporalComponents.createCalendar(getYear(), getMonth(), getDay(), getURFTime(), locale);
	}
	
	/**Returns the canonical lexical representation of this date time in the form "YYYY-MM-DDThh:mm:ss[.s+]+/-hh:mm".
	@return The canonical lexical representation of this date time.
	*/
	public String toString()
	{
		return append(new StringBuilder()).toString();	//append the lexical representation to a new string builder and return the resulting string
	}

}
