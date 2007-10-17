package com.garretwilson.urf;

import java.util.Date;

import com.garretwilson.lang.IntegerUtilities;
import static com.garretwilson.lang.StringBuilderUtilities.*;
import com.garretwilson.text.*;

import static com.garretwilson.urf.URF.*;

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

	/**Full Constructor.
	@param year The year, 0-9999.
	@param month The month, 1-12.
	@param day The day, 1-31.
	@param hours The hours, 0-23.
	@param minutes The minutes, 0-59.
	@param seconds The seconds, 0-59.
	@param microseconds The microseconds, 0-999999
	@param utcOffset The UTC offset, or <code>null</code> if no UTC offset is known.
	*/
	public URFDateTime(final int year, final int month, final int day, final int hours, final int minutes, final int seconds, final int microseconds, final URFUTCOffset utcOffset)
	{
		super(year, month, day, hours, minutes, seconds, microseconds, utcOffset);
	}

	/**Temporal component constructor.
	@param temporalcomponents The temporal components from which to construct the class.
	@exception NullPointerException if the given temporal components is <code>null</code>.
	*/
	private URFDateTime(final URFTemporalComponents temporalComponents)
	{
		super(temporalComponents.getYear(), temporalComponents.getMonth(), temporalComponents.getDay(), temporalComponents.getHours(), temporalComponents.getMinutes(), temporalComponents.getSeconds(), temporalComponents.getMicroseconds(), temporalComponents.asUTCOffset());
	}

	/**Date constructor.
	@param date The date representing the difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	@exception NullPointerException if the given date is <code>null</code>.
	*/
	public URFDateTime(final Date date)
	{
		this(new URFTemporalComponents(date));	//construct the class from temporal components
	}

	/**Millisecond time constructor.
	@param time The difference, measured in milliseconds, between the current time and midnight, January 1, 1970 UTC.
	*/
	public URFDateTime(final long time)
	{
		this(new URFTemporalComponents(time));	//construct the class from temporal components
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
			return new URFDateTime(URFTemporalComponents.parseDateTime(string, true, true));	//parse temporal components for both the date and the time and use that to create a new date time object			
		}
		catch(final SyntaxException syntaxException)	//if the syntax of the string was not correct
		{
			throw new ArgumentSyntaxException(syntaxException);
		}
	}

	/**Returns the canonical lexical representation of this date time.
	@return The canonical lexical representation of this UTC offset in the form "YYYY-MM-DDThh:mm:ss[.s+]+/-hh:mm".
	*/
	public final String toString()
	{
		final StringBuilder stringBuilder=new StringBuilder();	//create a new string builder
		stringBuilder.append(IntegerUtilities.toString(getYear(), 10, 4));	//append the year, using four digits
		stringBuilder.append(DATE_DELIMITER);	//append the date delimiter
		stringBuilder.append(IntegerUtilities.toString(getMonth(), 10, 2));	//append the month, using two digits
		stringBuilder.append(DATE_DELIMITER);	//append the date delimiter
		stringBuilder.append(IntegerUtilities.toString(getDay(), 10, 2));	//append the day, using two digits
		stringBuilder.append(TIME_BEGIN);	//indicate that the time is beginning
		stringBuilder.append(IntegerUtilities.toString(getHours(), 10, 2));	//append the hours, using two digits
		stringBuilder.append(TIME_DELIMITER);	//append ':'
		stringBuilder.append(IntegerUtilities.toString(getMinutes(), 10, 2));	//append the minutes, using two digits
		stringBuilder.append(TIME_DELIMITER);	//append ':'
		stringBuilder.append(IntegerUtilities.toString(getSeconds(), 10, 2));	//append the seconds, using two digits
		final int microseconds=getMicroseconds();	//get the microseconds
		if(microseconds>0)	//if microseconds are given
		{
			stringBuilder.append(DECIMAL_DELIMITER);	//indicate that subseconds are present
			assert microseconds<1000000 : "Unexpectedly more than a million microseconds.";	//TODO check this in the constructor
			stringBuilder.append(Integer.toString(microseconds));	//append the string form of the microseconds
			trimEnd(stringBuilder, '0');	//remove trailing zeros
		}
		final URFUTCOffset utcOffset=getUTCOffset();	//get the UTC offset, if any
		if(utcOffset!=null)	//if there is a UTC offset
		{
			utcOffset.append(stringBuilder);	//append the lexical form of the UTC offset to our string builder
		}
		return stringBuilder.toString();	//return the string we constructed
	}

}
