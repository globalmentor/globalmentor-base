package com.garretwilson.urf;

import java.util.TimeZone;

import com.garretwilson.lang.IntegerUtilities;
import static com.garretwilson.lang.IntegerUtilities.*;
import com.garretwilson.text.*;
import static com.garretwilson.urf.URF.*;
import static com.garretwilson.util.TimeZoneConstants.*;

/**The class representing an <code>urf.UTCOffset</code> type.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class URFUTCOffset implements URFTemporal
{

	/**The shared UTC offset representing Coordinated Universal Time (UTC), +00:00.*/
	public final static URFUTCOffset UTC_OFFSET_UTC=new URFUTCOffset(0, 0);

	/**The shared time zone representing Greenwich Mean Time (GMT).*/
	public final static TimeZone TimeZone_GMT=TimeZone.getTimeZone(GMT_ID);

	/**The offset hours.*/
	private final int hours;

		/**@return The offset hours.*/
		public int getHours() {return hours;}

	/**The offset minutes.*/
	private final int minutes;

		/**@return The offset minutes.*/
		public int getMinutes() {return minutes;}

	/**Full Constructor.
	@param hours The offset hours.
	@param minutes The offset minutes.
	@exception IllegalArgumentException if the given offset minutes is negative.
	*/
	public URFUTCOffset(final int hours, final int minutes)
	{
		this.hours=hours;	//save the hours
		this.minutes=checkMinimum(minutes, 0);	//save the minutes, but don't allow negative offset minutes
	}

	/**Returns an URF UTC offset object holding the value of the specified string.
	@param string The string to be parsed as a UTC offset.
	@return An URF UTC offset object represented by the string.
	@exception NullPointerException if the given string is <code>null</code>
	@exception ArgumentSyntaxException if the given string does not have the correct syntax.
	*/
	public static URFUTCOffset valueOf(final String string) throws ArgumentSyntaxException
	{
		try
		{
			final URFTemporalComponents temporalComponents=URFTemporalComponents.parseDateTime(string, false, false);	//parse temporal components for only the UTC offset
			return temporalComponents.asUTCOffset();	//return the UTC offset from the temporal components
		}
		catch(final SyntaxException syntaxException)	//if the syntax of the string was not correct
		{
			throw new ArgumentSyntaxException(syntaxException);
		}
	}

	/**Returns the UTC offset as a time zone.
	@return A time zone object representing the UTC offset.
	*/
	public TimeZone toTimeZone()
	{
		final StringBuilder timeZoneIDStringBuilder=new StringBuilder();	//create a new string builder
		timeZoneIDStringBuilder.append(GMT_ID);	//start with "GMT"
		if(hours!=0 || minutes>0)	//if this is not exactly GMT
		{
			if(hours>=0)	//if there is a nonnegative hour offset
			{
				timeZoneIDStringBuilder.append('+');	//show that  this is a positive offset
			}
			timeZoneIDStringBuilder.append(Integer.toString(hours));	//append the offset hours
			timeZoneIDStringBuilder.append(TIME_DELIMITER);	//append ':'
			timeZoneIDStringBuilder.append(IntegerUtilities.toString(minutes, 10, 2));	//append the offset minutes, using two digits
		}
		return TimeZone.getTimeZone(timeZoneIDStringBuilder.toString());	//look up the time zone and return it
	}

	/**Appends the canonical lexical representation of this UTC offset to a string builder in the form "+/-hh:mm".
	@param stringBuild The string builder to which the lexical representation will be appended.
	@return The string builder.
	*/
	public final StringBuilder append(final StringBuilder stringBuilder)
	{
		if(hours>=0)	//if the hours are not negative
		{
			stringBuilder.append('+');	//show that this is a positive offset
		}
		stringBuilder.append(IntegerUtilities.toString(hours, 10, 2));	//append the offset hours, using two digits
		stringBuilder.append(TIME_DELIMITER);	//append ':'
		stringBuilder.append(IntegerUtilities.toString(minutes, 10, 2));	//append the offset minutes, using two digits
		return stringBuilder;	//return the string builder
	}

	/**Returns the canonical lexical representation of this UTC offset.
	@return The canonical lexical representation of this UTC offset in the form "+/-hh:mm".
	*/
	public final String toString()
	{
		return append(new StringBuilder()).toString();	//append the lexical representation to a new string builder and return the resulting string
	}

}
