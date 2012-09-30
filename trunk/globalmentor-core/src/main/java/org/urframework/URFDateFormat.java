/*
 * Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.text.*;
import java.util.Date;
import java.util.Locale;

import static com.globalmentor.iso.ISO8601.*;
import static com.globalmentor.java.Objects.*;

/**Date format class for formatting dates, times, date times, and UTC offsets according to their URF lexical forms.
@author Garret Wilson
*/
public class URFDateFormat extends SimpleDateFormat
{
	/**The specific style of URF date format.*/
	public enum Style
	{
		/**Pattern for urf.Date (e.g. 1997-07-16)*/
		DATE("yyyy-MM-dd"),
		/**Pattern for urf.Time (e.g. 19:20.45+01:00)*/
		TIME("HH:mm:ss.SSSZ"),
		/**Pattern for urf.DateTime (e.g. 1997-07-16T19:20:30.45+01:00)*/
		DATE_TIME("yyyy-MM-ddTHH:mm:ss.SSSZ"),
		/**Pattern for urf.UTCOffset (e.g. +01:00)*/
		UTC_OFFSET("yyyy-MM-ddTHH:mm:ss.SSSZ");

		/**The {@link SimpleDateFormat} pattern for this style.*/
		private final String pattern;

			/**@return The {@link SimpleDateFormat} pattern for this style.*/
			public String getPattern() {return pattern;}

		/**Pattern constructor.
		@param pattern The {@link SimpleDateFormat} pattern for this style.
		@exception NullPointerException if the given pattern is <code>null</code>.
		*/
		private Style(final String pattern)
		{
			this.pattern=checkInstance(pattern, "Pattern cannot be null.");
		}
	};

	/**The style to use for formatting.*/
	private final Style style;

		/**@return The style to use for formatting.*/
		protected Style getStyle() {return style;}

	/**Constructs a W3C date and time formatter using the given style.
	@param style One of the formatting styles defined by this class
	@exception NullPointerException if the given style is <code>null</code>.
	*/
	public URFDateFormat(final Style style)
	{
		super(style.getPattern(), Locale.ENGLISH);	//the URF temporal formats always uses English
		this.style=style;	//save the style
	}

	/**Formats the given date into a date/time string and appends the result to the given string builder.
	This version adds the {@value URF#TIME_DELIMITER} character if a time zone was included.
	@param date The date-time value to be formatted into a date-time string.
	@param toAppendTo Where the new date-time text is to be appended.
	@param pos The formatting position. On input: an alignment field, if desired. On output: the offsets of the alignment field.
	@return The formatted date-time string.
	@exception NullPointerException if the given date is <code>null</code>.
	*/
	public StringBuffer format(final Date date, final StringBuffer toAppendTo, final FieldPosition fieldPosition)
	{
		final StringBuffer stringBuffer=super.format(date, toAppendTo, fieldPosition);	//do the default formatting
		final Style style=getStyle();	//get the style used
		if(style==Style.TIME || style==Style.DATE_TIME || style==Style.UTC_OFFSET)	//if we need to fix up the time zone
		{
			stringBuffer.insert(stringBuffer.length()-2, TIME_DELIMITER);	//insert the time zone time delimiter, ':', which the SimpleDateFormat leaves out
		}
		return stringBuffer;	//return the string buffer
	}

	/**Parses text from a string to produce a date.
	This version first removes the {@value URF#TIME_DELIMITER} character if a time zone is included.
	@param text A string, part of which should be parsed.
	@param position A parse position object with index and error index information.
	@return A date parsed from the string, or <code>null</code> if an error occurred.
	@exception NullPointerException if the text and/or position is <code>null</code>.
	*/
	public Date parse(String source, final ParsePosition position)
	{
		if(style==Style.TIME || style==Style.DATE_TIME || style==Style.UTC_OFFSET)	//if we need to fix up the time zone
		{
			if(source.length()>=3 && source.charAt(source.length()-3)==TIME_DELIMITER)	//if a time zone was included as expected
			{
				source=new StringBuilder(source).deleteCharAt(source.length()-3).toString();	//remove the time zone delimiter ':', because the SimpleDateFormat doesn't expect it
			}
		}
		return super.parse(source, position);	//parse the date normally
	}

	/**Formats a date by creating a formatter using the given style.
	@param style One of the URF date format styles.
	*/
	public static String format(final Date date, final Style style)
	{
		return new URFDateFormat(style).format(date);	//create a new format class and format the date
	}
}
