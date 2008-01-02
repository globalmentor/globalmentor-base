package com.garretwilson.text;

import java.text.*;
import java.util.Date;
import java.util.Locale;

import static com.globalmentor.java.CharSequenceUtilities.*;

/**Class for formatting dates and times according to the W3C Note, 
	"Date and Time Formats",
	<a href="http://www.w3.org/TR/NOTE-datetime">http://www.w3.org/TR/NOTE-datetime</a>,
	a profile of ISO 8601.
<p>Warning: The W3C patterns currently format the time zone according to RFC
	822 (+/-HHmm) rather than as specified by W3C NOTE-datetime (+/-HH:mm).
	The decimal portions of seconds may also not be correct.</p>
@author Garret Wilson
@see <a href="http://www.w3.org/TR/NOTE-datetime">Date and Time Formats</a>
*/
public class W3CDateFormat extends SimpleDateFormat
{
	/**The specific style of W3C date format.*/
	public enum Style
	{
		/**Style for year: YYYY (eg 1997)*/
		YEAR,
		/**Pattern for year and month: YYYY-MM (eg 1997-07)*/
		YEAR_MONTH,
		/**Pattern for complete date: YYYY-MM-DD (eg 1997-07-16)*/
		DATE,
		/**Pattern for complete date plus hours and minutes:
			YYYY-MM-DDThh:mmTZD (eg 1997-07-16T19:20+01:00)
		*/
		DATE_HOURS_MINUTES,
		/**Pattern for complete date plus hours, minutes and seconds:
			YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30+01:00)
		*/
		DATE_HOURS_MINUTES_SECONDS,
		/**Pattern for complete date plus hours, minutes, seconds and a decimal fraction of a second:
			YYYY-MM-DDThh:mm:ss.sTZD (eg 1997-07-16T19:20:30.45+01:00)
		*/
		DATE_TIME
	};

	/**Pattern for year: YYYY (eg 1997)*/
	private final static String YEAR_PATTERN="yyyy";
	/**Pattern for year and month: YYYY-MM (eg 1997-07)*/
	private final static String YEAR_MONTH_PATTERN=YEAR_PATTERN+"-MM";
	/**Pattern for complete date: YYYY-MM-DD (eg 1997-07-16)*/
	private final static String DATE_PATTERN=YEAR_MONTH_PATTERN+"-dd";
	/**Pattern for complete date plus hours and minutes:
		YYYY-MM-DDThh:mmTZD (eg 1997-07-16T19:20+01:00)
	*/
	private final static String DATE_HOURS_MINUTES_PATTERN=DATE_PATTERN+"'T'HH:mmZ";
	/**Pattern for complete date plus hours, minutes and seconds:
		YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30+01:00)
	*/
	private final static String DATE_HOURS_MINUTES_SECONDS_PATTERN=DATE_PATTERN+"'T'HH:mm:ssZ";
	/**Pattern for complete date plus hours, minutes, seconds and a decimal fraction of a second:
		YYYY-MM-DDThh:mm:ss.sTZD (eg 1997-07-16T19:20:30.45+01:00)
	*/
//G***testing 	private final static String DATE_TIME_PATTERN=DATE_HOURS_MINUTES_SECONDS_PATTERN;
	private final static String DATE_TIME_PATTERN=DATE_PATTERN+"'T'HH:mm:ss.SSSZ";

	/**The style to use for formatting.*/
	private final Style style;

		/**@return The style to use for formatting.*/
		protected Style getStyle() {return style;}

	/**Default constructor specifying a full {@link Style#DATE_TIME} format.*/
	public W3CDateFormat()
	{
		this(Style.DATE_TIME);	//construct a full date and time date format
	}

	/**Constructs a W3C date and time formatter using the given style.
	@param style One of the styles defined by this class
	*/
	public W3CDateFormat(final Style style)
	{
		super(getPattern(style), Locale.ENGLISH);	//the W3C date format always uses English
		this.style=style;	//save the style
//TODO del; only for testing		setTimeZone(TimeZone.getTimeZone(GMT_ID));	//G***testing; del
	}

	/**The formatting patterns used, in order.*/
	private final static String[] patterns={YEAR_PATTERN, YEAR_MONTH_PATTERN, DATE_PATTERN, DATE_HOURS_MINUTES_PATTERN, DATE_HOURS_MINUTES_SECONDS_PATTERN, DATE_TIME_PATTERN};

	/**Determines a pattern to use for the given style.
	This pattern may be incomplete and the output may require more processing.
	@param style One of the styles defined by this class
	@return A pattern to use as the basis for formatting.
	*/	 
	protected static String getPattern(final Style style)
	{
		final int styleOrdinal=style.ordinal();	//get the zero-based pattern index
		final String pattern=patterns[styleOrdinal];	//get this pattern
//G***del when works		return styleOrdinal>=Style.DATE_HOURS_MINUTES.ordinal() ? pattern+"Z" : pattern;	//add a time zone (although in incorrect format) to all time styles  
		return pattern;	//TODO tidy  
	}

  /**
   * Formats a Date into a date/time string.
   * @param date a Date to be formatted into a date/time string.
   * @param toAppendTo the string buffer for the returning date/time string.
   * @param fieldPosition keeps track of the position of the field
   * within the returned string.
   * On input: an alignment field,
   * if desired. On output: the offsets of the alignment field. For
   * example, given a time text "1996.07.10 AD at 15:08:56 PDT",
   * if the given fieldPosition is DateFormat.YEAR_FIELD, the
   * begin index and end index of fieldPosition will be set to
   * 0 and 4, respectively.
   * Notice that if the same time field appears
   * more than once in a pattern, the fieldPosition will be set for the first
   * occurrence of that time field. For instance, formatting a Date to
   * the time string "1 PM PDT (Pacific Daylight Time)" using the pattern
   * "h a z (zzzz)" and the alignment field DateFormat.TIMEZONE_FIELD,
   * the begin index and end index of fieldPosition will be set to
   * 5 and 8, respectively, for the first occurrence of the timezone
   * pattern character 'z'.
   * @return the formatted date/time string.
   */
	public StringBuffer format(Date date, StringBuffer toAppendTo, FieldPosition fieldPosition)	//TODO comment; use contants
	{
		final StringBuffer stringBuffer=super.format(date, toAppendTo, fieldPosition);	//do the default formatting
		if(stringBuffer.length()>=5)
		{
			final char timezoneDelimiter=stringBuffer.charAt(stringBuffer.length()-5);
			if(timezoneDelimiter=='+' || timezoneDelimiter=='-')
			{
				if(endsWith(stringBuffer, "0000"))
				{
					stringBuffer.replace(stringBuffer.length()-5, stringBuffer.length(), "Z");
				}
				else
				{
					stringBuffer.insert(stringBuffer.length()-2, ":");	//RFC 822 fixup TODO add comments
				}
			}
		}
		return stringBuffer;
	}

  /**
   * Parse a date/time string according to the given parse position.  For
   * example, a time text "07/10/96 4:5 PM, PDT" will be parsed into a Date
   * that is equivalent to Date(837039928046).
   *
   * <p> By default, parsing is lenient: If the input is not in the form used
   * by this object's format method but can still be parsed as a date, then
   * the parse succeeds.  Clients may insist on strict adherence to the
   * format by calling setLenient(false).
   *
   * @see java.text.DateFormat#setLenient(boolean)
   *
   * @param source  The date/time string to be parsed
   *
   * @param pos   On input, the position at which to start parsing; on
   *              output, the position at which parsing terminated, or the
   *              start position if the parse failed.
   *
   * @return      A Date, or null if the input could not be parsed
   */
	public Date parse(String source, ParsePosition pos)	//TODO comment; use contants
	{
		String revised=source;
		if(source.length()>0)
		{
			if(source.charAt(source.length()-1)=='Z')
			{
				revised=new StringBuilder(source).replace(source.length()-1, source.length(), "+0000").toString();
			}
			else if(source.length()>=3)	//TODO do more checks
			{
				if(source.charAt(source.length()-3)==':')	//RFC 822 fixup TODO add comments
				{
					revised=new StringBuilder(source).deleteCharAt(source.length()-3).toString();
				}
			}
		}
		return super.parse(revised, pos);
	}

	/**Formats a date by creating a W3C date and time formatter using the given style.
	@param style One of the W3C date/time styles.
	*/
	public static String format(final Date date, final Style style)
	{
		return new W3CDateFormat(style).format(date);	//create a new format class and format the date
	}
}
