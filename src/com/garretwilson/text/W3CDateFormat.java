package com.garretwilson.text;

import java.text.*;

/**Class for formatting dates and times according to the W3C Note, 
	"Date and Time Formats",
	<a href="http://www.w3.org/TR/NOTE-datetime">http://www.w3.org/TR/NOTE-datetime</a>,
	a profile of ISO 8601.
<p>Warning: The W3C patterns currently format the time zone according to RFC
	822 (+/-HHmm) rather than as specified by W3C NOTE-datetime (+/-HH:mm).
	The decimal portions of seconds may also not be correct.</p>
@author Garret Wilson
*/
public class W3CDateFormat extends SimpleDateFormat
{

	/**Style for year: YYYY (eg 1997)*/
	public final static int YEAR_STYLE=-1;
	/**Pattern for year and month: YYYY-MM (eg 1997-07)*/
	public final static int YEAR_MONTH_STYLE=-2;
	/**Pattern for complete date: YYYY-MM-DD (eg 1997-07-16)*/
	public final static int DATE_STYLE=-3;
	/**Pattern for complete date plus hours and minutes (without timezone):
		YYYY-MM-DDThh:mmTZD (eg 1997-07-16T19:20+01:00)
	*/
	public final static int DATE_HOURS_MINUTES_STYLE=-4;
	/**Pattern for complete date plus hours, minutes and seconds (without timezone):
		YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30+01:00)
	*/
	public final static int DATE_HOURS_MINUTES_SECONDS_STYLE=-5;
	/**Pattern for complete date plus hours, minutes, seconds and a decimal fraction of a second (without timezone):
		YYYY-MM-DDThh:mm:ss.sTZD (eg 1997-07-16T19:20:30.45+01:00)
	*/
	public final static int DATE_TIME_STYLE=-6;

	/**Pattern for year: YYYY (eg 1997)*/
	private final static String YEAR_PATTERN="yyyy";
	/**Pattern for year and month: YYYY-MM (eg 1997-07)*/
	private final static String YEAR_MONTH_PATTERN=YEAR_PATTERN+"-MM";
	/**Pattern for complete date: YYYY-MM-DD (eg 1997-07-16)*/
	private final static String DATE_PATTERN=YEAR_MONTH_PATTERN+"-dd";
	/**Pattern for complete date plus hours and minutes (without timezone):
		YYYY-MM-DDThh:mmTZD (eg 1997-07-16T19:20+01:00)
	*/
	private final static String DATE_HOURS_MINUTES_PATTERN=DATE_PATTERN+"'T'HH:mm";
	/**Pattern for complete date plus hours, minutes and seconds (without timezone):
		YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30+01:00)
	*/
	private final static String DATE_HOURS_MINUTES_SECONDS_PATTERN=DATE_HOURS_MINUTES_PATTERN+":ss";
	/**Pattern for complete date plus hours, minutes, seconds and a decimal fraction of a second (without timezone):
		YYYY-MM-DDThh:mm:ss.sTZD (eg 1997-07-16T19:20:30.45+01:00)
	*/
	private final static String DATE_TIME_PATTERN=DATE_HOURS_MINUTES_SECONDS_PATTERN+"S";

	/**The style to use for formatting.*/
	private final int style;

		/**@return The style to use for formatting.*/
		protected int getStyle() {return style;}

	/**Constructs a W3C date and time formatter using the given style.
	@param style One of the styles defined by this class
	*/
	public W3CDateFormat(final int style)
	{
		super(getPattern(style));	//construct the parent class with the appropriate pattern
		this.style=style;	//save the style
	}

	/**The formatting patterns used, in order.*/
	private final static String[] patterns={YEAR_PATTERN, YEAR_MONTH_PATTERN, DATE_PATTERN, DATE_HOURS_MINUTES_PATTERN, DATE_HOURS_MINUTES_SECONDS_PATTERN, DATE_TIME_PATTERN};

	/**Determines a pattern to use for the given style.
	This pattern may be incomplete and the output may require more processing.
	@param style One of the styles defined by this class
	@return A pattern to use as the basis for formatting.
	*/	 
	protected static String getPattern(final int style)
	{
		final int patternIndex=-style-1;	//switch from negative to positive and make the style zero-based to get the pattern index
		final String pattern=patterns[patternIndex];	//get this pattern
		return style<=DATE_HOURS_MINUTES_STYLE ? pattern+"Z" : pattern;	//add a time zone (although in incorrect format) to all time styles  
	}
}
