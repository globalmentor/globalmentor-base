package com.garretwilson.text.elff;

/**An enumeration encapsulating the WebTrends true/false values.
The string representation of this enumeration is that expected by WebTrends log files.
@author Garret Wilson
*/
public enum WebTrendsYesNo
{

	/**The value for "Yes" used by WebTrends for true values.*/
	YES("Yes"),
	
	/**The value for "No" used by WebTrends for false values.*/
	NO("No");

	/**The string representation of this value.*/
	private final String string;

	/**String representation constructor.
	@param string The string representation of this instance.
	*/
	private WebTrendsYesNo(final String string)
	{
		this.string=string;
	}

	/**@return The string representation of this value.*/
	public String toString()
	{
		return string;
	}

	/**Converts the given value to a yes/no enumeration instance.
	@param value The boolen value to convert.
	@return {@link #YES} for <code>true</code> or {@link #NO} for <code>false</code>.
	*/
	public static WebTrendsYesNo asYesNo(final boolean value)
	{
		return value ? YES : NO;	//return yes or no based upon the value
	}
}
