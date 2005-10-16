package com.garretwilson.lang;

import java.math.BigDecimal;

/**Utilities for manipulating numbers.
@author Garret Wilson
*/
public class NumberUtilities
{

	/**Determines if the given number is a non-integer decimal type: {@link Float}, {@link Double}, or {@link BigDecimal}.
	@param number The number to examine.
	@return <code>true</code> if the given number is a non-integer decimal type.
	*/
	public static boolean isDecimal(final Number number)
	{
		return number instanceof Float || number instanceof Double || number instanceof BigDecimal;
	}

	/**Returns a big decimal representation of the given number.
	@param number The number to return as a big decimal.
	@return The number as a big decimal.
	@see #isDecimal(Number)
	*/
	public static BigDecimal asBigDecimal(final Number number)
	{
		return number instanceof BigDecimal ? (BigDecimal)number : new BigDecimal(number.toString());	//create a big decimal only if needed
	}
}
