package com.garretwilson.lang;

import java.lang.Math;
import com.garretwilson.util.Debug;

/**Utilities for working with math.
@author Garret Wilson
*/
public class MathUtilities
{

	/**Rounds a value with a particular precision of a specified digit. A position
		of zero will round to a normal integer. Otherwise, the value will be
		multiplied by 10 to the power of <code>position</code> before rounding,
		then divided by the same value after ronding.
	@param a A float value.
	@param position The position relative to the decimal to which the value
		should be rounded, with negative numbers indicating fractional positions
		and positive numbers representing integer positions.
	*/
	public final static float round(float a, final int position)
	{
//G***del Debug.trace("rounding value: "+a+" at position: "+position);  //G***del
		final double multiplier=Math.pow(10, -position); //raise 10 to the power of the negative position to get the multiplier
/*G***del
Debug.trace("multiplier: "+multiplier);  //G***del
Debug.trace("value times multiplier: "+a*multiplier);  //G***del
Debug.trace("value times multiplier rounded: "+Math.round(a*multiplier));  //G***del
Debug.trace("final value: "+(float)(Math.round(a*multiplier)/multiplier));  //G***del
*/
		return (float)(Math.round(a*multiplier)/multiplier); //multiply by the multiplier, round, then divide by the multiplier
	}

	/**Returns the greater of two <code>short</code> values
	@param a An argument.
	@param b Another argument.
	@return The larger of <var>a</var> and <var>b</var>.
	@see Math#max(int, int)
	*/
	public static short max(final short a, final short b)
	{
		return a>=b ? a : b;	//return the greater of a and b
	}

	/**Returns the greatest of three <code>double</code> values
	@param a An argument.
	@param b Another argument.
	@param c Yet another argument.
	@return The greatest of <var>a</var>, <var>b</var>, and <var>c</var>.
	@see Math#max(double, double)
	*/
	public static double max(final double a, final double b, final double c)
	{
		return Math.max(Math.max(a, b), c);	//return the greates of a, b, and c
	}

	/**Returns the least of three <code>double</code> values
	@param a An argument.
	@param b Another argument.
	@param c Yet another argument.
	@return The least of <var>a</var>, <var>b</var>, and <var>c</var>.
	@see Math#min(double, double)
	*/
	public static double min(final double a, final double b, final double c)
	{
		return Math.min(Math.min(a, b), c);	//return the least of a, b, and c
	}

}