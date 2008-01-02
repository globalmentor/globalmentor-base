/* Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
 * All Rights Reserved.
 * 
 * Use is subject to the BSD-style license at
 * <https://svn.globalmentor.com/java/src/com/globalmentor/license.txt>.
 */

package com.globalmentor.java;

/**Utilities for manipulating float objects.
@author Garret Wilson
*/
public class Floats
{

	/**Checks to make sure that a given value is within the given range.
	@param value The value to check.
	@param rangeMin The minimum range value, inclusive.
	@param rangeMax The maximum range value, inclusive.
	@exception IllegalArgumentException if the value is less than the range minimum or greater than the range maximum.
	@return The given value.
	*/
	public static float checkRange(final float value, final float rangeMin, final float rangeMax)
	{
		if(value<rangeMin || value>rangeMax)	//if the value not within the range
		{
			throw new IllegalArgumentException("Value "+value+" is not within the range "+rangeMin+" to "+rangeMax);
		}
		return value;	//return the value, which has been determined to be within the range
  }

}