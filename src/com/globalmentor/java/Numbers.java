/*
 * Copyright © 2003-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.java;

import java.math.BigDecimal;

/**Utilities for manipulating numbers.
@author Garret Wilson
*/
public class Numbers
{

	/**The double value representing 1/3.*/
	public final static double ONE_THIRD_DOUBLE=1.0d/3.0d;
	/**The double value representing 2/3.*/
	public final static double TWO_THIRDS_DOUBLE=2.0d/3.0d;

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


	/**Compares the two specified numbers.
	@param number1 The first number to compare.
	@param number2 The second number to compare.
	@return The value <code>0</code> if the first number is numerically equal to the second number;
		a value less than <code>0</code> if the first number is numerically less than the second number;
		or a value greater than <code>0</code> if the first number is numerically greater than the second number.
	@exception NullPointerException if either of the given numbers is <code>null</code>.
	*/
	public static int compare(final Number number1, final Number number2)
	{
		if(number1.equals(number2))	//if they numbers are equal
		{
			return 0;	//return zero
		}
		else if((number1 instanceof Integer || number1 instanceof Long) && (number2 instanceof Integer || number2 instanceof Long))	//if both are integer types
		{
			return Longs.compare(number1.longValue(), number2.longValue());	//compare long values
		}
		else	//for all other cases TODO add support for big types
		{
			return Double.compare(number1.doubleValue(), number2.doubleValue());	//compare double values
		}
	}

	/**Compares the two specified objects, one of which must be an instance of {@link Number}, and either of which may be <code>null</code>.
	Numbers are sorted before non-numbers, and two numbers are compared.
	@param object1 The first object to compare.
	@param object2 The second object to compare.
	@return The value <code>0</code> if both are numbers and the first number is numerically equal to the second number;
		a value less than <code>0</code> if only the first is a number, or both are numbers and the first number is numerically less than the second number;
		or a value greater than <code>0</code> if only the second is a number, or both are numbers and the first number is numerically greater than the second number.
	@exception IllegalArgumentException if neither of the given objects is an instance of {@link Number}.
	*/
	public static int sort(final Object object1, final Object object2)
	{
		if(object1 instanceof Number)	//if the first is a number
		{
			if(object2 instanceof Number)	//if they are both numbers
			{
				return compare((Number)object1, (Number)object2);	//compare the numbers
			}
			else	//if only the first is a number
			{
				return -1;	//sort numbers in front of non-numbers
			}
		}
		else if(object2 instanceof Number)	//if the first is not a number, but the second is
		{
			return 1;	//put non-numbers after numbers
		}
		else	//if neither are numbers
		{
			throw new IllegalArgumentException("Either "+object1+" or "+object2+" must be a number.");
		}
	}

}
