/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.lang.Math;
import java.util.Collection;

/**
 * Utilities for working with math.
 * @author Garret Wilson
 */
public class Maths
{

	/**
	 * Finds the floor of a value with a particular precision of a specified digit. A position of zero will result in the input value. Otherwise, the value will
	 * effectively be divided by 10 to the power of <var>position</var> before rounding, then multiplied by the same value after rounding.
	 * <p>
	 * For example, if a long value represents milliseconds, calling <code>floor(value, -3)</code> will result in rounding down to the nearest millisecond.
	 * </p>
	 * @param value The value to round.
	 * @param position The position relative to the decimal to which the value should be rounded, with negative numbers indicating fractional positions and
	 *          positive numbers representing integer positions.
	 * @return The value rounded down at the given decimal position.
	 */
	public static int floor(final int value, final int position)
	{
		final double divisor = Math.pow(10, position); //raise 10 to the power of the negative position to get the divisor
		return (int)(((int)(value / divisor)) * divisor); //divide by the divisor, floor, then multiply by the divisor
	}

	/**
	 * Finds the floor of a value with a particular precision of a specified digit. A position of zero will result in the input value. Otherwise, the value will
	 * effectively be divided by 10 to the power of <var>position</var> before rounding, then multiplied by the same value after rounding.
	 * <p>
	 * For example, if a long value represents milliseconds, calling <code>floor(value, -3)</code> will result in rounding down to the nearest millisecond.
	 * </p>
	 * @param value The value to round.
	 * @param position The position relative to the decimal to which the value should be rounded, with negative numbers indicating fractional positions and
	 *          positive numbers representing integer positions.
	 * @return The value rounded down at the given decimal position.
	 */
	public static long floor(final long value, final int position)
	{
		final double divisor = Math.pow(10, position); //raise 10 to the power of the negative position to get the divisor
		return (long)(((long)(value / divisor)) * divisor); //divide by the divisor, floor, then multiply by the divisor
	}

	/**
	 * Rounds a value with a particular precision of a specified digit. A position of zero will round to a normal integer. Otherwise, the value will be
	 * effectively divided by 10 to the power of <var>position</var> before rounding, then multiplied by the same value after rounding.
	 * @param a A float value.
	 * @param position The position relative to the decimal to which the value should be rounded, with negative numbers indicating fractional positions and
	 *          positive numbers representing integer positions.
	 * @return The value rounded at the given position.
	 */
	public static float round(float a, final int position)
	{
		final double divisor = Math.pow(10, position); //raise 10 to the power of the negative position to get the divisor
		return (float)(Math.round(a / divisor) * divisor); //divide by the divisor, round, then multiply by the divisor
	}

	/**
	 * Returns the greater of two <code>short</code> values
	 * @param a An argument.
	 * @param b Another argument.
	 * @return The larger of <var>a</var> and <var>b</var>.
	 * @see Math#max(int, int)
	 */
	public static short max(final short a, final short b)
	{
		return a >= b ? a : b; //return the greater of a and b
	}

	/**
	 * Returns the greatest of three <code>double</code> values
	 * @param a An argument.
	 * @param b Another argument.
	 * @param c Yet another argument.
	 * @return The greatest of <var>a</var>, <var>b</var>, and <var>c</var>.
	 * @see Math#max(double, double)
	 */
	public static double max(final double a, final double b, final double c)
	{
		return Math.max(Math.max(a, b), c); //return the greates of a, b, and c
	}

	/**
	 * Returns the least of three <code>double</code> values
	 * @param a An argument.
	 * @param b Another argument.
	 * @param c Yet another argument.
	 * @return The least of <var>a</var>, <var>b</var>, and <var>c</var>.
	 * @see Math#min(double, double)
	 */
	public static double min(final double a, final double b, final double c)
	{
		return Math.min(Math.min(a, b), c); //return the least of a, b, and c
	}

	/**
	 * Determines the lowest integer from a collection of integers.
	 * @param integers The collection of integers.
	 * @return The minimum of the given integers, or the {@link Integer#MAX_VALUE} if there are no integers.
	 * @exception NullPointerException if the given collection is <code>null</code> or one of its elements is <code>null</code>.
	 */
	public static int min(final Collection<? extends Integer> integers)
	{
		return min(integers, Integer.MAX_VALUE); //return the minimum of the integers with the largest integer as the max
	}

	/**
	 * Determines the lowest integer from a collection of integers with a given maximum.
	 * @param integers The collection of integers.
	 * @param max The maximum value this method can return.
	 * @return The minimum of the given integers, or the given maximum if there are no integers.
	 * @exception NullPointerException if the given collection is <code>null</code> or one of its elements is <code>null</code>.
	 */
	public static int min(final Collection<? extends Integer> integers, int max)
	{
		for(final Integer integer : integers) //for each integer
		{
			final int intValue = integer.intValue(); //get the int value
			if(intValue < max) //if this is a lower value
			{
				max = intValue; //lower the max value
			}
		}
		return max; //return the new updated max value
	}
}