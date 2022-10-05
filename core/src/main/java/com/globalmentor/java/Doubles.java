/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.java;

import static java.lang.Double.*;
import static java.lang.Math.*;

/**
 * Utilities for manipulating double objects and values.
 * @author Garret Wilson
 */
public class Doubles {

	/**
	 * Checks to make sure that a given value is within the given range.
	 * @param value The value to check.
	 * @param rangeMin The minimum range value, inclusive.
	 * @param rangeMax The maximum range value, inclusive.
	 * @throws IllegalArgumentException if the value is less than the range minimum or greater than the range maximum.
	 * @return The given value.
	 */
	public static double checkRange(final double value, final double rangeMin, final double rangeMax) {
		if(value < rangeMin || value > rangeMax) { //if the value not within the range
			throw new IllegalArgumentException("Value " + value + " is not within the range " + rangeMin + " to " + rangeMax);
		}
		return value; //return the value, which has been determined to be within the range
	}

	/**
	 * Compares to <code>double</code> values to see if there are effectively equals. Double values that are equals as per <code>==</code> are always leniently
	 * equal; <code>NaN</code> are leniently equal; and zero is considered equal without regard to whether it is positive or negative.
	 * @apiNote This is a convenience method that provides a default tolerance that most of the time is appropriate for comparing <code>double</code> values to
	 *          see if they are "close enough".
	 * @implSpec This implementation delegates to {@link #equalsLenient(double, double, double)} using a tolerance derived of 1 ulp.
	 * @param a The first value to compare.
	 * @param b The second value to compare.
	 * @return <code>true</code> if the values are effectively equal, with lenient comparison to allow for floating point rounding.
	 * @see #equalsLenient(double, double, double)
	 * @see Math#ulp(double)
	 * @see <a href="https://en.wikipedia.org/wiki/Unit_in_the_last_place">Unit in the last place</a>
	 * @see <a href="https://stackoverflow.com/q/9090500">How to compare that sequence of doubles are all “approximately equal” in Java?</a>
	 * @see <a href="https://matlabgeeks.com/tips-tutorials/floating-point-comparisons-in-matlab/">Floating Point Comparisons in Matlab</a>
	 */
	public static boolean equalsLenient(final double a, final double b) {
		return equalsLenient(a, b, ulp(a));
	}

	/**
	 * Compares to <code>double</code> values to see if there are effectively equals, with a difference within some tolerance. Double values that are equals as
	 * per <code>==</code> are always leniently equal; <code>NaN</code> are leniently equal; and zero is considered equal without regard to whether it is positive
	 * or negative. If the tolerance is zero, then the result is equivalent to <code>==</code> except that <code>NaN</code>s are considered equal.
	 * @apiNote This method is equivalent to <code>Math.abs(a - b) &lt;= tolerance || Double.valueOf(a).equals(Double.valueOf(b))</code>.
	 * @param a The first value to compare.
	 * @param b The second value to compare.
	 * @param tolerance The maximum allowed absolute difference between the values, or <code>0.0</code> (or <code>NaN</code>) if non-<code>NaN</code> comparison
	 *          should work equivalent to <code>==</code>.
	 * @return <code>true</code> if the values are effectively equal, with lenient comparison to allow for floating point rounding.
	 * @see <a href=
	 *      "https://guava.dev/releases/snapshot-jre/api/docs/com/google/common/math/DoubleMath.html#fuzzyEquals-double-double-double-">com.google.common.math.DoubleMath.fuzzyEquals(double
	 *      a, double b, double tolerance)</a>
	 */
	public static boolean equalsLenient(final double a, final double b, final double tolerance) {
		if(a == b) { //short circuit for truly equal values; handles 0.0/-0.0 equivalence
			return true;
		}
		if(isNaN(a)) {
			return isNaN(b); //if one is NaN, they both must be NaN
		} else if(isNaN(b)) {
			return false;
		}
		if(tolerance == 0.0) {
			return false;
		}
		return Math.abs(a - b) <= tolerance; //returns false if tolerance is NaN		
	}

}
