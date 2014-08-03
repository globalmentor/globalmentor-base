/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

}