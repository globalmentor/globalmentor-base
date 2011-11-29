/*
 * Copyright © 2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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
 * Various checks on objects given as arguments in a method.
 * 
 * <p>
 * The name of this class was inspired by the <a href="http://code.google.com/p/guava-libraries/">Google Guava Library</a>.
 * </p>
 * 
 * @author Garret Wilson
 */
public class Preconditions
{

	/**
	 * Checks the results of an expression to see if an argument is correct, and throws an {@link IllegalArgumentException} if the value is <code>false</code>.
	 * @param test The result of the test.
	 * @throws IllegalArgumentException if the given value is <code>false</code>
	 */
	public static void checkArgument(final boolean test)
	{
		checkArgument(test, null); //check the test with no description
	}

	/**
	 * Checks the results of an expression to see if an argument is correct, and throws an {@link IllegalArgumentException} if the value is <code>false</code>.
	 * @param test The result of the test.
	 * @param description A description of the test to be used when generating an exception, or <code>null</code> for no description.
	 * @throws IllegalArgumentException if the given value is <code>false</code>
	 */
	public static void checkArgument(final boolean test, final String description)
	{
		if(!test)
		{
			throw new IllegalArgumentException(description);
		}
	}

	/**
	 * Checks to make sure that a given value is not smaller than the given minimum.
	 * @param value The value to check.
	 * @param rangeMin The minimum range value, inclusive.
	 * @throws IllegalArgumentException if the value is less than the range minimum.
	 * @return The given value.
	 */
	public static int checkArgumentMinimum(final int value, final int rangeMin)
	{
		if(value < rangeMin) //if the value not within the range
		{
			throw new IllegalArgumentException("Value " + value + " cannot be less than " + rangeMin);
		}
		return value; //return the value, which has been determined to be within the range
	}

	/**
	 * Checks to make sure that a given value is not smaller than the given minimum.
	 * @param value The value to check.
	 * @param rangeMin The minimum range value, inclusive.
	 * @throws IllegalArgumentException if the value is less than the range minimum.
	 * @return The given value.
	 */
	public static long checkArgumentMinimum(final long value, final long rangeMin)
	{
		if(value < rangeMin) //if the value not within the range
		{
			throw new IllegalArgumentException("Value " + value + " cannot be less than " + rangeMin);
		}
		return value; //return the value, which has been determined to be within the range
	}

	/**
	 * Checks to make sure that a given argument value is within the given range.
	 * @param value The value to check.
	 * @param rangeMin The minimum range value, inclusive.
	 * @param rangeMax The maximum range value, inclusive.
	 * @return The given value.
	 * @throws IllegalArgumentException if the value is less than the range minimum or greater than the range maximum.
	 */
	public static int checkArgumentRange(final int value, final int rangeMin, final int rangeMax)
	{
		if(value < rangeMin || value > rangeMax) //if the value not within the range
		{
			throw new IllegalArgumentException("Value " + value + " is not within the range " + rangeMin + " to " + rangeMax);
		}
		return value; //return the value, which has been determined to be within the range
	}

	/**
	 * Checks to make sure that a given argument value is within the given range.
	 * @param value The value to check.
	 * @param rangeMin The minimum range value, inclusive.
	 * @param rangeMax The maximum range value, inclusive.
	 * @return The given value.
	 * @throws IllegalArgumentException if the value is less than the range minimum or greater than the range maximum.
	 */
	public static long checkArgumentRange(final long value, final long rangeMin, final long rangeMax)
	{
		if(value < rangeMin || value > rangeMax) //if the value not within the range
		{
			throw new IllegalArgumentException("Value " + value + " is not within the range " + rangeMin + " to " + rangeMax);
		}
		return value; //return the value, which has been determined to be within the range
	}

	/**
	 * Checks to make sure that a given index is within the range of zero inclusive to the given length exclusive.
	 * @param index The index to check.
	 * @param length The exclusive length.
	 * @return The given index.
	 * @throws IndexOutOfBoundsException if the index is less than zero, or equal to or greater than given length.
	 */
	public static int checkIndexBounds(final int index, final int length)
	{
		return checkIndexBounds(index, 0, length);
	}

	/**
	 * Checks to make sure that a given index is within the range of zero inclusive to the given length exclusive.
	 * @param index The index to check.
	 * @param length The exclusive length.
	 * @return The given index.
	 * @throws IndexOutOfBoundsException if the index is less than zero, or equal to or greater than given length.
	 */
	public static long checkIndexBounds(final long index, final long length)
	{
		return checkIndexBounds(index, 0, length);
	}

	/**
	 * Checks to make sure that a given index is within the given range.
	 * @param index The index to check.
	 * @param rangeMin The minimum range index, inclusive.
	 * @param rangeMax The maximum range index, exclusive.
	 * @return The given index.
	 * @throws IndexOutOfBoundsException if the index is less than the range minimum, or equal to or greater than the range maximum.
	 */
	public static int checkIndexBounds(final int index, final int rangeMin, final int rangeMax)
	{
		if(index < rangeMin || index >= rangeMax) //if the index not within its bounds
		{
			throw new IndexOutOfBoundsException("Index out of bounds: " + index);
		}
		return index; //return the index, which has been determined to be in bounds
	}

	/**
	 * Checks to make sure that a given index is within the given range.
	 * @param index The index to check.
	 * @param rangeMin The minimum range index, inclusive.
	 * @param rangeMax The maximum range index, exclusive.
	 * @return The given index.
	 * @throws IndexOutOfBoundsException if the index is less than the range minimum, or equal to or greater than the range maximum.
	 */
	public static long checkIndexBounds(final long index, final long rangeMin, final long rangeMax)
	{
		if(index < rangeMin || index >= rangeMax) //if the index not within its bounds
		{
			throw new IndexOutOfBoundsException("Index out of bounds: " + index);
		}
		return index; //return the index, which has been determined to be in bounds
	}

}
