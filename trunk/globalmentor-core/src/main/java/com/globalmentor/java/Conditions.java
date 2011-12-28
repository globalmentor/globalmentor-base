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

import java.io.UnsupportedEncodingException;

/**
 * Various checks on objects. Some of these methods indicate <dfn>preconditions</dfn> on objects given as arguments in a method.
 * 
 * <p>
 * The name of this class was inspired by the <code>Preconditions</code> class in <a href="http://code.google.com/p/guava-libraries/">Google Guava Library</a>.
 * </p>
 * 
 * @author Garret Wilson
 */
public class Conditions
{

	/**
	 * Checks the results of an expression to see if an argument is correct, and throws an {@link IllegalArgumentException} if the value is <code>false</code>.
	 * <p>
	 * This is a precondition check.
	 * </p>
	 * @param test The result of the test.
	 * @throws IllegalArgumentException if the given value is <code>false</code>
	 */
	public static void checkArgument(final boolean test)
	{
		checkArgument(test, null); //check the test with no description
	}

	/**
	 * Checks the results of an expression to see if an argument is correct, and throws an {@link IllegalArgumentException} if the value is <code>false</code>.
	 * <p>
	 * This is a precondition check.
	 * </p>
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
	 * <p>
	 * This is a precondition check.
	 * </p>
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
	 * <p>
	 * This is a precondition check.
	 * </p>
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
	 * Checks to make sure that a given value is not negative.
	 * <p>
	 * This is a precondition check.
	 * </p>
	 * @param value The value to check.
	 * @throws IllegalArgumentException if the value is negative.
	 * @return The given value.
	 * @see #checkArgumentMinimum(int, int)
	 */
	public static int checkArgumentNotNegative(final int value)
	{
		return checkArgumentMinimum(value, 0);
	}

	/**
	 * Checks to make sure that a given value is not zero or negative
	 * <p>
	 * This is a precondition check.
	 * </p>
	 * @param value The value to check.
	 * @throws IllegalArgumentException if the value is not positive.
	 * @return The given value.
	 * @see #checkArgumentMinimum(int, int)
	 */
	public static int checkArgumentPositive(final int value)
	{
		return checkArgumentMinimum(value, 1);
	}

	/**
	 * Checks to make sure that a given value is not negative.
	 * <p>
	 * This is a precondition check.
	 * </p>
	 * @param value The value to check.
	 * @throws IllegalArgumentException if the value is negative.
	 * @return The given value.
	 * @see #checkArgumentMinimum(long, long)
	 */
	public static long checkArgumentNotNegative(final long value)
	{
		return checkArgumentMinimum(value, 0);
	}

	/**
	 * Checks to make sure that a given value is not zero or negative
	 * <p>
	 * This is a precondition check.
	 * </p>
	 * @param value The value to check.
	 * @throws IllegalArgumentException if the value is not positive.
	 * @return The given value.
	 * @see #checkArgumentMinimum(long, long)
	 */
	public static long checkArgumentPositive(final long value)
	{
		return checkArgumentMinimum(value, 1);
	}

	/**
	 * Checks to make sure that a given argument value is within the given range.
	 * <p>
	 * This is a precondition check.
	 * </p>
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
	 * <p>
	 * This is a precondition check.
	 * </p>
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
	 * <p>
	 * This is normally a precondition check.
	 * </p>
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
	 * <p>
	 * This is normally a precondition check.
	 * </p>
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
	 * <p>
	 * This is normally a precondition check.
	 * </p>
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
	 * <p>
	 * This is normally a precondition check.
	 * </p>
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

	/**
	 * Checks to make sure a given state is <code>true</code>.
	 * @param state The state to check.
	 * @throws IllegalStateException if the given state is <code>false</code>.
	 */
	public static void checkState(final boolean state)
	{
		checkState(state, null);
	}

	/**
	 * Checks to make sure a given state is <code>true</code>.
	 * @param state The state to check.
	 * @param description A description of the state to be used when generating an exception, or <code>null</code> for no description.
	 * @throws IllegalStateException if the given state is <code>false</code>.
	 */
	public static void checkState(final boolean state, final String description)
	{
		if(!state)
		{
			throw new IllegalStateException(description);
		}
	}

	/**
	 * Creates a throwable indicating an unexpected condition, which can be thrown by the caller.
	 * <p>
	 * An <dfn>unexpected</dfn> condition is one that is logically possible to occur, but yet should not occur because of an API contract, for instance. For
	 * example, all JVMs should support UTF-8, so one would not expect an {@link UnsupportedEncodingException} if UTF-8 is explicitly specified.
	 * </p>
	 * <p>
	 * This is a convenience method that makes the semantics of the condition more readily apparent.
	 * </p>
	 * @param message The detail message, or <code>null</code> if there is no detail message.
	 * @return A throwable indicating an unexpected condition.
	 */
	public static IllegalStateException unexpected(final String message)
	{
		return unexpected(message, null);
	}

	/**
	 * Creates a throwable indicating an unexpected condition, which can be thrown by the caller.
	 * <p>
	 * An <dfn>unexpected</dfn> condition is one that is logically possible to occur, but yet should not occur because of an API contract, for instance. For
	 * example, all JVMs should support UTF-8, so one would not expect an {@link UnsupportedEncodingException} if UTF-8 is explicitly specified.
	 * </p>
	 * <p>
	 * This is a convenience method that makes the semantics of the condition more readily apparent.
	 * </p>
	 * @param cause The throwable cause of the unexpected condition, or <code>null</code> if a throwable cause is nonexistent or unknown.
	 * @return A throwable indicating an unexpected condition.
	 */
	public static IllegalStateException unexpected(final Throwable cause)
	{
		return unexpected(null, cause);
	}

	/**
	 * Creates a throwable indicating an unexpected condition, which can be thrown by the caller.
	 * <p>
	 * An <dfn>unexpected</dfn> condition is one that is logically possible to occur, but yet should not occur because of an API contract, for instance. For
	 * example, all JVMs should support UTF-8, so one would not expect an {@link UnsupportedEncodingException} if UTF-8 is explicitly specified.
	 * </p>
	 * <p>
	 * This is a convenience method that makes the semantics of the condition more readily apparent.
	 * </p>
	 * @param message The detail message, or <code>null</code> if there is no detail message.
	 * @param cause The throwable cause of the unexpected condition, or <code>null</code> if a throwable cause is nonexistent or unknown.
	 * @return A throwable indicating an unexpected condition.
	 */
	public static IllegalStateException unexpected(final String message, final Throwable cause)
	{
		return new IllegalStateException(message, cause);
	}

	/**
	 * Creates a throwable indicating a logically impossible condition, which can be thrown by the caller.
	 * <p>
	 * An <dfn>impossible</dfn> condition is one that is supposedly logically impossible to occur. For example, a default section of a switch statement covering
	 * all possible values of an enum should never be executed; the throwable produced by this method provides a convenient outcome to indicate this condition
	 * (for example, if a new enum value is later introduced and the switch statement isn't updated). The code following a method guaranteed to throw an exception
	 * is another example.
	 * </p>
	 * <p>
	 * This is a convenience method that makes the semantics of the condition more readily apparent.
	 * </p>
	 * @param message The detail message, or <code>null</code> if there is no detail message.
	 * @return A throwable indicating an impossible condition.
	 */
	public static AssertionError impossible(final String message)
	{
		return impossible(message, null);
	}

	/**
	 * Creates a throwable indicating a logically impossible condition, which can be thrown by the caller.
	 * <p>
	 * An <dfn>impossible</dfn> condition is one that is supposedly logically impossible to occur. For example, a default section of a switch statement covering
	 * all possible values of an enum should never be executed; the throwable produced by this method provides a convenient outcome to indicate this condition
	 * (for example, if a new enum value is later introduced and the switch statement isn't updated). The code following a method guaranteed to throw an exception
	 * is another example.
	 * </p>
	 * <p>
	 * This is a convenience method that makes the semantics of the condition more readily apparent.
	 * </p>
	 * @param cause The throwable cause of the impossible condition, or <code>null</code> if a throwable cause is nonexistent or unknown.
	 * @return A throwable indicating an impossible condition.
	 */
	public static AssertionError impossible(final Throwable cause)
	{
		return impossible(null, cause);
	}

	/**
	 * Creates a throwable indicating a logically impossible condition, which can be thrown by the caller.
	 * <p>
	 * An <dfn>impossible</dfn> condition is one that is supposedly logically impossible to occur. For example, a default section of a switch statement covering
	 * all possible values of an enum should never be executed; the throwable produced by this method provides a convenient outcome to indicate this condition
	 * (for example, if a new enum value is later introduced and the switch statement isn't updated). The code following a method guaranteed to throw an exception
	 * is another example.
	 * </p>
	 * <p>
	 * This is a convenience method that makes the semantics of the condition more readily apparent.
	 * </p>
	 * @param message The detail message, or <code>null</code> if there is no detail message.
	 * @param cause The throwable cause of the impossible condition, or <code>null</code> if a throwable cause is nonexistent or unknown.
	 * @return A throwable indicating an impossible condition.
	 */
	public static AssertionError impossible(final String message, final Throwable cause)
	{
		AssertionError impossible = message != null ? new AssertionError(message) : new AssertionError();
		if(cause != null)
		{
			impossible = (AssertionError)impossible.initCause(cause);
		}
		return impossible;
	}

}
