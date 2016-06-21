/*
 * Copyright © 2008-2013 GlobalMentor, Inc. <http://www.globalmentor.com/>
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
import java.text.MessageFormat;

import com.globalmentor.model.ConfigurationException;

/**
 * Various checks on objects. Some of these methods indicate <dfn>preconditions</dfn> on objects given as arguments in a method.
 * 
 * <p>
 * The name of this class was inspired by the <code>Preconditions</code> class in <a href="http://code.google.com/p/guava-libraries/">Google Guava Library</a>.
 * </p>
 * 
 * @author Garret Wilson
 */
public class Conditions {

	/**
	 * Checks the results of an expression to see if an argument is correct, and throws an {@link IllegalArgumentException} if the value is <code>false</code>.
	 * <p>
	 * This is a precondition check.
	 * </p>
	 * @param test The result of the test.
	 * @throws IllegalArgumentException if the given value is <code>false</code>.
	 */
	public static void checkArgument(final boolean test) {
		checkArgument(test, null); //check the test with no description
	}

	/**
	 * Checks the results of an expression to see if an argument is correct, and throws an {@link IllegalArgumentException} if the value is <code>false</code>.
	 * <p>
	 * This is a precondition check.
	 * </p>
	 * @param test The result of the test.
	 * @param description A description of the test to be used when generating an exception, optionally formatted with arguments, or <code>null</code> for no
	 *          description.
	 * @param arguments The arguments to be applied when formatting, or an empty array if the message should not be formatted.
	 * @throws NullPointerException if the given arguments is <code>null</code>.
	 * @throws IllegalArgumentException if the given value is <code>false</code>.
	 * @throws IllegalArgumentException if the description is an invalid pattern, or if an argument in the arguments array is not of the type expected by the
	 *           format element(s) that use it.
	 * @see MessageFormat#format(String, Object...)
	 */
	public static void checkArgument(final boolean test, String description, final Object... arguments) {
		if(!test) {
			//format the message if appropriate
			if(description != null && arguments.length > 0) {
				description = MessageFormat.format(description, arguments);
			}
			throw new IllegalArgumentException(description);
		}
	}

	/**
	 * Check to make sure an argument isn't <code>null</code>, throwing {@link IllegalArgumentException} if the object is <code>null</code>.
	 * <p>
	 * This is a precondition check.
	 * </p>
	 * @param <T> The type of the object to be tested.
	 * @param object The object to test.
	 * @return The object, if it is not <code>null</code>
	 * @throws IllegalArgumentException if the given object is <code>null</code>.
	 */
	public static <T> T checkArgumentNotNull(final T object) {
		return checkArgumentNotNull(object, null); //check the object with no description
	}

	/**
	 * Check to make sure an argument isn't <code>null</code>, throwing {@link IllegalArgumentException} if the object is <code>null</code>.
	 * <p>
	 * This is a precondition check.
	 * </p>
	 * @param <T> The type of the object to be tested.
	 * @param object The object to test.
	 * @param description A description of the test to be used when generating an exception, optionally formatted with arguments, or <code>null</code> for no
	 *          description.
	 * @param arguments The arguments to be applied when formatting, or an empty array if the message should not be formatted.
	 * @return The given object.
	 * @throws IllegalArgumentException if the given object is <code>null</code>.
	 * @throws NullPointerException if the given arguments is <code>null</code>.
	 * @throws IllegalArgumentException if the description is an invalid pattern, or if an argument in the arguments array is not of the type expected by the
	 *           format element(s) that use it.
	 * @see MessageFormat#format(String, Object...)
	 */
	public static <T> T checkArgumentNotNull(final T object, String description, final Object... arguments) {
		if(object == null) {
			//format the message if appropriate
			if(description != null && arguments.length > 0) {
				description = MessageFormat.format(description, arguments);
			}
			throw new IllegalArgumentException(description);
		}
		return object;
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
	public static int checkArgumentMinimum(final int value, final int rangeMin) {
		if(value < rangeMin) { //if the value not within the range
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
	public static long checkArgumentMinimum(final long value, final long rangeMin) {
		if(value < rangeMin) { //if the value not within the range
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
	public static int checkArgumentNotNegative(final int value) {
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
	public static int checkArgumentPositive(final int value) {
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
	public static long checkArgumentNotNegative(final long value) {
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
	public static long checkArgumentPositive(final long value) {
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
	public static int checkArgumentRange(final int value, final int rangeMin, final int rangeMax) {
		if(value < rangeMin || value > rangeMax) { //if the value not within the range
			throw new IllegalArgumentException("Value " + value + " is not within the range " + rangeMin + " to " + rangeMax);
		}
		return value; //return the value, which has been determined to be within the range
	}

	/**
	 * Checks to make sure that the given range is within the given range.
	 * <p>
	 * This is a precondition check.
	 * </p>
	 * @param from The beginning value to check, inclusive.
	 * @param to The ending value to check, inclusive.
	 * @param rangeMin The minimum range value, inclusive.
	 * @param rangeMax The maximum range value, inclusive.
	 * @throws IllegalArgumentException if the from value is less than the range minimum or greater than the range maximum; or if the to value is less than the
	 *           from value or greater than the range maximum.
	 */
	public static void checkArgumentRange(final int from, final int to, final int rangeMin, final int rangeMax) {
		if(to < from) {
			throw new IllegalArgumentException("Range from value " + from + " cannot be less than range to value " + to);
		}
		if(from < rangeMin) { //if the from value is below the range
			throw new IllegalArgumentException("Range from value " + from + " is not within the range " + rangeMin + " to " + rangeMax);
		}
		if(to < rangeMax) { //if the to value is above the range
			throw new IllegalArgumentException("Range to value " + to + " is not within the range " + rangeMin + " to " + rangeMax);
		}
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
	public static long checkArgumentRange(final long value, final long rangeMin, final long rangeMax) {
		if(value < rangeMin || value > rangeMax) { //if the value not within the range
			throw new IllegalArgumentException("Value " + value + " is not within the range " + rangeMin + " to " + rangeMax);
		}
		return value; //return the value, which has been determined to be within the range
	}

	/**
	 * Checks the results of an expression to see if an argument is correct, and throws a {@link ConfigurationException} if the value is <code>false</code>.
	 * @param test The result of the test.
	 * @throws ConfigurationException if the given value is <code>false</code>.
	 */
	public static void checkConfiguration(final boolean test) {
		checkArgument(test, null); //check the test with no description
	}

	/**
	 * Checks the results of an expression to see if an argument is correct, and throws a {@link ConfigurationException} if the value is <code>false</code>.
	 * @param test The result of the test.
	 * @param description A description of the test to be used when generating an exception, optionally formatted with arguments, or <code>null</code> for no
	 *          description.
	 * @param arguments The arguments to be applied when formatting, or an empty array if the message should not be formatted.
	 * @throws NullPointerException if the given arguments is <code>null</code>.
	 * @throws ConfigurationException if the given value is <code>false</code>.
	 * @throws IllegalArgumentException if the description is an invalid pattern, or if an argument in the arguments array is not of the type expected by the
	 *           format element(s) that use it.
	 * @see MessageFormat#format(String, Object...)
	 */
	public static void checkConfiguration(final boolean test, String description, final Object... arguments) {
		if(!test) {
			//format the message if appropriate
			if(description != null && arguments.length > 0) {
				description = MessageFormat.format(description, arguments);
			}
			throw new ConfigurationException(description);
		}
	}

	/**
	 * Checks to see if a given variable is an instance of any object, and throws a {@link ConfigurationException} if the variable is <code>null</code>.
	 * @param <T> The type of variable to check.
	 * @param variable The variable to check.
	 * @return The given variable.
	 * @throws ConfigurationException if the given variable is <code>null</code>.
	 */
	public static <T> T checkConfigurationNotNull(final T variable) {
		return checkConfigurationNotNull(variable, null); //check for null with no description
	}

	/**
	 * Checks to see if a given variable is an instance of any object, and throws a {@link ConfigurationException} if the variable is <code>null</code>.
	 * @param <T> The type of variable to check.
	 * @param variable The variable to check.
	 * @param description A description of the variable to be used when generating an exception, or <code>null</code> for no description.
	 * @return The given variable.
	 * @throws NullPointerException if the given variable is <code>null</code>.
	 */
	public static <T> T checkConfigurationNotNull(final T variable, final String description) {
		if(variable == null) { //if the variable is null
			throw new ConfigurationException(description);
		}
		return variable; //return the variable
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
	public static int checkIndexBounds(final int index, final int length) {
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
	public static long checkIndexBounds(final long index, final long length) {
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
	public static int checkIndexBounds(final int index, final int rangeMin, final int rangeMax) {
		if(index < rangeMin || index >= rangeMax) { //if the index not within its bounds
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
	public static long checkIndexBounds(final long index, final long rangeMin, final long rangeMax) {
		if(index < rangeMin || index >= rangeMax) { //if the index not within its bounds
			throw new IndexOutOfBoundsException("Index out of bounds: " + index);
		}
		return index; //return the index, which has been determined to be in bounds
	}

	/**
	 * Checks to make sure a given state is <code>true</code>.
	 * @param state The state to check.
	 * @throws IllegalStateException if the given state is <code>false</code>.
	 */
	public static void checkState(final boolean state) {
		checkState(state, null);
	}

	/**
	 * Checks to make sure a given state is <code>true</code>.
	 * @param state The state to check.
	 * @param description A description of the state to be used when generating an exception, or <code>null</code> for no description.
	 * @throws IllegalStateException if the given state is <code>false</code>.
	 */
	public static void checkState(final boolean state, final String description) {
		if(!state) {
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
	public static IllegalStateException unexpected(final String message) {
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
	public static IllegalStateException unexpected(final Throwable cause) {
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
	public static IllegalStateException unexpected(final String message, final Throwable cause) {
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
	 * @return A throwable indicating an impossible condition.
	 */
	public static AssertionError impossible() {
		return impossible((String)null);
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
	public static AssertionError impossible(final String message) {
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
	public static AssertionError impossible(final Throwable cause) {
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
	public static AssertionError impossible(final String message, final Throwable cause) {
		AssertionError impossible = message != null ? new AssertionError(message) : new AssertionError();
		if(cause != null) {
			impossible = (AssertionError)impossible.initCause(cause);
		}
		return impossible;
	}

}
