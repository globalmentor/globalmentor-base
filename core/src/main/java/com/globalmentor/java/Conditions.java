/*
 * Copyright © 2008-2013 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import java.io.UnsupportedEncodingException;
import java.util.Optional;
import java.util.function.*;

import org.jspecify.annotations.*;

import com.globalmentor.model.ConfiguredStateException;

/**
 * Various checks on objects. Some of these methods indicate <dfn>preconditions</dfn> on objects given as arguments in a method.
 * @apiNote The name of this class was inspired by the
 *          <a href="https://guava.dev/releases/snapshot-jre/api/docs/com/google/common/base/Preconditions.html"><code>Preconditions</code></a> class in
 *          <a href="https://github.com/google/guava">Google Guava Library</a>.
 * @author Garret Wilson
 */
public final class Conditions {

	private Conditions() {
	}

	/**
	 * Checks the result of an expression to see if an argument is correct, and throws an {@link IllegalArgumentException} if the value is <code>false</code>.
	 * @apiNote This is a precondition check.
	 * @param test The result of the test.
	 * @throws IllegalArgumentException if the given value is <code>false</code>.
	 */
	public static void checkArgument(final boolean test) {
		checkArgument(test, null); //check the test with no description
	}

	/**
	 * Checks the result of an expression to see if an argument is correct, and throws an {@link IllegalArgumentException} if the value is <code>false</code>.
	 * @apiNote This is a precondition check.
	 * @param test The result of the test.
	 * @param description A description of the test to be used when generating an exception, optionally formatted with arguments, or <code>null</code> for no
	 *          description.
	 * @param arguments The arguments to be applied when formatting, or an empty array if the message should not be formatted.
	 * @throws NullPointerException if the given arguments is <code>null</code>.
	 * @throws IllegalArgumentException if the given value is <code>false</code>.
	 * @throws IllegalArgumentException if the description is an invalid pattern, or if an argument in the arguments array is not of the type expected by the
	 *           format element(s) that use it.
	 * @see String#format(String, Object...)
	 */
	public static void checkArgument(final boolean test, String description, final Object... arguments) {
		if(!test) { //format the message if appropriate
			if(description != null && arguments.length > 0) {
				description = String.format(description, arguments);
			}
			throw new IllegalArgumentException(description);
		}
	}

	/**
	 * Creates a consumer to check the result of a predicate to see if an argument is correct, and throws an {@link IllegalArgumentException} if the value is
	 * <code>false</code>.
	 * @apiNote This method is useful for checking an optionally present value using e.g. {@link Optional#ifPresent(Consumer)}.
	 * @apiNote This is a precondition check.
	 * @param <T> The type of object being tested.
	 * @param predicate The predicate to test.
	 * @return A consumer that will throw {@link IllegalArgumentException} if the result of the predicate is <code>false</code>.
	 */
	public static <T> Consumer<T> checkArgument(@NonNull final Predicate<? super T> predicate) {
		return checkArgument(predicate, null); //check the test with no description
	}

	/**
	 * Creates a consumer to check the result of a predicate to see if an argument is correct, and throws an {@link IllegalArgumentException} if the value is
	 * <code>false</code>.
	 * @apiNote This method is useful for checking an optionally present value using e.g. {@link Optional#ifPresent(Consumer)}.
	 * @apiNote This is a precondition check.
	 * @param <T> The type of object being tested.
	 * @param predicate The predicate to test.
	 * @param description A description of the test to be used when generating an exception, optionally formatted with arguments, or <code>null</code> for no
	 *          description.
	 * @param arguments The arguments to be applied when formatting, or an empty array if the message should not be formatted.
	 * @return A consumer that will throw {@link IllegalArgumentException} if the result of the predicate is <code>false</code>; or {@link NullPointerException}
	 *         if the given arguments is <code>null</code>; or {@link IllegalArgumentException} if the description is an invalid pattern, or if an argument in the
	 *         arguments array is not of the type expected by the format element(s) that use it.
	 * @see String#format(String, Object...)
	 */
	public static <T> Consumer<T> checkArgument(@NonNull final Predicate<? super T> predicate, String description, final Object... arguments) {
		return __ -> checkArgument(predicate.test(__));
	}

	/**
	 * Checks to make sure an argument is an instance of a given type, returning the object cast to the appropriate type.
	 * @apiNote This is a precondition check.
	 * @param <T> The expected type of the object.
	 * @param object The object to test.
	 * @param instanceClass The class of which to test that the object is an instance.
	 * @return The given object, cast to the given type.
	 * @throws IllegalArgumentException if the given object not of the indicated type.
	 */
	public static <T> T checkArgumentIsInstance(final Object object, @NonNull final Class<T> instanceClass) {
		final String instanceClassCanonicalName = instanceClass.getCanonicalName(); //the canonical name is more user-friendly for named inner classes, although it will be missing for e.g. anonymous inner classes
		return checkArgumentIsInstance(object, instanceClass, "Argument not instance of class `%s`.",
				instanceClassCanonicalName != null ? instanceClassCanonicalName : instanceClass.getName());
	}

	/**
	 * Checks to make sure an argument is an instance of a given type, returning the object cast to the appropriate type.
	 * @apiNote This is a precondition check.
	 * @param <T> The expected type of the object.
	 * @param object The object to test.
	 * @param instanceClass The class of which to test that the object is an instance.
	 * @param description A description of the test to be used when generating an exception, optionally formatted with arguments, or <code>null</code> for no
	 *          description.
	 * @param arguments The arguments to be applied when formatting, or an empty array if the message should not be formatted.
	 * @return The given object, cast to the given type.
	 * @throws IllegalArgumentException if the given object not of the indicated type.
	 * @throws NullPointerException if the given arguments is <code>null</code>.
	 * @throws IllegalArgumentException if the description is an invalid pattern, or if an argument in the arguments array is not of the type expected by the
	 *           format element(s) that use it.
	 * @see String#format(String, Object...)
	 */
	public static <T> T checkArgumentIsInstance(final Object object, @NonNull final Class<T> instanceClass, @Nullable String description,
			@NonNull final Object... arguments) {
		checkArgument(instanceClass.isInstance(object), description, arguments);
		return instanceClass.cast(object);
	}

	/**
	 * Checks to make sure an argument isn't <code>null</code>, throwing {@link IllegalArgumentException} if the object is <code>null</code>.
	 * @apiNote This is a precondition check.
	 * @apiNote Normally an idiomatic Java precondition check for a <code>null</code> argument should throw {@link NullPointerException} instead of an
	 *          {@link IllegalArgumentException}.
	 * @param <T> The type of the object to be tested.
	 * @param object The object to test.
	 * @return The object, if it is not <code>null</code>.
	 * @throws IllegalArgumentException if the given object is <code>null</code>.
	 */
	public static <T> T checkArgumentNotNull(final T object) {
		return checkArgumentNotNull(object, "Argument cannot be `null`.");
	}

	/**
	 * Checks to make sure an argument isn't <code>null</code>, throwing {@link IllegalArgumentException} if the object is <code>null</code>.
	 * @apiNote This is a precondition check.
	 * @apiNote Normally in Java a precondition check for a <code>null</code> argument should throw {@link NullPointerException}.
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
	 * @see String#format(String, Object...)
	 */
	public static <T> T checkArgumentNotNull(final T object, String description, final Object... arguments) {
		if(object == null) { //format the message if appropriate
			if(description != null && arguments.length > 0) {
				description = String.format(description, arguments);
			}
			throw new IllegalArgumentException(description);
		}
		return object;
	}

	/**
	 * Checks to make sure an argument isn't empty, throwing {@link IllegalArgumentException} if the string is empty.
	 * @apiNote This is a precondition check.
	 * @param string The string to test.
	 * @return The string, if it is not empty.
	 * @throws IllegalArgumentException if the given string is empty.
	 * @see String#isEmpty()
	 */
	public static String checkArgumentNotEmpty(final String string) {
		return checkArgumentNotEmpty(string, "Argument cannot be empty.");
	}

	/**
	 * Checks to make sure an argument isn't empty, throwing {@link IllegalArgumentException} if the string is empty.
	 * @apiNote This is a precondition check.
	 * @param string The string to test.
	 * @param description A description of the test to be used when generating an exception, optionally formatted with arguments, or <code>null</code> for no
	 *          description.
	 * @param arguments The arguments to be applied when formatting, or an empty array if the message should not be formatted.
	 * @return The given string.
	 * @throws IllegalArgumentException if the given string is empty.
	 * @throws NullPointerException if the given arguments is <code>null</code>.
	 * @throws IllegalArgumentException if the description is an invalid pattern, or if an argument in the arguments array is not of the type expected by the
	 *           format element(s) that use it.
	 * @see String#format(String, Object...)
	 * @see String#isEmpty()
	 */
	public static String checkArgumentNotEmpty(final String string, String description, final Object... arguments) {
		if(string.isEmpty()) { //format the message if appropriate
			if(description != null && arguments.length > 0) {
				description = String.format(description, arguments);
			}
			throw new IllegalArgumentException(description);
		}
		return string;
	}

	/**
	 * Checks to make sure an argument isn't blank, throwing {@link IllegalArgumentException} if the string is blank.
	 * @apiNote This is a precondition check.
	 * @param string The string to test.
	 * @return The string, if it is not blank.
	 * @throws IllegalArgumentException if the given string is blank.
	 * @see String#isBlank()
	 */
	public static String checkArgumentNotBlank(final String string) {
		return checkArgumentNotBlank(string, "Argument cannot be blank.");
	}

	/**
	 * Checks to make sure an argument isn't blank, throwing {@link IllegalArgumentException} if the string is blank.
	 * @apiNote This is a precondition check.
	 * @param string The string to test.
	 * @param description A description of the test to be used when generating an exception, optionally formatted with arguments, or <code>null</code> for no
	 *          description.
	 * @param arguments The arguments to be applied when formatting, or an empty array if the message should not be formatted.
	 * @return The given string.
	 * @throws IllegalArgumentException if the given string is blank.
	 * @throws NullPointerException if the given arguments is <code>null</code>.
	 * @throws IllegalArgumentException if the description is an invalid pattern, or if an argument in the arguments array is not of the type expected by the
	 *           format element(s) that use it.
	 * @see String#format(String, Object...)
	 * @see String#isBlank()
	 */
	public static String checkArgumentNotBlank(final String string, String description, final Object... arguments) {
		if(string.isBlank()) { //format the message if appropriate
			if(description != null && arguments.length > 0) {
				description = String.format(description, arguments);
			}
			throw new IllegalArgumentException(description);
		}
		return string;
	}

	/**
	 * Checks to make sure an argument is present, throwing {@link IllegalArgumentException} if the optional is not present.
	 * @apiNote This is a precondition check.
	 * @param <T> The type of optional object to be tested.
	 * @param optional The optional object to test.
	 * @return The optional object.
	 * @throws NullPointerException if the given optional is <code>null</code>.
	 * @throws IllegalArgumentException if the given optional is not present.
	 * @see Optional#isPresent()
	 * @see Optional#get()
	 */
	public static <T> T checkArgumentPresent(@NonNull final Optional<T> optional) {
		return checkArgumentPresent(optional, "Argument not present.");
	}

	/**
	 * Checks to make sure an argument is present, throwing {@link IllegalArgumentException} if the optional is not present.
	 * @apiNote This is a precondition check.
	 * @param <T> The type of optional object to be tested.
	 * @param optional The optional object to test.
	 * @param description A description of the test to be used when generating an exception, optionally formatted with arguments, or <code>null</code> for no
	 *          description.
	 * @param arguments The arguments to be applied when formatting, or an empty array if the message should not be formatted.
	 * @return The optional object.
	 * @throws NullPointerException if the given optional and/or arguments is <code>null</code>.
	 * @throws IllegalArgumentException if the given optional is not present, or if an argument in the arguments array is not of the type expected by the format
	 *           element(s) that use it.
	 * @see String#format(String, Object...)
	 * @see Optional#isPresent()
	 * @see Optional#get()
	 */
	public static <T> T checkArgumentPresent(@NonNull final Optional<T> optional, @Nullable final String description, @NonNull final Object... arguments) {
		return optional.orElseThrow(() -> {
			final String message = description != null && arguments.length > 0 ? String.format(description, arguments) : description;
			return new IllegalArgumentException(message);
		});
	}

	/**
	 * Checks to make sure that a given value is not smaller than the given minimum.
	 * @apiNote This is a precondition check.
	 * @param value The value to check.
	 * @param rangeMin The minimum range value, inclusive.
	 * @throws IllegalArgumentException if the value is less than the range minimum.
	 * @return The given value.
	 */
	public static int checkArgumentMinimum(final int value, final int rangeMin) {
		if(value < rangeMin) { //if the value not within the range
			throw new IllegalArgumentException(String.format("Value %d cannot be less than %d", value, rangeMin));
		}
		return value; //return the value, which has been determined to be within the range
	}

	/**
	 * Checks to make sure that a given value is not smaller than the given minimum.
	 * @apiNote This is a precondition check.
	 * @param value The value to check.
	 * @param rangeMin The minimum range value, inclusive.
	 * @throws IllegalArgumentException if the value is less than the range minimum.
	 * @return The given value.
	 */
	public static long checkArgumentMinimum(final long value, final long rangeMin) {
		if(value < rangeMin) { //if the value not within the range
			throw new IllegalArgumentException(String.format("Value %d cannot be less than %d", value, rangeMin));
		}
		return value; //return the value, which has been determined to be within the range
	}

	/**
	 * Checks to make sure that a given value is not negative.
	 * @apiNote This is a precondition check.
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
	 * @apiNote This is a precondition check.
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
	 * @apiNote This is a precondition check.
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
	 * @apiNote This is a precondition check.
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
	 * @apiNote This is a precondition check.
	 * @param value The value to check.
	 * @param rangeMin The minimum range value, inclusive.
	 * @param rangeMax The maximum range value, inclusive.
	 * @return The given value.
	 * @throws IllegalArgumentException if the value is less than the range minimum or greater than the range maximum.
	 */
	public static int checkArgumentRange(final int value, final int rangeMin, final int rangeMax) {
		if(value < rangeMin || value > rangeMax) { //if the value not within the range
			throw new IllegalArgumentException(String.format("Value %d is not within the range %d to %d", value, rangeMin, rangeMax));
		}
		return value; //return the value, which has been determined to be within the range
	}

	/**
	 * Checks to make sure that the given range is within the given range.
	 * @apiNote This is a precondition check.
	 * @param from The beginning value to check, inclusive.
	 * @param to The ending value to check, inclusive.
	 * @param rangeMin The minimum range value, inclusive.
	 * @param rangeMax The maximum range value, inclusive.
	 * @throws IllegalArgumentException if the from value is less than the range minimum or greater than the range maximum; or if the to value is less than the
	 *           from value or greater than the range maximum.
	 */
	public static void checkArgumentRange(final int from, final int to, final int rangeMin, final int rangeMax) {
		if(to < from) {
			throw new IllegalArgumentException(String.format("Range from value %d cannot be less than range to value %d", from, to));
		}
		if(from < rangeMin) { //if the from value is below the range
			throw new IllegalArgumentException(String.format("Range from value %d is not within the range %d to %d", from, rangeMin, rangeMax));
		}
		if(to > rangeMax) { //if the to value is above the range
			throw new IllegalArgumentException(String.format("Range to value %d is not within the range %d to %d", to, rangeMin, rangeMax));
		}
	}

	/**
	 * Checks to make sure that a given argument value is within the given range.
	 * @apiNote This is a precondition check.
	 * @param value The value to check.
	 * @param rangeMin The minimum range value, inclusive.
	 * @param rangeMax The maximum range value, inclusive.
	 * @return The given value.
	 * @throws IllegalArgumentException if the value is less than the range minimum or greater than the range maximum.
	 */
	public static long checkArgumentRange(final long value, final long rangeMin, final long rangeMax) {
		if(value < rangeMin || value > rangeMax) { //if the value not within the range
			throw new IllegalArgumentException(String.format("Value %d is not within the range %d to %d", value, rangeMin, rangeMax));
		}
		return value; //return the value, which has been determined to be within the range
	}

	/**
	 * Checks the result of an expression to see if an argument is correct, and throws a {@link ConfiguredStateException} if the value is <code>false</code>.
	 * @param test The result of the test.
	 * @throws ConfiguredStateException if the given value is <code>false</code>.
	 */
	public static void checkConfiguredState(final boolean test) {
		checkConfiguredState(test, null); //check the test with no description
	}

	/**
	 * Checks the result of an expression to see if an argument is correct, and throws a {@link ConfiguredStateException} if the value is <code>false</code>.
	 * @param test The result of the test.
	 * @param description A description of the test to be used when generating an exception, optionally formatted with arguments, or <code>null</code> for no
	 *          description.
	 * @param arguments The arguments to be applied when formatting, or an empty array if the message should not be formatted.
	 * @throws NullPointerException if the given arguments is <code>null</code>.
	 * @throws ConfiguredStateException if the given value is <code>false</code>.
	 * @throws IllegalArgumentException if the description is an invalid pattern, or if an argument in the arguments array is not of the type expected by the
	 *           format element(s) that use it.
	 * @see String#format(String, Object...)
	 */
	public static void checkConfiguredState(final boolean test, String description, final Object... arguments) {
		if(!test) { //format the message if appropriate
			if(description != null && arguments.length > 0) {
				description = String.format(description, arguments);
			}
			throw new ConfiguredStateException(description);
		}
	}

	/**
	 * Creates a consumer to check the result of an expression to see if an argument is correct, and throws a {@link ConfiguredStateException} if the value is
	 * <code>false</code>.
	 * @apiNote This method is useful for checking an optionally present value using e.g. {@link Optional#ifPresent(Consumer)}.
	 * @param <T> The type of object being tested.
	 * @param predicate The predicate to test.
	 * @return A consumer that will throw {@link ConfiguredStateException} if the given value is <code>false</code>.
	 */
	public static <T> Consumer<T> checkConfiguredState(@NonNull final Predicate<? super T> predicate) {
		return checkConfiguredState(predicate, null); //check the test with no description
	}

	/**
	 * Creates a consumer to check the result of an expression to see if an argument is correct, and throws a {@link ConfiguredStateException} if the value is
	 * <code>false</code>.
	 * @apiNote This method is useful for checking an optionally present value using e.g. {@link Optional#ifPresent(Consumer)}.
	 * @param <T> The type of object being tested.
	 * @param predicate The predicate to test.
	 * @param description A description of the test to be used when generating an exception, optionally formatted with arguments, or <code>null</code> for no
	 *          description.
	 * @param arguments The arguments to be applied when formatting, or an empty array if the message should not be formatted.
	 * @return A consumer that will throw {@link ConfiguredStateException} if the given value is <code>false</code>; or {@link NullPointerException} if the given
	 *         arguments is <code>null</code>; or {@link IllegalArgumentException} if the description is an invalid pattern, or if an argument in the arguments
	 *         array is not of the type expected by the format element(s) that use it.
	 * @see String#format(String, Object...)
	 */
	public static <T> Consumer<T> checkConfiguredState(@NonNull final Predicate<? super T> predicate, String description, final Object... arguments) {
		return __ -> checkConfiguredState(predicate.test(__), description, arguments);
	}

	/**
	 * Checks to see if a given variable is an instance of any object, and throws a {@link ConfiguredStateException} if the variable is <code>null</code>.
	 * @param <T> The type of variable to check.
	 * @param variable The variable to check.
	 * @return The given variable.
	 * @throws ConfiguredStateException if the given variable is <code>null</code>.
	 */
	public static <T> T checkConfiguredStateNotNull(final T variable) {
		return checkConfiguredStateNotNull(variable, "Configured state cannot be `null`.");
	}

	/**
	 * Checks to see if a given variable is an instance of any object, and throws a {@link ConfiguredStateException} if the variable is <code>null</code>.
	 * @param <T> The type of variable to check.
	 * @param variable The variable to check.
	 * @param description A description of the variable to be used when generating an exception, or <code>null</code> for no description.
	 * @return The given variable.
	 * @throws NullPointerException if the given variable is <code>null</code>.
	 */
	public static <T> T checkConfiguredStateNotNull(final T variable, final String description) {
		if(variable == null) { //if the variable is null
			throw new ConfiguredStateException(description);
		}
		return variable; //return the variable
	}

	/**
	 * Checks to make sure that a given index is within the range of zero inclusive to the given length exclusive.
	 * <p>This is normally a precondition check.</p>
	 * @apiNote Java 9 introduced an equivalent method {@link java.util.Objects#checkIndex(int, int)}.
	 * @param index The index to check.
	 * @param length The exclusive length.
	 * @return The given index.
	 * @throws IndexOutOfBoundsException if the index is less than zero, or equal to or greater than given length.
	 * @see java.util.Objects#checkIndex(int, int)
	 */
	public static int checkIndexBounds(final int index, final int length) {
		return checkIndexBounds(index, 0, length);
	}

	/**
	 * Checks to make sure that a given index is within the range of zero inclusive to the given length exclusive.
	 * <p>This is normally a precondition check.</p>
	 * @apiNote Java 16 introduced an equivalent method in {@link java.util.Objects#checkIndex(long, long)}.
	 * @param index The index to check.
	 * @param length The exclusive length.
	 * @return The given index.
	 * @throws IndexOutOfBoundsException if the index is less than zero, or equal to or greater than given length.
	 * @see java.util.Objects#checkIndex(long, long)
	 */
	public static long checkIndexBounds(final long index, final long length) {
		return checkIndexBounds(index, 0, length);
	}

	/**
	 * Checks to make sure that a given index is within the given range.
	 * <p>This is normally a precondition check.</p>
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
	 * <p>This is normally a precondition check.</p>
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
	 * @param description A description of the state to be used when generating an exception, or <code>null</code> for no description. The message is formed by
	 *          replacing each %s placeholder in the template with an argument. These are matched by position - the first %s gets errorMessageArgs[0], etc.
	 * @param errorMessageArgs The arguments to be substituted into the message template.
	 * @throws IllegalStateException if the given state is <code>false</code>.
	 */
	public static void checkState(final boolean state, String description, final Object... errorMessageArgs) {
		if(!state) {
			if(description != null && description.length() > 0) {
				description = String.format(description, errorMessageArgs);
			}

			throw new IllegalStateException(description);
		}
	}

	/**
	 * Checks the result of an expression to see if an operation is supported, and throws an {@link UnsupportedOperationException} if the value is
	 * <code>false</code>.
	 * @param test The result of the test.
	 * @throws UnsupportedOperationException if the given value is <code>false</code>.
	 */
	public static void checkSupportedOperation(final boolean test) {
		checkSupportedOperation(test, null); //check the test with no description
	}

	/**
	 * Checks the result of an expression to see if an operation is supported, and throws an {@link UnsupportedOperationException} if the value is
	 * <code>false</code>.
	 * @param test The result of the test.
	 * @param description A description of the test to be used when generating an exception, optionally formatted with arguments, or <code>null</code> for no
	 *          description.
	 * @param arguments The arguments to be applied when formatting, or an empty array if the message should not be formatted.
	 * @throws NullPointerException if the given arguments is <code>null</code>.
	 * @throws UnsupportedOperationException if the given value is <code>false</code>.
	 * @throws IllegalArgumentException if the description is an invalid pattern, or if an argument in the arguments array is not of the type expected by the
	 *           format element(s) that use it.
	 * @see String#format(String, Object...)
	 */
	public static void checkSupportedOperation(final boolean test, String description, final Object... arguments) {
		if(!test) { //format the message if appropriate
			if(description != null && arguments.length > 0) {
				description = String.format(description, arguments);
			}
			throw new UnsupportedOperationException(description);
		}
	}

	/**
	 * Creates a consumer to check the result of a predicate to see if an operation is supported, and throws an {@link UnsupportedOperationException} if the value
	 * is <code>false</code>.
	 * @apiNote This method is useful for checking an optionally present value using e.g. {@link Optional#ifPresent(Consumer)}.
	 * @param <T> The type of object being tested.
	 * @param predicate The predicate to test.
	 * @return A consumer that will throw {@link UnsupportedOperationException} if the given value is <code>false</code>.
	 */
	public static <T> Consumer<T> checkSupportedOperation(@NonNull final Predicate<? super T> predicate) {
		return checkSupportedOperation(predicate, null); //check the test with no description
	}

	/**
	 * Creates a consumer to check the result of a predicate to see if an operation is supported, and throws an {@link UnsupportedOperationException} if the value
	 * is <code>false</code>.
	 * @apiNote This method is useful for checking an optionally present value using e.g. {@link Optional#ifPresent(Consumer)}.
	 * @param <T> The type of object being tested.
	 * @param predicate The predicate to test.
	 * @param description A description of the test to be used when generating an exception, optionally formatted with arguments, or <code>null</code> for no
	 *          description.
	 * @param arguments The arguments to be applied when formatting, or an empty array if the message should not be formatted.
	 * @return A consumer that will throw {@link UnsupportedOperationException} if the given value is <code>false</code>; or {@link NullPointerException} if the
	 *         given arguments is <code>null</code>; or {@link IllegalArgumentException} if the description is an invalid pattern, or if an argument in the
	 *         arguments array is not of the type expected by the format element(s) that use it.
	 * @see String#format(String, Object...)
	 */
	public static <T> Consumer<T> checkSupportedOperation(@NonNull final Predicate<? super T> predicate, String description, final Object... arguments) {
		return __ -> checkSupportedOperation(predicate.test(__), description, arguments);
	}

	/**
	 * Creates a throwable indicating an unexpected condition, which can be thrown by the caller.
	 * <p>An <dfn>unexpected</dfn> condition is one that is logically possible to occur, but yet should not occur because of an API contract, for instance. For
	 * example, all JVMs should support UTF-8, so one would not expect an {@link UnsupportedEncodingException} if UTF-8 is explicitly specified.</p>
	 * <p>This is a convenience method that makes the semantics of the condition more readily apparent.</p>
	 * @param message The detail message, or <code>null</code> if there is no detail message.
	 * @return A throwable indicating an unexpected condition.
	 */
	public static IllegalStateException unexpected(final String message) {
		return unexpected(message, null);
	}

	/**
	 * Creates a throwable indicating an unexpected condition, which can be thrown by the caller.
	 * <p>An <dfn>unexpected</dfn> condition is one that is logically possible to occur, but yet should not occur because of an API contract, for instance. For
	 * example, all JVMs should support UTF-8, so one would not expect an {@link UnsupportedEncodingException} if UTF-8 is explicitly specified.</p>
	 * <p>This is a convenience method that makes the semantics of the condition more readily apparent.</p>
	 * @param cause The throwable cause of the unexpected condition, or <code>null</code> if a throwable cause is nonexistent or unknown.
	 * @return A throwable indicating an unexpected condition.
	 */
	public static IllegalStateException unexpected(final Throwable cause) {
		return unexpected(null, cause);
	}

	/**
	 * Creates a throwable indicating an unexpected condition, which can be thrown by the caller.
	 * <p>An <dfn>unexpected</dfn> condition is one that is logically possible to occur, but yet should not occur because of an API contract, for instance. For
	 * example, all JVMs should support UTF-8, so one would not expect an {@link UnsupportedEncodingException} if UTF-8 is explicitly specified.</p>
	 * <p>This is a convenience method that makes the semantics of the condition more readily apparent.</p>
	 * @param message The detail message, or <code>null</code> if there is no detail message.
	 * @param cause The throwable cause of the unexpected condition, or <code>null</code> if a throwable cause is nonexistent or unknown.
	 * @return A throwable indicating an unexpected condition.
	 */
	public static IllegalStateException unexpected(final String message, final Throwable cause) {
		return new IllegalStateException(message, cause);
	}

	/**
	 * Creates a throwable indicating a logically impossible condition, which can be thrown by the caller.
	 * <p>An <dfn>impossible</dfn> condition is one that is supposedly logically impossible to occur. For example, a default section of a switch statement
	 * covering all possible values of an enum should never be executed; the throwable produced by this method provides a convenient outcome to indicate this
	 * condition (for example, if a new enum value is later introduced and the switch statement isn't updated). The code following a method guaranteed to throw an
	 * exception is another example.</p>
	 * <p>This is a convenience method that makes the semantics of the condition more readily apparent.</p>
	 * @return A throwable indicating an impossible condition.
	 */
	public static AssertionError impossible() {
		return impossible((String)null);
	}

	/**
	 * Creates a throwable indicating a logically impossible condition, which can be thrown by the caller.
	 * <p>An <dfn>impossible</dfn> condition is one that is supposedly logically impossible to occur. For example, a default section of a switch statement
	 * covering all possible values of an enum should never be executed; the throwable produced by this method provides a convenient outcome to indicate this
	 * condition (for example, if a new enum value is later introduced and the switch statement isn't updated). The code following a method guaranteed to throw an
	 * exception is another example.</p>
	 * <p>This is a convenience method that makes the semantics of the condition more readily apparent.</p>
	 * @param message The detail message, or <code>null</code> if there is no detail message.
	 * @return A throwable indicating an impossible condition.
	 */
	public static AssertionError impossible(final String message) {
		return impossible(message, null);
	}

	/**
	 * Creates a throwable indicating a logically impossible condition, which can be thrown by the caller.
	 * <p>An <dfn>impossible</dfn> condition is one that is supposedly logically impossible to occur. For example, a default section of a switch statement
	 * covering all possible values of an enum should never be executed; the throwable produced by this method provides a convenient outcome to indicate this
	 * condition (for example, if a new enum value is later introduced and the switch statement isn't updated). The code following a method guaranteed to throw an
	 * exception is another example.</p>
	 * <p>This is a convenience method that makes the semantics of the condition more readily apparent.</p>
	 * @param cause The throwable cause of the impossible condition, or <code>null</code> if a throwable cause is nonexistent or unknown.
	 * @return A throwable indicating an impossible condition.
	 */
	public static AssertionError impossible(final Throwable cause) {
		return impossible(null, cause);
	}

	/**
	 * Creates a throwable indicating a logically impossible condition, which can be thrown by the caller.
	 * <p>An <dfn>impossible</dfn> condition is one that is supposedly logically impossible to occur. For example, a default section of a switch statement
	 * covering all possible values of an enum should never be executed; the throwable produced by this method provides a convenient outcome to indicate this
	 * condition (for example, if a new enum value is later introduced and the switch statement isn't updated). The code following a method guaranteed to throw an
	 * exception is another example.</p>
	 * <p>This is a convenience method that makes the semantics of the condition more readily apparent.</p>
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
