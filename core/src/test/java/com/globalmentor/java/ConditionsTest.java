/*
 * Copyright © 2016 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static com.globalmentor.java.Conditions.*;
import static java.util.function.Predicate.*;

import com.globalmentor.model.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.*;

import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.junit.jupiter.api.*;

/**
 * Tests for {@link Conditions}.
 * 
 * @author Magno Nascimento
 * @author Garret Wilson
 */
public class ConditionsTest {

	/**
	 * @see Conditions#checkArgument(boolean)
	 * @see Conditions#checkArgument(boolean, String, Object...)
	 */
	@Test
	void testCheckArgument() {
		checkArgument(true);
		checkArgument(true, "error message");
		checkArgument(true, "error message", 123);
		checkArgument(true, "error message %d", 123);
		assertThrows(IllegalArgumentException.class, () -> checkArgument(false), "false statement");
	}

	/**
	 * @see Conditions#checkArgument(Predicate)
	 * @see Conditions#checkArgument(Predicate, String, Object...)
	 */
	@Test
	void testCheckArgumentPredicate() {
		Optional.of("foo").ifPresent(checkArgument(not(String::isEmpty)));
		Optional.of("foo").ifPresent(checkArgument(not(String::isEmpty), "error message"));
		Optional.of("foo").ifPresent(checkArgument(not(String::isEmpty), "error message", 123));
		Optional.of("foo").ifPresent(checkArgument(not(String::isEmpty), "error message %d", 123));
		final Predicate<CharSequence> charSequenceNotEmpty = not(CharSequence::isEmpty); //test contravariance
		Optional.of("foo").ifPresent(checkArgument(charSequenceNotEmpty));
		assertThrows(IllegalArgumentException.class, () -> Optional.of("foo").ifPresent(checkArgument(String::isEmpty)), "false predicate");
	}

	/**
	 * Tests the {@link Conditions#checkArgument(boolean)} and {@link Conditions#checkArgument(boolean, String, Object...)} methods with a false statement and how
	 * the messages are being formatted.
	 */
	@Test
	void testCheckArgumentErrorMessage() {
		assertThat(assertThrows(IllegalArgumentException.class, () -> checkArgument(false)).getMessage(), equalTo(null));
		assertThat(assertThrows(IllegalArgumentException.class, () -> checkArgument(false, "error message")).getMessage(), equalTo("error message"));
		assertThat(assertThrows(IllegalArgumentException.class, () -> checkArgument(false, "error message %d", 123)).getMessage(), equalTo("error message 123"));
		assertThat("The arguments of the error message should be ignored.",
				assertThrows(IllegalArgumentException.class, () -> checkArgument(false, "error message", 123)).getMessage(), equalTo("error message"));
	}

	/**
	 * Tests the {@link Conditions#checkArgumentIsInstance(Object, Class)} and the {@link Conditions#checkArgumentIsInstance(Object, Class, String, Object...)}
	 * methods.
	 */
	@Test
	void testCheckArgumentIsInstance() {
		final Long testNumber = Long.valueOf(123456789);
		for(final Class<?> instanceType : (Iterable<Class<?>>)Stream.of(Long.class, Number.class, Object.class)::iterator) {
			assertThat(checkArgumentIsInstance(testNumber, instanceType), sameInstance(testNumber));
			assertThat(checkArgumentIsInstance(testNumber, instanceType, "error message"), sameInstance(testNumber));
			assertThat(checkArgumentIsInstance(testNumber, instanceType, "error message", 123), sameInstance(testNumber));
			assertThat(checkArgumentIsInstance(testNumber, instanceType, "error message %s", 123), sameInstance(testNumber));
		}
	}

	/** Tests the {@link Conditions#checkArgumentIsInstance(Object, Class)} method with <code>null</code>. */
	@Test
	void testCheckArgumentIsInstanceWithANullObject() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentIsInstance(null, Boolean.class));
	}

	/**
	 * Tests the {@link Conditions#checkArgumentIsInstance(Object, Class)} and {@link Conditions#checkArgumentIsInstance(Object, Class, String, Object...)}
	 * methods with <code>null</code> and how the messages are being formatted.
	 */
	@Test
	void testCheckArgumentIsInstanceErrorMessage() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentIsInstance(null, Boolean.class));
		assertThat(assertThrows(IllegalArgumentException.class, () -> checkArgumentIsInstance(null, Boolean.class, "error message")).getMessage(),
				equalTo("error message"));
		assertThat(assertThrows(IllegalArgumentException.class, () -> checkArgumentIsInstance(null, Boolean.class, "error message %d", 123)).getMessage(),
				equalTo("error message 123"));
		assertThat("The arguments of the error message should be ignored.",
				assertThrows(IllegalArgumentException.class, () -> checkArgumentIsInstance(null, Boolean.class, "error message", 123)).getMessage(),
				equalTo("error message"));
	}

	/** Tests the {@link Conditions#checkArgumentNotNull(Object)} and the {@link Conditions#checkArgumentNotNull(Object, String, Object...)} methods. */
	@Test
	void testCheckArgumentNotNull() {
		final Object object = new Object();
		assertThat(checkArgumentNotNull(object), sameInstance(object));
		assertThat(checkArgumentNotNull(object, "error message"), sameInstance(object));
		assertThat(checkArgumentNotNull(object, "error message", 123), sameInstance(object));
		assertThat(checkArgumentNotNull(object, "error message %s", 123), sameInstance(object));
	}

	/** Tests the {@link Conditions#checkArgumentNotNull(Object)} method with <code>null</code>. */
	@Test
	void testCheckArgumentNotNullWithANullObject() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentNotNull(null));
	}

	/**
	 * Tests the {@link Conditions#checkArgumentNotNull(Object)} and {@link Conditions#checkArgumentNotNull(Object, String, Object...)} methods with
	 * <code>null</code> and how the messages are being formatted.
	 */
	@Test
	void testCheckArgumentNotNullErrorMessage() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentNotNull(null));
		assertThat(assertThrows(IllegalArgumentException.class, () -> checkArgumentNotNull(null, "error message")).getMessage(), equalTo("error message"));
		assertThat(assertThrows(IllegalArgumentException.class, () -> checkArgumentNotNull(null, "error message %d", 123)).getMessage(),
				equalTo("error message 123"));
		assertThat("The arguments of the error message should be ignored.",
				assertThrows(IllegalArgumentException.class, () -> checkArgumentNotNull(null, "error message", 123)).getMessage(), equalTo("error message"));
	}

	/** Tests the {@link Conditions#checkArgumentNotEmpty(String)} and the {@link Conditions#checkArgumentNotEmpty(String, String, Object...)} methods. */
	@Test
	void testCheckArgumentNotEmpty() {
		final String string = "test";
		assertThat(checkArgumentNotEmpty(string), sameInstance(string));
		assertThat(checkArgumentNotEmpty(string, "error message"), sameInstance(string));
		assertThat(checkArgumentNotEmpty(string, "error message", 123), sameInstance(string));
		assertThat(checkArgumentNotEmpty(string, "error message %s", 123), sameInstance(string));
	}

	/** Tests the {@link Conditions#checkArgumentNotEmpty(String)} method with an empty string. */
	@Test
	void testCheckArgumentNotEmptyWithAnEmptyString() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentNotEmpty(""));
	}

	/**
	 * Tests the {@link Conditions#checkArgumentNotEmpty(String)} and {@link Conditions#checkArgumentNotEmpty(String, String, Object...)} methods with an empty
	 * string and how the messages are being formatted.
	 */
	@Test
	void testCheckArgumentNotEmptyErrorMessage() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentNotEmpty(""));
		assertThat(assertThrows(IllegalArgumentException.class, () -> checkArgumentNotEmpty("", "error message")).getMessage(), equalTo("error message"));
		assertThat(assertThrows(IllegalArgumentException.class, () -> checkArgumentNotEmpty("", "error message %d", 123)).getMessage(),
				equalTo("error message 123"));
		assertThat("The arguments of the error message should be ignored.",
				assertThrows(IllegalArgumentException.class, () -> checkArgumentNotEmpty("", "error message", 123)).getMessage(), equalTo("error message"));
	}

	/** Tests the {@link Conditions#checkArgumentNotBlank(String)} and the {@link Conditions#checkArgumentNotBlank(String, String, Object...)} methods. */
	@Test
	void testCheckArgumentNotBlank() {
		final String string = "test";
		assertThat(checkArgumentNotBlank(string), sameInstance(string));
		assertThat(checkArgumentNotBlank(string, "error message"), sameInstance(string));
		assertThat(checkArgumentNotBlank(string, "error message", 123), sameInstance(string));
		assertThat(checkArgumentNotBlank(string, "error message %s", 123), sameInstance(string));
	}

	/** Tests the {@link Conditions#checkArgumentNotBlank(String)} method with a blank string. */
	@Test
	void testCheckArgumentNotBlankWithABlankString() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentNotBlank(""));
		assertThrows(IllegalArgumentException.class, () -> checkArgumentNotBlank(" "));
		assertThrows(IllegalArgumentException.class, () -> checkArgumentNotBlank("  "));
		assertThrows(IllegalArgumentException.class, () -> checkArgumentNotBlank("\t"));
		assertThrows(IllegalArgumentException.class, () -> checkArgumentNotBlank("\n"));
		assertThrows(IllegalArgumentException.class, () -> checkArgumentNotBlank(" \t\n "));
	}

	/**
	 * Tests the {@link Conditions#checkArgumentNotBlank(String)} and {@link Conditions#checkArgumentNotBlank(String, String, Object...)} methods with a blank
	 * string and how the messages are being formatted.
	 */
	@Test
	void testCheckArgumentNotBlankErrorMessage() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentNotBlank(""));
		assertThat(assertThrows(IllegalArgumentException.class, () -> checkArgumentNotBlank("", "error message")).getMessage(), equalTo("error message"));
		assertThat(assertThrows(IllegalArgumentException.class, () -> checkArgumentNotBlank("", "error message %d", 123)).getMessage(),
				equalTo("error message 123"));
		assertThat("The arguments of the error message should be ignored.",
				assertThrows(IllegalArgumentException.class, () -> checkArgumentNotBlank("", "error message", 123)).getMessage(), equalTo("error message"));
	}

	/** Tests the {@link Conditions#checkArgumentPresent(Optional)} and the {@link Conditions#checkArgumentPresent(Optional, String, Object...)} methods. */
	@Test
	void testCheckArgumentPresent() {
		final Object object = new Object();
		assertThat(checkArgumentPresent(Optional.of(object)), sameInstance(object));
		assertThat(checkArgumentPresent(Optional.of(object), "error message"), sameInstance(object));
		assertThat(checkArgumentPresent(Optional.of(object), "error message", 123), sameInstance(object));
		assertThat(checkArgumentPresent(Optional.of(object), "error message %s", 123), sameInstance(object));
	}

	/** Tests the {@link Conditions#checkArgumentPresent(Optional)} method with a <code>null</code> object. */
	@Test
	void testCheckArgumentPresentWithANullObject() {
		assertThrows(NullPointerException.class, () -> checkArgumentPresent(null));
	}

	/** Tests the {@link Conditions#checkArgumentPresent(Optional)} method with an empty optional. */
	@Test
	void testCheckArgumentPresentWithAnEmptyObject() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentPresent(Optional.empty()));
	}

	/**
	 * Tests the {@link Conditions#checkArgumentPresent(Optional)} and {@link Conditions#checkArgumentPresent(Optional, String, Object...)} methods with an empty
	 * optional and how the messages are being formatted.
	 */
	@Test
	void testCheckArgumentPresentErrorMessage() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentPresent(Optional.empty()));
		assertThat(assertThrows(IllegalArgumentException.class, () -> checkArgumentPresent(Optional.empty(), "error message")).getMessage(),
				equalTo("error message"));
		assertThat(assertThrows(IllegalArgumentException.class, () -> checkArgumentPresent(Optional.empty(), "error message %d", 123)).getMessage(),
				equalTo("error message 123"));
		assertThat("The arguments of the error message should be ignored.",
				assertThrows(IllegalArgumentException.class, () -> checkArgumentPresent(Optional.empty(), "error message", 123)).getMessage(),
				equalTo("error message"));
	}

	/** Tests the {@link Conditions#checkArgumentMinimum(int, int)} and the {@link Conditions#checkArgumentMinimum(long, long)} methods. */
	@Test
	void testCheckArgumentMinimum() {
		checkArgumentMinimum(10, 0);
		checkArgumentMinimum(10L, 0L);
	}

	/** Tests the {@link Conditions#checkArgumentMinimum(int, int)} method with a false statement. */
	@Test
	void testCheckArgumentMinimumWithAMinorIntValue() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentMinimum(0, 10));
	}

	/** Tests the {@link Conditions#checkArgumentMinimum(long, long)} method with a false statement. */
	@Test
	void testCheckArgumentMinimumWithAMinorLongValue() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentMinimum(0L, 10L));
	}

	/**
	 * Tests the {@link Conditions#checkArgumentNotNegative(int)}, {@link Conditions#checkArgumentNotNegative(long)},
	 * {@link Conditions#checkArgumentPositive(int)} and the {@link Conditions#checkArgumentPositive(long)} methods.
	 */
	@Test
	void testCheckArgumentNotNegativeAndPositive() {
		checkArgumentNotNegative(10);
		checkArgumentNotNegative(10L);

		checkArgumentPositive(10);
		checkArgumentPositive(10L);
	}

	/** Tests the {@link Conditions#checkArgumentNotNegative(int)} method with a false statement. */
	@Test
	void testCheckArgumentNotNegativeWithANegativeIntValue() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentNotNegative(-10));
	}

	/** Tests the {@link Conditions#checkArgumentNotNegative(long)} method with a false statement. */
	@Test
	void testCheckArgumentNotNegativeWithANegativeLongValue() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentNotNegative(-10L));
	}

	/** Tests the {@link Conditions#checkArgumentPositive(int)} method with a false statement. */
	@Test
	void testCheckArgumentPositiveWithANegativeIntValue() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentPositive(-10));
	}

	/** Tests the {@link Conditions#checkArgumentPositive(long)} method with a false statement. */
	@Test
	void testCheckArgumentPositiveWithANegativeLongValue() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentPositive(-10L));
	}

	/** Tests the method {@link Conditions#checkArgumentRange(int, int, int)}. */
	@Test
	void testCheckArgumentRangeWithThreeIntegers() {

		//test for the edges
		checkArgumentRange(0, 0, 10);
		checkArgumentRange(10, 0, 10);

		//test for the signals
		checkArgumentRange(5, 0, 10); //test for the middle
		checkArgumentRange(-5, -10, 0);
		checkArgumentRange(0, -5, 5);

	}

	/** Tests the method {@link Conditions#checkArgumentRange(long, long, long)}. */
	@Test
	void testCheckArgumentRangeWithThreeLongs() {

		//test for the edges
		checkArgumentRange(0L, 0L, 10L);
		checkArgumentRange(10L, 0L, 10L);

		//test for the signals
		checkArgumentRange(5L, 0L, 10L); //test for the middle
		checkArgumentRange(-5L, -10L, 0L);
		checkArgumentRange(0L, -5L, 5L);

	}

	/** Tests the method {@link Conditions#checkArgumentRange(int, int, int, int)}. */
	@Test
	void testCheckArgumentRangeWithFourIntegers() {

		//test for the edges
		checkArgumentRange(0, 5, 0, 10);
		checkArgumentRange(5, 10, 0, 10);
		checkArgumentRange(0, 10, 0, 10);

		//test for the signals
		checkArgumentRange(1, 9, 0, 10); //test for the middle
		checkArgumentRange(-9, -1, -10, 0);
		checkArgumentRange(-4, 4, -5, 5);

	}

	/** Tests the {@link Conditions#checkArgumentRange(int, int, int)} method with a false statement. */
	@Test
	void testCheckArgumentRangeWithAMinorIntValue() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentRange(-1, 0, 10));
	}

	/** Tests the {@link Conditions#checkArgumentRange(int, int, int)} method with a false statement. */
	@Test
	void testCheckArgumentRangeWithABiggerIntValue() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentRange(11, 0, 10));
	}

	/** Tests the {@link Conditions#checkArgumentRange(long, long, long)} method with a false statement. */
	@Test
	void testCheckArgumentRangeWithAMinorLongValue() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentRange(-1L, 0L, 10L));
	}

	/** Tests the {@link Conditions#checkArgumentRange(long, long, long)} method with a false statement. */
	@Test
	void testCheckArgumentRangeWithABiggerLongValue() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentRange(11L, 0L, 10L));
	}

	/** Tests the {@link Conditions#checkArgumentRange(int, int, int, int)} method with a false statement. */
	@Test
	void testCheckArgumentRangeWithAMoreNegativeInterval() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentRange(-10, -1, 0, 10));
	}

	/** Tests the {@link Conditions#checkArgumentRange(int, int, int, int)} method with a false statement. */
	@Test
	void testCheckArgumentRangeWithAMorePositiveInterval() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentRange(11, 20, 0, 10));
	}

	/** Tests the {@link Conditions#checkArgumentRange(int, int, int, int)} method with a false statement. */
	@Test
	void testCheckArgumentRangeWithABiggerInterval() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentRange(-1, 11, 0, 10));
	}

	/**
	 * @see Conditions#checkConfiguredState(boolean)
	 * @see Conditions#checkConfiguredState(boolean, String, Object...)
	 */
	@Test
	void testCheckConfiguredState() {
		checkConfiguredState(true);
		checkConfiguredState(true, "error message");
		checkConfiguredState(true, "error message", 123);
		checkConfiguredState(true, "error message %d", 123);
		assertThrows(ConfiguredStateException.class, () -> checkConfiguredState(false), "false statement");
	}

	/**
	 * @see Conditions#checkConfiguredState(Predicate)
	 * @see Conditions#checkConfiguredState(Predicate, String, Object...)
	 */
	@Test
	void testCheckConfiguredStatePredicate() {
		Optional.of("foo").ifPresent(checkConfiguredState(not(String::isEmpty)));
		Optional.of("foo").ifPresent(checkConfiguredState(not(String::isEmpty), "error message"));
		Optional.of("foo").ifPresent(checkConfiguredState(not(String::isEmpty), "error message", 123));
		Optional.of("foo").ifPresent(checkConfiguredState(not(String::isEmpty), "error message %d", 123));
		final Predicate<CharSequence> charSequenceNotEmpty = not(CharSequence::isEmpty); //test contravariance
		Optional.of("foo").ifPresent(checkConfiguredState(charSequenceNotEmpty));
		assertThrows(ConfiguredStateException.class, () -> Optional.of("foo").ifPresent(checkConfiguredState(String::isEmpty)), "false predicate");
	}

	/**
	 * Tests the {@link Conditions#checkConfiguredState(boolean)} and {@link Conditions#checkConfiguredState(boolean, String, Object...)} methods with a false
	 * statement and how the messages are being formatted.
	 */
	@Test
	void testCheckConfigedStateErrorMessage() {
		assertThrows(ConfiguredStateException.class, () -> checkConfiguredState(false));
		assertThat(assertThrows(ConfiguredStateException.class, () -> checkConfiguredState(false, "error message")).getMessage(), equalTo("error message"));
		assertThat(assertThrows(ConfiguredStateException.class, () -> checkConfiguredState(false, "error message %d", 123)).getMessage(),
				equalTo("error message 123"));
		assertThat("The arguments of the error message should be ignored.",
				assertThrows(ConfiguredStateException.class, () -> checkConfiguredState(false, "error message", 123)).getMessage(), equalTo("error message"));
	}

	/** Tests the {@link Conditions#checkConfiguredStateNotNull(Object)} and the {@link Conditions#checkConfiguredStateNotNull(Object, String)} methods. */
	@Test
	void testCheckConfiguredStateNotNull() {
		checkConfiguredStateNotNull(new Object());
		checkConfiguredStateNotNull(new Object(), "error message");
	}

	/** Tests the {@link Conditions#checkConfiguredStateNotNull(Object)} method with a false statement. */
	@Test
	void testCheckConfiguredStateNotNullWithANullObject() {
		assertThrows(ConfiguredStateException.class, () -> checkConfiguredStateNotNull(null));
	}

	/**
	 * Tests the {@link Conditions#checkConfiguredStateNotNull(Object)} and {@link Conditions#checkConfiguredStateNotNull(Object, String)} methods with a false
	 * statement and how the messages are being formatted.
	 */
	@Test
	void testCheckConfiguredStateNotNullErrorMessage() {
		assertThrows(ConfiguredStateException.class, () -> checkConfiguredStateNotNull(null));
		assertThat(assertThrows(ConfiguredStateException.class, () -> checkConfiguredStateNotNull(null, "error message")).getMessage(), equalTo("error message"));
	}

	/** Tests the {@link Conditions#checkIndexBounds(int, int)} and {@link Conditions#checkIndexBounds(int, int, int)} methods. */
	@Test
	void testCheckIndexBoundsWithIntegers() {

		//test for the edges
		checkIndexBounds(0, 10);
		checkIndexBounds(9, 10);
		checkIndexBounds(0, 0, 10);
		checkIndexBounds(9, 0, 10);

		checkIndexBounds(4, 10); //test for the middle

		//test for the signals
		checkIndexBounds(5, 0, 10);
		checkIndexBounds(-5, -10, 0);
		checkIndexBounds(0, -5, 5);

	}

	/** Tests the {@link Conditions#checkIndexBounds(long, long)} and {@link Conditions#checkIndexBounds(long, long, long)} methods. */
	@Test
	void testCheckIndexBoundsWithLongs() {

		//test for the edges
		checkIndexBounds(0L, 10L);
		checkIndexBounds(9L, 10L);
		checkIndexBounds(0L, 0L, 10L);
		checkIndexBounds(9L, 0L, 10L);

		checkIndexBounds(4L, 10L); //test for the middle

		//test for the signals
		checkIndexBounds(5L, 0L, 10L);
		checkIndexBounds(-5L, -10L, 0L);
		checkIndexBounds(0L, -5L, 5L);

	}

	/** Tests the {@link Conditions#checkIndexBounds(int, int)} method with a false statement. */
	@Test
	void testCheckIndexBoundsWithAMinorIntValue() {
		assertThrows(IndexOutOfBoundsException.class, () -> checkIndexBounds(-1, 10));
	}

	/** Tests the {@link Conditions#checkIndexBounds(int, int)} method with a false statement. */
	@Test
	void testCheckIndexBoundsWithABiggerIntValue() {
		assertThrows(IndexOutOfBoundsException.class, () -> checkIndexBounds(10, 10));
	}

	/** Tests the {@link Conditions#checkIndexBounds(int, int, int)} method with a false statement. */
	@Test
	void testCheckIndexBoundsWithAMinorIntValueUsingARange() {
		assertThrows(IndexOutOfBoundsException.class, () -> checkIndexBounds(-1, 0, 10));
	}

	/** Tests the {@link Conditions#checkIndexBounds(int, int, int)} method with a false statement. */
	@Test
	void testCheckIndexBoundsWithABiggerIntValueUsingARange() {
		assertThrows(IndexOutOfBoundsException.class, () -> checkIndexBounds(10, 0, 10));
	}

	/** Tests the {@link Conditions#checkIndexBounds(long, long)} method with a false statement. */
	@Test
	void testCheckIndexBoundsWithAMinorLongValue() {
		assertThrows(IndexOutOfBoundsException.class, () -> checkIndexBounds(-1L, 10L));
	}

	/** Tests the {@link Conditions#checkIndexBounds(long, long, long)} method with a false statement. */
	@Test
	void testCheckIndexBoundsWithAMinorLongValueUsingARange() {
		assertThrows(IndexOutOfBoundsException.class, () -> checkIndexBounds(-1L, 0L, 10L));
	}

	/** Tests the {@link Conditions#checkIndexBounds(long, long, long)} method with a false statement. */
	@Test
	void testCheckIndexBoundsWithABiggerLongValueUsingARange() {
		assertThrows(IndexOutOfBoundsException.class, () -> checkIndexBounds(10L, 0L, 10L));
	}

	/** Tests the {@link Conditions#checkIndexBounds(long, long)} method with a false statement. */
	@Test
	void testCheckIndexBoundsWithABiggerLongValue() {
		assertThrows(IndexOutOfBoundsException.class, () -> checkIndexBounds(10L, 10L));
	}

	/** Tests the {@link Conditions#checkState(boolean)} and the {@link Conditions#checkState(boolean, String, Object...)} methods. */
	@Test
	void testCheckState() {
		checkState(true);
		checkState(true, "error message");
		checkState(true, "error message", 123);
		checkState(true, "error message %d", 123);
	}

	/** Tests the {@link Conditions#checkState(boolean)} method with a false statement. */
	@Test
	void testCheckStateWithAFalseStatement() {
		assertThrows(IllegalStateException.class, () -> checkState(false));
	}

	/**
	 * Tests the {@link Conditions#checkState(boolean)} and {@link Conditions#checkState(boolean, String, Object...)} methods with a false statement and how the
	 * messages are being formatted.
	 */
	@Test
	void testCheckStateErrorMessage() {
		assertThat(assertThrows(IllegalStateException.class, () -> checkState(false)).getMessage(), equalTo(null));
		assertThat(assertThrows(IllegalStateException.class, () -> checkState(false, "error message")).getMessage(), equalTo("error message"));
		assertThat(assertThrows(IllegalStateException.class, () -> checkState(false, "error message %d", 123)).getMessage(), equalTo("error message 123"));
		assertThat("The arguments of the error message should be ignored.",
				assertThrows(IllegalStateException.class, () -> checkState(false, "error message", 123)).getMessage(), equalTo("error message"));
	}

	/**
	 * @see Conditions#checkSupportedOperation(boolean)
	 * @see Conditions#checkSupportedOperation(boolean, String, Object...)
	 */
	@Test
	void testCheckSupportedOperation() {
		checkSupportedOperation(true);
		checkSupportedOperation(true, "error message");
		checkSupportedOperation(true, "error message", 123);
		checkSupportedOperation(true, "error message %d", 123);
		assertThrows(UnsupportedOperationException.class, () -> checkSupportedOperation(false), "false statement");
	}

	/**
	 * @see Conditions#checkSupportedOperation(Predicate)
	 * @see Conditions#checkSupportedOperation(Predicate, String, Object...)
	 */
	@Test
	void testCheckSupportedOperationPredicate() {
		Optional.of("foo").ifPresent(checkSupportedOperation(not(String::isEmpty)));
		Optional.of("foo").ifPresent(checkSupportedOperation(not(String::isEmpty), "error message"));
		Optional.of("foo").ifPresent(checkSupportedOperation(not(String::isEmpty), "error message", 123));
		Optional.of("foo").ifPresent(checkSupportedOperation(not(String::isEmpty), "error message %d", 123));
		final Predicate<CharSequence> charSequenceNotEmpty = not(CharSequence::isEmpty); //test contravariance
		Optional.of("foo").ifPresent(checkSupportedOperation(charSequenceNotEmpty));
		assertThrows(UnsupportedOperationException.class, () -> Optional.of("foo").ifPresent(checkSupportedOperation(String::isEmpty)), "false predicate");
	}

	/**
	 * Tests the {@link Conditions#unexpected(String)} method.
	 */
	@Test
	void testUnexpectedWithAString() {

		assertThat(unexpected("test message"), instanceOf(IllegalStateException.class));
		assertThat(unexpected("test message").getMessage(), equalTo("test message"));
		assertThat(unexpected("test message").getCause(), equalTo(null));

	}

	/**
	 * Tests the {@link Conditions#unexpected(Throwable)} method.
	 */
	@Test
	void testUnexpectedWithAThrowable() {

		final Throwable throwableCause = new IllegalArgumentException();

		assertThat(unexpected(throwableCause), instanceOf(IllegalStateException.class));
		assertThat(unexpected(throwableCause).getMessage(), equalTo(null));
		assertThat(unexpected(throwableCause).getCause(), equalTo(throwableCause));

	}

	/**
	 * Tests the {@link Conditions#unexpected(String, Throwable)} method.
	 */
	@Test
	void testUnexpectedWithAStringAndThrowable() {

		final Throwable throwableCause = new IllegalArgumentException();

		assertThat(unexpected("test message", throwableCause), instanceOf(IllegalStateException.class));
		assertThat(unexpected("test message", throwableCause).getMessage(), equalTo("test message"));
		assertThat(unexpected("test message", throwableCause).getCause(), equalTo(throwableCause));

	}

	/**
	 * Tests the {@link Conditions#impossible()} method.
	 */
	@Test
	void testImpossible() {

		assertThat(impossible(), instanceOf(AssertionError.class));
		assertThat(impossible().getMessage(), equalTo(null));
		assertThat(impossible().getCause(), equalTo(null));

	}

	/**
	 * Tests the {@link Conditions#impossible(String)} method.
	 */
	@Test
	void testImpossibleWithAString() {

		assertThat(impossible("test message"), instanceOf(AssertionError.class));
		assertThat(impossible("test message").getMessage(), equalTo("test message"));
		assertThat(impossible("test message").getCause(), equalTo(null));

	}

	/**
	 * Tests the {@link Conditions#impossible(Throwable)} method.
	 */
	@Test
	void testImpossibleWithAThrowable() {

		final Throwable throwableCause = new IllegalArgumentException();

		assertThat(impossible(throwableCause), instanceOf(AssertionError.class));
		assertThat(impossible(throwableCause).getMessage(), equalTo(null));
		assertThat(impossible(throwableCause).getCause(), equalTo(throwableCause));

	}

	/**
	 * Tests the {@link Conditions#impossible(String, Throwable)} method.
	 */
	@Test
	void testImpossibleWithAStringAndAThrowable() {

		final Throwable throwableCause = new IllegalArgumentException();

		assertThat(impossible("test message", throwableCause), instanceOf(AssertionError.class));
		assertThat(impossible("test message", throwableCause).getMessage(), equalTo("test message"));
		assertThat(impossible("test message", throwableCause).getCause(), equalTo(throwableCause));

	}

}
