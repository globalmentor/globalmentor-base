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

import com.globalmentor.model.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.*;

import java.util.Optional;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

/**
 * Tests for {@link Conditions}.
 * 
 * @author Magno Nascimento
 * 
 */
public class ConditionsTest {

	/** Tests the {@link Conditions#checkArgument(boolean)} and the {@link Conditions#checkArgument(boolean, String, Object...)} methods. */
	@Test
	public void testCheckArgument() {
		checkArgument(true);
		checkArgument(true, "error message");
		checkArgument(true, "error message", 123);
		checkArgument(true, "error message %d", 123);
	}

	/** Tests the {@link Conditions#checkArgument(boolean)} method with a false statement. */
	@Test
	public void testCheckArgumentWithAFalseStatement() {
		assertThrows(IllegalArgumentException.class, () -> checkArgument(false));
	}

	/**
	 * Tests the {@link Conditions#checkArgument(boolean)} and {@link Conditions#checkArgument(boolean, String, Object...)} methods with a false statement and how
	 * the messages are being formatted.
	 */
	@Test
	public void testCheckArgumentErrorMessage() {

		try {
			checkArgument(false);
			fail("The statement above should have thrown an IllegalArgumentExcepton");
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo(null));
		}

		try {
			checkArgument(false, "error message");
			fail("The statement above should have thrown an IllegalArgumentExcepton");
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo("error message"));
		}

		try {
			checkArgument(false, "error message %d", 123);
			fail("The statement above should have thrown an IllegalArgumentExcepton");
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo("error message 123"));
		}

		try {
			checkArgument(false, "error message", 123); // The arguments of the error message should be ignored.
			fail("The statement above should have thrown an IllegalArgumentExcepton");
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo("error message"));
		}
	}

	/**
	 * Tests the {@link Conditions#checkArgumentIsInstance(Object, Class)} and the {@link Conditions#checkArgumentIsInstance(Object, Class, String, Object...)}
	 * methods.
	 */
	@Test
	public void testCheckArgumentIsInstance() {
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
	public void testCheckArgumentIsInstanceWithANullObject() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentIsInstance(null, Boolean.class));
	}

	/**
	 * Tests the {@link Conditions#checkArgumentIsInstance(Object, Class)} and {@link Conditions#checkArgumentIsInstance(Object, Class, String, Object...)}
	 * methods with <code>null</code> and how the messages are being formatted.
	 */
	@Test
	public void testCheckArgumentIsInstanceErrorMessage() {

		try {
			checkArgumentIsInstance(null, Boolean.class);
			fail("The statement above should have thrown an IllegalArgumentExcepton");
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo(null));
		}

		try {
			checkArgumentIsInstance(null, Boolean.class, "error message");
			fail("The statement above should have thrown an IllegalArgumentExcepton");
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo("error message"));
		}

		try {
			checkArgumentIsInstance(null, Boolean.class, "error message %d", 123);
			fail("The statement above should have thrown an IllegalArgumentExcepton");
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo("error message 123"));
		}

		try {
			checkArgumentIsInstance(null, Boolean.class, "error message", 123); // The arguments of the error message should be ignored.
			fail("The statement above should have thrown an IllegalArgumentExcepton");
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo("error message"));
		}
	}

	/** Tests the {@link Conditions#checkArgumentNotNull(Object)} and the {@link Conditions#checkArgumentNotNull(Object, String, Object...)} methods. */
	@Test
	public void testCheckArgumentNotNull() {
		final Object object = new Object();
		assertThat(checkArgumentNotNull(object), sameInstance(object));
		assertThat(checkArgumentNotNull(object, "error message"), sameInstance(object));
		assertThat(checkArgumentNotNull(object, "error message", 123), sameInstance(object));
		assertThat(checkArgumentNotNull(object, "error message %s", 123), sameInstance(object));
	}

	/** Tests the {@link Conditions#checkArgumentNotNull(Object)} method with <code>null</code>. */
	@Test
	public void testCheckArgumentNotNullWithANullObject() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentNotNull(null));
	}

	/**
	 * Tests the {@link Conditions#checkArgumentNotNull(Object)} and {@link Conditions#checkArgumentNotNull(Object, String, Object...)} methods with
	 * <code>null</code> and how the messages are being formatted.
	 */
	@Test
	public void testCheckArgumentNotNullErrorMessage() {

		try {
			checkArgumentNotNull(null);
			fail("The statement above should have thrown an IllegalArgumentExcepton");
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo(null));
		}

		try {
			checkArgumentNotNull(null, "error message");
			fail("The statement above should have thrown an IllegalArgumentExcepton");
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo("error message"));
		}

		try {
			checkArgumentNotNull(null, "error message %d", 123);
			fail("The statement above should have thrown an IllegalArgumentExcepton");
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo("error message 123"));
		}

		try {
			checkArgumentNotNull(null, "error message", 123); // The arguments of the error message should be ignored.
			fail("The statement above should have thrown an IllegalArgumentExcepton");
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo("error message"));
		}
	}

	/** Tests the {@link Conditions#checkArgumentPresent(Optional)} and the {@link Conditions#checkArgumentPresent(Optional, String, Object...)} methods. */
	@Test
	public void testCheckArgumentPresent() {
		final Object object = new Object();
		assertThat(checkArgumentPresent(Optional.of(object)), sameInstance(object));
		assertThat(checkArgumentPresent(Optional.of(object), "error message"), sameInstance(object));
		assertThat(checkArgumentPresent(Optional.of(object), "error message", 123), sameInstance(object));
		assertThat(checkArgumentPresent(Optional.of(object), "error message %s", 123), sameInstance(object));
	}

	/** Tests the {@link Conditions#checkArgumentPresent(Optional)} method with a <code>null</code> object. */
	@Test
	public void testCheckArgumentPresentWithANullObject() {
		assertThrows(NullPointerException.class, () -> checkArgumentPresent(null));
	}

	/** Tests the {@link Conditions#checkArgumentPresent(Optional)} method with an empty optional. */
	@Test
	public void testCheckArgumentPresentWithAnEmptyObject() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentPresent(Optional.empty()));
	}

	/**
	 * Tests the {@link Conditions#checkArgumentPresent(Optional)} and {@link Conditions#checkArgumentPresent(Optional, String, Object...)} methods with an empty
	 * optional and how the messages are being formatted.
	 */
	@Test
	public void testCheckArgumentPresentErrorMessage() {

		try {
			checkArgumentPresent(Optional.empty());
			fail("The statement above should have thrown an IllegalArgumentExcepton");
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo(null));
		}

		try {
			checkArgumentPresent(Optional.empty(), "error message");
			fail("The statement above should have thrown an IllegalArgumentExcepton");
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo("error message"));
		}

		try {
			checkArgumentPresent(Optional.empty(), "error message %d", 123);
			fail("The statement above should have thrown an IllegalArgumentExcepton");
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo("error message 123"));
		}

		try {
			checkArgumentPresent(Optional.empty(), "error message", 123); // The arguments of the error message should be ignored.
			fail("The statement above should have thrown an IllegalArgumentExcepton");
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo("error message"));
		}
	}

	/** Tests the {@link Conditions#checkArgumentMinimum(int, int)} and the {@link Conditions#checkArgumentMinimum(long, long)} methods. */
	@Test
	public void testCheckArgumentMinimum() {
		checkArgumentMinimum(10, 0);
		checkArgumentMinimum(10L, 0L);
	}

	/** Tests the {@link Conditions#checkArgumentMinimum(int, int)} method with a false statement. */
	@Test
	public void testCheckArgumentMinimumWithAMinorIntValue() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentMinimum(0, 10));
	}

	/** Tests the {@link Conditions#checkArgumentMinimum(long, long)} method with a false statement. */
	@Test
	public void testCheckArgumentMinimumWithAMinorLongValue() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentMinimum(0L, 10L));
	}

	/**
	 * Tests the {@link Conditions#checkArgumentNotNegative(int)}, {@link Conditions#checkArgumentNotNegative(long)},
	 * {@link Conditions#checkArgumentPositive(int)} and the {@link Conditions#checkArgumentPositive(long)} methods.
	 */
	@Test
	public void testCheckArgumentNotNegativeAndPositive() {
		checkArgumentNotNegative(10);
		checkArgumentNotNegative(10L);

		checkArgumentPositive(10);
		checkArgumentPositive(10L);
	}

	/** Tests the {@link Conditions#checkArgumentNotNegative(int)} method with a false statement. */
	@Test
	public void testCheckArgumentNotNegativeWithANegativeIntValue() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentNotNegative(-10));
	}

	/** Tests the {@link Conditions#checkArgumentNotNegative(long)} method with a false statement. */
	@Test
	public void testCheckArgumentNotNegativeWithANegativeLongValue() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentNotNegative(-10L));
	}

	/** Tests the {@link Conditions#checkArgumentPositive(int)} method with a false statement. */
	@Test
	public void testCheckArgumentPositiveWithANegativeIntValue() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentPositive(-10));
	}

	/** Tests the {@link Conditions#checkArgumentPositive(long)} method with a false statement. */
	@Test
	public void testCheckArgumentPositiveWithANegativeLongValue() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentPositive(-10L));
	}

	/** Tests the method {@link Conditions#checkArgumentRange(int, int, int)}. */
	@Test
	public void testCheckArgumentRangeWithThreeIntegers() {

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
	public void testCheckArgumentRangeWithThreeLongs() {

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
	public void testCheckArgumentRangeWithFourIntegers() {

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
	public void testCheckArgumentRangeWithAMinorIntValue() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentRange(-1, 0, 10));
	}

	/** Tests the {@link Conditions#checkArgumentRange(int, int, int)} method with a false statement. */
	@Test
	public void testCheckArgumentRangeWithABiggerIntValue() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentRange(11, 0, 10));
	}

	/** Tests the {@link Conditions#checkArgumentRange(long, long, long)} method with a false statement. */
	@Test
	public void testCheckArgumentRangeWithAMinorLongValue() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentRange(-1L, 0L, 10L));
	}

	/** Tests the {@link Conditions#checkArgumentRange(long, long, long)} method with a false statement. */
	@Test
	public void testCheckArgumentRangeWithABiggerLongValue() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentRange(11L, 0L, 10L));
	}

	/** Tests the {@link Conditions#checkArgumentRange(int, int, int, int)} method with a false statement. */
	@Test
	public void testCheckArgumentRangeWithAMoreNegativeInterval() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentRange(-10, -1, 0, 10));
	}

	/** Tests the {@link Conditions#checkArgumentRange(int, int, int, int)} method with a false statement. */
	@Test
	public void testCheckArgumentRangeWithAMorePositiveInterval() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentRange(11, 20, 0, 10));
	}

	/** Tests the {@link Conditions#checkArgumentRange(int, int, int, int)} method with a false statement. */
	@Test
	public void testCheckArgumentRangeWithABiggerInterval() {
		assertThrows(IllegalArgumentException.class, () -> checkArgumentRange(-1, 11, 0, 10));
	}

	/** Tests the {@link Conditions#checkConfiguredState(boolean)} and the {@link Conditions#checkConfiguredState(boolean, String, Object...)} methods. */
	@Test
	public void testCheckConfiguration() {
		checkConfiguredState(true);
		checkConfiguredState(true, "error message");
		checkConfiguredState(true, "error message", 123);
		checkConfiguredState(true, "error message %d", 123);
	}

	/** Tests the {@link Conditions#checkConfiguredState(boolean)} method with a false statement. */
	@Test
	public void testCheckConfigurationWithAFalseStatement() {
		assertThrows(ConfiguredStateException.class, () -> checkConfiguredState(false));
	}

	/**
	 * Tests the {@link Conditions#checkConfiguredState(boolean)} and {@link Conditions#checkConfiguredState(boolean, String, Object...)} methods with a false
	 * statement and how the messages are being formatted.
	 */
	@Test
	public void testCheckConfigurationErrorMessage() {

		try {
			checkConfiguredState(false);
			fail("The statement above should have thrown an ConfigurationException");
		} catch(final ConfiguredStateException configurationException) {
			assertThat(configurationException.getMessage(), equalTo(null));
		}

		try {
			checkConfiguredState(false, "error message");
			fail("The statement above should have thrown an ConfigurationException");
		} catch(final ConfiguredStateException configurationException) {
			assertThat(configurationException.getMessage(), equalTo("error message"));
		}

		try {
			checkConfiguredState(false, "error message %d", 123);
			fail("The statement above should have thrown an ConfigurationException");
		} catch(final ConfiguredStateException configurationException) {
			assertThat(configurationException.getMessage(), equalTo("error message 123"));
		}

		try {
			checkConfiguredState(false, "error message", 123); // The arguments of the error message should be ignored.
			fail("The statement above should have thrown an ConfigurationException");
		} catch(final ConfiguredStateException configurationException) {
			assertThat(configurationException.getMessage(), equalTo("error message"));
		}
	}

	/** Tests the {@link Conditions#checkConfiguredStateNotNull(Object)} and the {@link Conditions#checkConfiguredStateNotNull(Object, String)} methods. */
	@Test
	public void testCheckConfigurationNotNull() {
		checkConfiguredStateNotNull(new Object());
		checkConfiguredStateNotNull(new Object(), "error message");
	}

	/** Tests the {@link Conditions#checkConfiguredStateNotNull(Object)} method with a false statement. */
	@Test
	public void testCheckConfigurationNotNullWithANullObject() {
		assertThrows(ConfiguredStateException.class, () -> checkConfiguredStateNotNull(null));
	}

	/**
	 * Tests the {@link Conditions#checkConfiguredStateNotNull(Object)} and {@link Conditions#checkConfiguredStateNotNull(Object, String)} methods with a false
	 * statement and how the messages are being formatted.
	 */
	@Test
	public void testCheckConfigurationNotNullErrorMessage() {

		try {
			checkConfiguredStateNotNull(null);
			fail("The statement above should have thrown an ConfigurationException");
		} catch(final ConfiguredStateException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo(null));
		}

		try {
			checkConfiguredStateNotNull(null, "error message");
			fail("The statement above should have thrown an ConfigurationException");
		} catch(final ConfiguredStateException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo("error message"));
		}

	}

	/** Tests the {@link Conditions#checkIndexBounds(int, int)} and {@link Conditions#checkIndexBounds(int, int, int)} methods. */
	@Test
	public void testCheckIndexBoundsWithIntegers() {

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
	public void testCheckIndexBoundsWithLongs() {

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
	public void testCheckIndexBoundsWithAMinorIntValue() {
		assertThrows(IndexOutOfBoundsException.class, () -> checkIndexBounds(-1, 10));
	}

	/** Tests the {@link Conditions#checkIndexBounds(int, int)} method with a false statement. */
	@Test
	public void testCheckIndexBoundsWithABiggerIntValue() {
		assertThrows(IndexOutOfBoundsException.class, () -> checkIndexBounds(10, 10));
	}

	/** Tests the {@link Conditions#checkIndexBounds(int, int, int)} method with a false statement. */
	@Test
	public void testCheckIndexBoundsWithAMinorIntValueUsingARange() {
		assertThrows(IndexOutOfBoundsException.class, () -> checkIndexBounds(-1, 0, 10));
	}

	/** Tests the {@link Conditions#checkIndexBounds(int, int, int)} method with a false statement. */
	@Test
	public void testCheckIndexBoundsWithABiggerIntValueUsingARange() {
		assertThrows(IndexOutOfBoundsException.class, () -> checkIndexBounds(10, 0, 10));
	}

	/** Tests the {@link Conditions#checkIndexBounds(long, long)} method with a false statement. */
	@Test
	public void testCheckIndexBoundsWithAMinorLongValue() {
		assertThrows(IndexOutOfBoundsException.class, () -> checkIndexBounds(-1L, 10L));
	}

	/** Tests the {@link Conditions#checkIndexBounds(long, long, long)} method with a false statement. */
	@Test
	public void testCheckIndexBoundsWithAMinorLongValueUsingARange() {
		assertThrows(IndexOutOfBoundsException.class, () -> checkIndexBounds(-1L, 0L, 10L));
	}

	/** Tests the {@link Conditions#checkIndexBounds(long, long, long)} method with a false statement. */
	@Test
	public void testCheckIndexBoundsWithABiggerLongValueUsingARange() {
		assertThrows(IndexOutOfBoundsException.class, () -> checkIndexBounds(10L, 0L, 10L));
	}

	/** Tests the {@link Conditions#checkIndexBounds(long, long)} method with a false statement. */
	@Test
	public void testCheckIndexBoundsWithABiggerLongValue() {
		assertThrows(IndexOutOfBoundsException.class, () -> checkIndexBounds(10L, 10L));
	}

	/** Tests the {@link Conditions#checkState(boolean)} and the {@link Conditions#checkState(boolean, String, Object...)} methods. */
	@Test
	public void testCheckState() {
		checkState(true);
		checkState(true, "error message");
		checkState(true, "error message", 123);
		checkState(true, "error message %d", 123);
	}

	/** Tests the {@link Conditions#checkState(boolean)} method with a false statement. */
	@Test
	public void testCheckStateWithAFalseStatement() {
		assertThrows(IllegalStateException.class, () -> checkState(false));
	}

	/**
	 * Tests the {@link Conditions#checkState(boolean)} and {@link Conditions#checkState(boolean, String, Object...)} methods with a false statement and how the
	 * messages are being formatted.
	 */
	@Test
	public void testCheckStateErrorMessage() {

		try {
			checkState(false);
			fail("The statement above should have thrown an IllegalStateException");
		} catch(final IllegalStateException illegalStateException) {
			assertThat(illegalStateException.getMessage(), equalTo(null));
		}

		try {
			checkState(false, "error message");
			fail("The statement above should have thrown an IllegalStateException");
		} catch(final IllegalStateException illegalStateException) {
			assertThat(illegalStateException.getMessage(), equalTo("error message"));
		}

		try {
			checkState(false, "error message %d", 123);
			fail("The statement above should have thrown an IllegalStateException");
		} catch(final IllegalStateException illegalStateException) {
			assertThat(illegalStateException.getMessage(), equalTo("error message 123"));
		}

		try {
			checkState(false, "error message", 123); // The arguments of the error message should be ignored.
			fail("The statement above should have thrown an IllegalStateException");
		} catch(final IllegalStateException illegalStateException) {
			assertThat(illegalStateException.getMessage(), equalTo("error message"));
		}
	}

	/**
	 * Tests the {@link Conditions#unexpected(String)} method.
	 */
	@Test
	public void testUnexpectedWithAString() {

		assertThat(unexpected("test message"), instanceOf(IllegalStateException.class));
		assertThat(unexpected("test message").getMessage(), equalTo("test message"));
		assertThat(unexpected("test message").getCause(), equalTo(null));

	}

	/**
	 * Tests the {@link Conditions#unexpected(Throwable)} method.
	 */
	@Test
	public void testUnexpectedWithAThrowable() {

		final Throwable throwableCause = new IllegalArgumentException();

		assertThat(unexpected(throwableCause), instanceOf(IllegalStateException.class));
		assertThat(unexpected(throwableCause).getMessage(), equalTo(null));
		assertThat(unexpected(throwableCause).getCause(), equalTo(throwableCause));

	}

	/**
	 * Tests the {@link Conditions#unexpected(String, Throwable)} method.
	 */
	@Test
	public void testUnexpectedWithAStringAndThrowable() {

		final Throwable throwableCause = new IllegalArgumentException();

		assertThat(unexpected("test message", throwableCause), instanceOf(IllegalStateException.class));
		assertThat(unexpected("test message", throwableCause).getMessage(), equalTo("test message"));
		assertThat(unexpected("test message", throwableCause).getCause(), equalTo(throwableCause));

	}

	/**
	 * Tests the {@link Conditions#impossible()} method.
	 */
	@Test
	public void testImpossible() {

		assertThat(impossible(), instanceOf(AssertionError.class));
		assertThat(impossible().getMessage(), equalTo(null));
		assertThat(impossible().getCause(), equalTo(null));

	}

	/**
	 * Tests the {@link Conditions#impossible(String)} method.
	 */
	@Test
	public void testImpossibleWithAString() {

		assertThat(impossible("test message"), instanceOf(AssertionError.class));
		assertThat(impossible("test message").getMessage(), equalTo("test message"));
		assertThat(impossible("test message").getCause(), equalTo(null));

	}

	/**
	 * Tests the {@link Conditions#impossible(Throwable)} method.
	 */
	@Test
	public void testImpossibleWithAThrowable() {

		final Throwable throwableCause = new IllegalArgumentException();

		assertThat(impossible(throwableCause), instanceOf(AssertionError.class));
		assertThat(impossible(throwableCause).getMessage(), equalTo(null));
		assertThat(impossible(throwableCause).getCause(), equalTo(throwableCause));

	}

	/**
	 * Tests the {@link Conditions#impossible(String, Throwable)} method.
	 */
	@Test
	public void testImpossibleWithAStringAndAThrowable() {

		final Throwable throwableCause = new IllegalArgumentException();

		assertThat(impossible("test message", throwableCause), instanceOf(AssertionError.class));
		assertThat(impossible("test message", throwableCause).getMessage(), equalTo("test message"));
		assertThat(impossible("test message", throwableCause).getCause(), equalTo(throwableCause));

	}

}
