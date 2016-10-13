/*
 * Copyright © 2016 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.java.Conditions.*;

import com.globalmentor.model.*;

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.*;

import org.junit.*;

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
		checkArgument(true, "error message {0}", 123);
	}

	/** Tests the {@link Conditions#checkArgument(boolean)} method with a false statement. */
	@Test(expected = IllegalArgumentException.class)
	public void testCheckArgumentWithAFalseStatement() {
		checkArgument(false);
	}

	/**
	 * Tests the {@link Conditions#checkArgument(boolean)} and {@link Conditions#checkArgument(boolean, String, Object...)} methods with a false statement and how
	 * the messages are being formatted.
	 */
	@Test
	public void testCheckArgumentErrorMessage() {

		try {
			checkArgument(false);
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo(null));
		}

		try {
			checkArgument(false, "error message");
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo("error message"));
		}

		try {
			checkArgument(false, "error message {0}", 123);
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo("error message 123"));
		}

		try {
			checkArgument(false, "error message", 123); // The arguments of the error message should be ignored.
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo("error message"));
		}
	}

	/** Tests the {@link Conditions#checkArgumentNotNull(boolean)} and the {@link Conditions#checkArgumentNotNull(boolean, String, Object...)} methods. */
	@Test
	public void testCheckArgumentNotNull() {
		checkArgumentNotNull(new Object());
		checkArgumentNotNull(new Object(), "error message");
		checkArgumentNotNull(new Object(), "error message", 123);
		checkArgumentNotNull(new Object(), "error message {0}", 123);
	}

	/** Tests the {@link Conditions#checkArgumentNotNull(boolean)} method with a false statement. */
	@Test(expected = IllegalArgumentException.class)
	public void testCheckArgumentNotNullWithANullObject() {
		checkArgumentNotNull(null);
	}

	/**
	 * Tests the {@link Conditions#checkArgumentNotNull(boolean)} and {@link Conditions#checkArgumentNotNull(boolean, String, Object...)} methods with a false
	 * statement and how the messages are being formatted.
	 */
	@Test
	public void testCheckArgumentNotNullErrorMessage() {

		try {
			checkArgumentNotNull(null);
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo(null));
		}

		try {
			checkArgumentNotNull(null, "error message");
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo("error message"));
		}

		try {
			checkArgumentNotNull(null, "error message {0}", 123);
		} catch(final IllegalArgumentException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo("error message 123"));
		}

		try {
			checkArgumentNotNull(null, "error message", 123); // The arguments of the error message should be ignored.
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
	@Test(expected = IllegalArgumentException.class)
	public void testCheckArgumentMinimumWithAMinorIntValue() {
		checkArgumentMinimum(0, 10);
	}

	/** Tests the {@link Conditions#checkArgumentMinimum(long, long)} method with a false statement. */
	@Test(expected = IllegalArgumentException.class)
	public void testCheckArgumentMinimumWithAMinorLongValue() {
		checkArgumentMinimum(0L, 10L);
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
	@Test(expected = IllegalArgumentException.class)
	public void testCheckArgumentNotNegativeWithANegativeIntValue() {
		checkArgumentNotNegative(-10);
	}

	/** Tests the {@link Conditions#checkArgumentNotNegative(long)} method with a false statement. */
	@Test(expected = IllegalArgumentException.class)
	public void testCheckArgumentNotNegativeWithANegativeLongValue() {
		checkArgumentNotNegative(-10L);
	}

	/** Tests the {@link Conditions#checkArgumentPositive(int)} method with a false statement. */
	@Test(expected = IllegalArgumentException.class)
	public void testCheckArgumentPositiveWithANegativeIntValue() {
		checkArgumentPositive(-10);
	}

	/** Tests the {@link Conditions#checkArgumentPositive(long)} method with a false statement. */
	@Test(expected = IllegalArgumentException.class)
	public void testCheckArgumentPositiveWithANegativeLongValue() {
		checkArgumentPositive(-10L);
	}

	@Ignore
	//TODO The version of checkArgumentRange that verifies if an interval of integer is inside another isn't working correctly
	/**
	 * Tests the {@link Conditions#checkArgumentRange(int, int, int)}, {@link Conditions#checkArgumentRange(long, long, long)} and
	 * {@link Conditions#checkArgumentRange(int, int, int, int)} methods.
	 */
	@Test
	public void testCheckArgumentRange() {

		{ //tests for Conditions#checkArgumentRange(int, int, int)

			//test for the edges
			checkArgumentRange(0, 0, 10);
			checkArgumentRange(10, 0, 10);

			//test for the signals
			checkArgumentRange(5, 0, 10); //test for the middle
			checkArgumentRange(-5, -10, 0);
			checkArgumentRange(0, -5, 5);
		}

		{ //tests for Conditions#checkArgumentRange(long, long, long)

			//test for the edges
			checkArgumentRange(0L, 0L, 10L);
			checkArgumentRange(10L, 0L, 10L);

			//test for the signals
			checkArgumentRange(5L, 0L, 10L); //test for the middle
			checkArgumentRange(-5L, -10L, 0L);
			checkArgumentRange(0L, -5L, 5L);
		}

		{ //tests for Conditions#checkArgumentRange(int, int, int, int)

			//test for the edges
			checkArgumentRange(0, 5, 0, 10);
			checkArgumentRange(5, 10, 0, 10);
			checkArgumentRange(0, 10, 0, 10);

			//test for the signals
			checkArgumentRange(1, 9, 0, 10); //test for the middle
			checkArgumentRange(-9, -1, -10, 0);
			checkArgumentRange(-4, 4, -5, 5);
		}

	}

	/** Tests the {@link Conditions#checkArgumentRange(int, int, int)} method with a false statement. */
	@Test(expected = IllegalArgumentException.class)
	public void testCheckArgumentRangeWithAMinorIntValue() {
		checkArgumentRange(-1, 0, 10);
	}

	/** Tests the {@link Conditions#checkArgumentRange(int, int, int)} method with a false statement. */
	@Test(expected = IllegalArgumentException.class)
	public void testCheckArgumentRangeWithABiggerIntValue() {
		checkArgumentRange(11, 0, 10);
	}

	/** Tests the {@link Conditions#checkArgumentRange(long, long, long)} method with a false statement. */
	@Test(expected = IllegalArgumentException.class)
	public void testCheckArgumentRangeWithAMinorLongValue() {
		checkArgumentRange(-1L, 0L, 10L);
	}

	/** Tests the {@link Conditions#checkArgumentRange(long, long, long)} method with a false statement. */
	@Test(expected = IllegalArgumentException.class)
	public void testCheckArgumentRangeWithABiggerLongValue() {
		checkArgumentRange(11L, 0L, 10L);
	}

	/** Tests the {@link Conditions#checkArgumentRange(int, int, int, int)} method with a false statement. */
	@Test(expected = IllegalArgumentException.class)
	public void testCheckArgumentRangeWithAMoreNegativeInterval() {
		checkArgumentRange(-10, -1, 0, 10);
	}

	@Ignore
	//TODO The version of checkArgumentRange that verifies if an interval of integer is inside another isn't working correctly
	/** Tests the {@link Conditions#checkArgumentRange(int, int, int, int)} method with a false statement. */
	@Test(expected = IllegalArgumentException.class)
	public void testCheckArgumentRangeWithAMorePositiveInterval() {
		checkArgumentRange(11, 20, 0, 10);
	}

	/** Tests the {@link Conditions#checkArgumentRange(int, int, int, int)} method with a false statement. */
	@Test(expected = IllegalArgumentException.class)
	public void testCheckArgumentRangeWithABiggerInterval() {
		checkArgumentRange(-1, 11, 0, 10);
	}

	/** Tests the {@link Conditions#checkConfiguration(boolean)} and the {@link Conditions#checkConfiguration(boolean, String, Object...)} methods. */
	@Test
	public void testCheckConfiguration() {
		checkConfiguration(true);
		checkConfiguration(true, "error message");
		checkConfiguration(true, "error message", 123);
		checkConfiguration(true, "error message {0}", 123);
	}

	@Ignore
	//TODO The version of the method without a description referrs to checkArgument instead of checkConfiguration
	/** Tests the {@link Conditions#checkConfiguration(boolean)} method with a false statement. */
	@Test(expected = ConfigurationException.class)
	public void testCheckConfigurationWithAFalseStatement() {
		checkConfiguration(false);
	}

	@Ignore
	//TODO The version of the method without a description referrs to checkArgument instead of checkConfiguration
	/**
	 * Tests the {@link Conditions#checkConfiguration(boolean)} and {@link Conditions#checkConfiguration(boolean, String, Object...)} methods with a false
	 * statement and how the messages are being formatted.
	 */
	@Test
	public void testCheckConfigurationErrorMessage() {

		try {
			checkConfiguration(false);
		} catch(final ConfigurationException configurationException) {
			assertThat(configurationException.getMessage(), equalTo(null));
		}

		try {
			checkConfiguration(false, "error message");
		} catch(final ConfigurationException configurationException) {
			assertThat(configurationException.getMessage(), equalTo("error message"));
		}

		try {
			checkConfiguration(false, "error message {0}", 123);
		} catch(final ConfigurationException configurationException) {
			assertThat(configurationException.getMessage(), equalTo("error message 123"));
		}

		try {
			checkConfiguration(false, "error message", 123); // The arguments of the error message should be ignored.
		} catch(final ConfigurationException configurationException) {
			assertThat(configurationException.getMessage(), equalTo("error message"));
		}
	}

	/** Tests the {@link Conditions#checkConfigurationNotNull(Object)} and the {@link Conditions#checkConfigurationNotNull(Object, String)} methods. */
	@Test
	public void testCheckConfigurationNotNull() {
		checkConfigurationNotNull(new Object());
		checkConfigurationNotNull(new Object(), "error message");
	}

	/** Tests the {@link Conditions#checkConfigurationNotNull(Object)} method with a false statement. */
	@Test(expected = ConfigurationException.class)
	public void testCheckConfigurationNotNullWithANullObject() {
		checkConfigurationNotNull(null);
	}

	/**
	 * Tests the {@link Conditions#checkConfigurationNotNull(Object)} and {@link Conditions#checkConfigurationNotNull(Object, String)} methods with a false
	 * statement and how the messages are being formatted.
	 */
	@Test
	public void testCheckConfigurationNotNullErrorMessage() {

		try {
			checkConfigurationNotNull(null);
		} catch(final ConfigurationException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo(null));
		}

		try {
			checkConfigurationNotNull(null, "error message");
		} catch(final ConfigurationException illegalArgumentException) {
			assertThat(illegalArgumentException.getMessage(), equalTo("error message"));
		}

	}

	/**
	 * Tests the {@link Conditions#checkIndexBounds(int, int)}, {@link Conditions#checkIndexBounds(long, long)},
	 * {@link Conditions#checkIndexBounds(int, int, int)} and {@link Conditions#checkIndexBounds(long, long, long)} methods.
	 */
	@Test
	public void testCheckIndexBounds() {

		{ //test for the Conditions#checkIndexBounds(int, int) and checkIndexBounds(int, int, int)

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

		{ //test for the Conditions#checkIndexBounds(long, long) and checkIndexBounds(long, long, long)

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
	}

	/** Tests the {@link Conditions#checkIndexBounds(int, int)} method with a false statement. */
	@Test(expected = IndexOutOfBoundsException.class)
	public void testCheckIndexBoundsWithAMinorIntValue() {
		checkIndexBounds(-1, 10);
	}

	/** Tests the {@link Conditions#checkIndexBounds(int, int)} method with a false statement. */
	@Test(expected = IndexOutOfBoundsException.class)
	public void testCheckIndexBoundsWithABiggerIntValue() {
		checkIndexBounds(10, 10);
	}

	/** Tests the {@link Conditions#checkIndexBounds(int, int, int)} method with a false statement. */
	@Test(expected = IndexOutOfBoundsException.class)
	public void testCheckIndexBoundsWithAMinorIntValueUsingARange() {
		checkIndexBounds(-1, 0, 10);
	}

	/** Tests the {@link Conditions#checkIndexBounds(int, int, int)} method with a false statement. */
	@Test(expected = IndexOutOfBoundsException.class)
	public void testCheckIndexBoundsWithABiggerIntValueUsingARange() {
		checkIndexBounds(10, 0, 10);
	}

	/** Tests the {@link Conditions#checkIndexBounds(long, long)} method with a false statement. */
	@Test(expected = IndexOutOfBoundsException.class)
	public void testCheckIndexBoundsWithAMinorLongValue() {
		checkIndexBounds(-1L, 10L);
	}

	/** Tests the {@link Conditions#checkIndexBounds(long, long, long)} method with a false statement. */
	@Test(expected = IndexOutOfBoundsException.class)
	public void testCheckIndexBoundsWithAMinorLongValueUsingARange() {
		checkIndexBounds(-1L, 0L, 10L);
	}

	/** Tests the {@link Conditions#checkIndexBounds(long, long, long)} method with a false statement. */
	@Test(expected = IndexOutOfBoundsException.class)
	public void testCheckIndexBoundsWithABiggerLongValueUsingARange() {
		checkIndexBounds(10L, 0L, 10L);
	}

	/** Tests the {@link Conditions#checkIndexBounds(long, long)} method with a false statement. */
	@Test(expected = IndexOutOfBoundsException.class)
	public void testCheckIndexBoundsWithABiggerLongValue() {
		checkIndexBounds(10L, 10L);
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
	@Test(expected = IllegalStateException.class)
	public void testCheckStateWithAFalseStatement() {
		checkState(false);
	}

	/**
	 * Tests the {@link Conditions#checkState(boolean)} and {@link Conditions#checkState(boolean, String, Object...)} methods with a false statement and how the
	 * messages are being formatted.
	 */
	@Test
	public void testCheckStateErrorMessage() {

		try {
			checkState(false);
		} catch(final IllegalStateException illegalStateException) {
			assertThat(illegalStateException.getMessage(), equalTo(null));
		}

		try {
			checkState(false, "error message");
		} catch(final IllegalStateException illegalStateException) {
			assertThat(illegalStateException.getMessage(), equalTo("error message"));
		}

		try {
			checkState(false, "error message %d", 123);
		} catch(final IllegalStateException illegalStateException) {
			assertThat(illegalStateException.getMessage(), equalTo("error message 123"));
		}

		try {
			checkState(false, "error message", 123); // The arguments of the error message should be ignored.
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
