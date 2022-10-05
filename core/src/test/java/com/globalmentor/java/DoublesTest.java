/*
 * Copyright © 2020 GlobalMentor, Inc. <https://www.globalmentor.com/>
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
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

/**
 * Tests of {@link Doubles}.
 * @author Garret Wilson
 */
public class DoublesTest {

	/** @see Doubles#equalsLenient(double, double) */
	@Test
	public void testEqualsLenient() {
		assertThat("Identical double values are equal.", Doubles.equalsLenient(123.456, 123.456), is(true));
		assertThat("Zero and negative zero are equal.", Doubles.equalsLenient(0.0, -0.0), is(true));
		assertThat("Zero and negative zero are equal.", Doubles.equalsLenient(-0.0, 0.0), is(true));
		assertThat("NaNs are equal.", Doubles.equalsLenient(NaN, NaN), is(true));
		assertThat("Non-matching NaNs are not equal.", Doubles.equalsLenient(123.456, NaN), is(false));
		assertThat("Non-matching NaNs are not equal.", Doubles.equalsLenient(NaN, 123.456), is(false));
		assertThat("Different double values are not considered equal.", Doubles.equalsLenient(123.455, 123.456), is(false));
		assertThat("Different double values are not considered equal.", Doubles.equalsLenient(123.23, 123.24), is(false));
		assertThat("Different double values are not considered equal.", Doubles.equalsLenient(123.234, 123.243), is(false));
		assertThat("Different double values are not considered equal.", Doubles.equalsLenient(123.446, 123.456), is(false));
		assertThat("Different double values are not considered equal.", Doubles.equalsLenient(123.0, 456.0), is(false));
		final double pointOneTimes11 = 0.1 * 11; //see https://howtodoinjava.com/java-examples/correctly-compare-float-double/
		final double pointOne11Times = 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1;
		assertFalse(pointOneTimes11 == pointOne11Times, "0.1 multplied by 11 is not the same double value as 0.1 11 times.");
		assertThat("Very close double values are considered equal.", Doubles.equalsLenient(pointOneTimes11, pointOne11Times), is(true));
	}

	/** @see Doubles#equalsLenient(double, double, double) */
	@Test
	public void testEqualsLenientWithTolerance() {
		assertThat("Identical double values are equal.", Doubles.equalsLenient(123.456, 123.456, 0.01), is(true));
		assertThat("Identical double values are equal with zero tolerance.", Doubles.equalsLenient(123.456, 123.456, 0.0), is(true));
		assertThat("Identical double values are equal with NaN tolerance.", Doubles.equalsLenient(123.456, 123.456, NaN), is(true));
		assertThat("Zero and negative zero are equal.", Doubles.equalsLenient(0.0, -0.0, 0.01), is(true));
		assertThat("Zero and negative zero are equal.", Doubles.equalsLenient(-0.0, 0.0, 0.01), is(true));
		assertThat("Zero and negative zero are equal with zero tolerance.", Doubles.equalsLenient(0.0, -0.0, 0.0), is(true));
		assertThat("Zero and negative zero are equal with NaN tolerance.", Doubles.equalsLenient(0.0, -0.0, NaN), is(true));
		assertThat("NaNs are equal.", Doubles.equalsLenient(NaN, NaN, 0.01), is(true));
		assertThat("NaNs are equal with zero tolerance.", Doubles.equalsLenient(NaN, NaN, 0.0), is(true));
		assertThat("NaNs are equal with NaN tolerance.", Doubles.equalsLenient(NaN, NaN, NaN), is(true));
		assertThat("Non-matching NaNs are not equal.", Doubles.equalsLenient(123.456, NaN, 0.01), is(false));
		assertThat("Non-matching NaNs are not equal.", Doubles.equalsLenient(NaN, 123.456, 0.01), is(false));
		assertThat("Double values within tolerance are considered equal.", Doubles.equalsLenient(123.455, 123.456, 0.01), is(true));
		assertThat("Double values within tolerance are considered equal.", Doubles.equalsLenient(123.23, 123.24, 0.01), is(true));
		assertThat("Double values within tolerance are considered equal.", Doubles.equalsLenient(123.234, 123.243, 0.01), is(true));
		assertThat("Double values outside of tolerance are not considered equal.", Doubles.equalsLenient(123.446, 123.456, 0.01), is(false));
		assertThat("Double values outside of tolerance are not considered equal.", Doubles.equalsLenient(123.0, 456.0, 0.01), is(false));
	}

}
