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

import static org.junit.Assert.*;
import static org.hamcrest.Matchers.*;

import org.junit.Test;

import static com.globalmentor.java.Conditions.*;

/**
 * Tests for {@link Conditions}.
 * 
 * @author Magno Nascimento
 * 
 */
public class ConditionsTest {

	/** Tests the {@link Conditions#checkArgument(boolean)} and the {@link Conditions#checkArgument(boolean, String, Object...)} methods. */
	@Test
	public void testCheckState() {
		checkState(true);
		checkState(true, "error message");
		checkState(true, "error message", 123);
		checkState(true, "error message %d", 123);
	}

	/** Tests the {@link Conditions#checkArgument(boolean)} method with a false statement. */
	@Test(expected = IllegalStateException.class)
	public void testCheckStateWithAFalseStatement() {
		checkState(false);
	}

	/**
	 * Tests the {@link Conditions#checkArgument(boolean)} and {@link Conditions#checkArgument(boolean, String, Object...)} methods with a false statement and how
	 * the messages are being formatted.
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

}
