/*
 * Copyright © 2019 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.text;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.*;

/**
 * Tests of ASCII utilities.
 * @author Garret Wilson
 * @see ASCII
 */
public class ASCIITest {

	/** @see ASCII#equalsIgnoreCase(CharSequence, CharSequence) */
	@Test
	void testEqualsIgnoreCaseTwoStrings() {
		assertThat(ASCII.equalsIgnoreCase(null, null), is(true));
		assertThat(ASCII.equalsIgnoreCase("", null), is(false));
		assertThat(ASCII.equalsIgnoreCase(null, ""), is(false));
		assertThat(ASCII.equalsIgnoreCase("x", null), is(false));
		assertThat(ASCII.equalsIgnoreCase(null, "x"), is(false));
		assertThat(ASCII.equalsIgnoreCase("foo", null), is(false));
		assertThat(ASCII.equalsIgnoreCase(null, "foo"), is(false));
		assertThat(ASCII.equalsIgnoreCase("", ""), is(true));
		assertThat(ASCII.equalsIgnoreCase("x", ""), is(false));
		assertThat(ASCII.equalsIgnoreCase("", "x"), is(false));
		assertThat(ASCII.equalsIgnoreCase("x", "x"), is(true));
		assertThat(ASCII.equalsIgnoreCase("X", "x"), is(true));
		assertThat(ASCII.equalsIgnoreCase("x", "X"), is(true));
		assertThat(ASCII.equalsIgnoreCase("X", "X"), is(true));
		assertThat(ASCII.equalsIgnoreCase("x", "xx"), is(false));
		assertThat(ASCII.equalsIgnoreCase("xx", "x"), is(false));
		assertThat(ASCII.equalsIgnoreCase("xx", "xx"), is(true));
		assertThat(ASCII.equalsIgnoreCase("xX", "Xx"), is(true));
		assertThat(ASCII.equalsIgnoreCase("foo", ""), is(false));
		assertThat(ASCII.equalsIgnoreCase("", "foo"), is(false));
		assertThat(ASCII.equalsIgnoreCase(" foo", "foo"), is(false));
		assertThat(ASCII.equalsIgnoreCase("foo ", "foo"), is(false));
		assertThat(ASCII.equalsIgnoreCase("foo", " foo"), is(false));
		assertThat(ASCII.equalsIgnoreCase("foo", "foo "), is(false));
		assertThat(ASCII.equalsIgnoreCase(" foo ", " foo "), is(true));
		assertThat(ASCII.equalsIgnoreCase("foo", "foo"), is(true));
		assertThat(ASCII.equalsIgnoreCase("fOo", "FoO"), is(true));
		assertThat(ASCII.equalsIgnoreCase("FOO", "foo"), is(true));
		assertThat(ASCII.equalsIgnoreCase("foo", "bar"), is(false));
		assertThat(ASCII.equalsIgnoreCase("foo", "foobar"), is(false));
		assertThat(ASCII.equalsIgnoreCase("foobar", "foo"), is(false));
		assertThat(ASCII.equalsIgnoreCase("bar", "foo"), is(false));
		assertThat(ASCII.equalsIgnoreCase("foobar", "fooBar"), is(true));
		assertThat(ASCII.equalsIgnoreCase("foobar", "foofar"), is(false));
		assertThat(ASCII.equalsIgnoreCase("foobar", "fooFar"), is(false));
		assertThat(ASCII.equalsIgnoreCase("fooBar", "FooBar"), is(true));
		assertThat(ASCII.equalsIgnoreCase("fooBar", "FOOBAR"), is(true));
		assertThat(ASCII.equalsIgnoreCase("FOOBAR", "foobar"), is(true));
	}

	/** @see ASCII#valueOfDigit(char) */
	@Test
	void testValueOfDigit() {
		assertThat(ASCII.valueOfDigit('0'), is(0));
		assertThat(ASCII.valueOfDigit('1'), is(1));
		assertThat(ASCII.valueOfDigit('5'), is(5));
		assertThat(ASCII.valueOfDigit('9'), is(9));
		assertThrows(IllegalArgumentException.class, () -> ASCII.valueOfDigit('c'));
		assertThrows(IllegalArgumentException.class, () -> ASCII.valueOfDigit('C'));
		assertThrows(IllegalArgumentException.class, () -> ASCII.valueOfDigit('x'));
		assertThrows(IllegalArgumentException.class, () -> ASCII.valueOfDigit('!'));
		assertThrows(IllegalArgumentException.class, () -> ASCII.valueOfDigit((char)(ASCII.DIGIT_FIRST - 1)));
		assertThrows(IllegalArgumentException.class, () -> ASCII.valueOfDigit((char)(ASCII.DIGIT_LAST + 1)));
	}

	/** @see ASCII#valueOfhexDigit(char) */
	@Test
	void testValueOfHexDigit() {
		assertThat(ASCII.valueOfHexDigit('0'), is(0));
		assertThat(ASCII.valueOfHexDigit('1'), is(1));
		assertThat(ASCII.valueOfDigit('5'), is(5));
		assertThat(ASCII.valueOfHexDigit('9'), is(9));
		assertThat(ASCII.valueOfHexDigit('a'), is(10));
		assertThat(ASCII.valueOfHexDigit('A'), is(10));
		assertThat(ASCII.valueOfHexDigit('c'), is(12));
		assertThat(ASCII.valueOfHexDigit('C'), is(12));
		assertThat(ASCII.valueOfHexDigit('f'), is(15));
		assertThat(ASCII.valueOfHexDigit('F'), is(15));
		assertThrows(IllegalArgumentException.class, () -> ASCII.valueOfDigit('x'));
		assertThrows(IllegalArgumentException.class, () -> ASCII.valueOfDigit('!'));
		assertThrows(IllegalArgumentException.class, () -> ASCII.valueOfHexDigit((char)(ASCII.DIGIT_FIRST - 1)));
		assertThrows(IllegalArgumentException.class, () -> ASCII.valueOfHexDigit((char)(ASCII.DIGIT_LAST + 1)));
		assertThrows(IllegalArgumentException.class, () -> ASCII.valueOfHexDigit((char)('a' - 1)));
		assertThrows(IllegalArgumentException.class, () -> ASCII.valueOfHexDigit((char)('A' - 1)));
		assertThrows(IllegalArgumentException.class, () -> ASCII.valueOfHexDigit((char)('f' + 1)));
		assertThrows(IllegalArgumentException.class, () -> ASCII.valueOfHexDigit((char)('F' + 1)));
	}

}
