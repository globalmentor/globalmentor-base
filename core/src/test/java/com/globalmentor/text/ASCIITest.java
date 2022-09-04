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

	/** @see ASCII#equalsIgnoreCase(char, char) */
	@Test
	void testEqualsIgnoreCaseTwoCharacters() {
		assertThat(ASCII.equalsIgnoreCase('a', 'a'), is(true));
		assertThat(ASCII.equalsIgnoreCase('A', 'a'), is(true));
		assertThat(ASCII.equalsIgnoreCase('a', 'A'), is(true));
		assertThat(ASCII.equalsIgnoreCase('A', 'A'), is(true));
		assertThat(ASCII.equalsIgnoreCase('x', 'x'), is(true));
		assertThat(ASCII.equalsIgnoreCase('X', 'x'), is(true));
		assertThat(ASCII.equalsIgnoreCase('x', 'X'), is(true));
		assertThat(ASCII.equalsIgnoreCase('X', 'X'), is(true));
		assertThat(ASCII.equalsIgnoreCase('z', 'z'), is(true));
		assertThat(ASCII.equalsIgnoreCase('Z', 'z'), is(true));
		assertThat(ASCII.equalsIgnoreCase('z', 'Z'), is(true));
		assertThat(ASCII.equalsIgnoreCase('Z', 'Z'), is(true));
		assertThat(ASCII.equalsIgnoreCase('b', 'x'), is(false));
		assertThat(ASCII.equalsIgnoreCase('b', 'X'), is(false));
		assertThat(ASCII.equalsIgnoreCase('b', '5'), is(false));
		assertThat(ASCII.equalsIgnoreCase('b', '!'), is(false));
		assertThat(ASCII.equalsIgnoreCase('B', 'x'), is(false));
		assertThat(ASCII.equalsIgnoreCase('B', 'X'), is(false));
		assertThat(ASCII.equalsIgnoreCase('B', '5'), is(false));
		assertThat(ASCII.equalsIgnoreCase('B', '!'), is(false));
		assertThat(ASCII.equalsIgnoreCase('x', 'b'), is(false));
		assertThat(ASCII.equalsIgnoreCase('X', 'b'), is(false));
		assertThat(ASCII.equalsIgnoreCase('5', 'b'), is(false));
		assertThat(ASCII.equalsIgnoreCase('!', 'b'), is(false));
		assertThat(ASCII.equalsIgnoreCase('x', 'B'), is(false));
		assertThat(ASCII.equalsIgnoreCase('X', 'B'), is(false));
		assertThat(ASCII.equalsIgnoreCase('5', 'B'), is(false));
		assertThat(ASCII.equalsIgnoreCase('!', 'B'), is(false));
	}

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

	/** @see ASCII#indexOfIgnoreCase(CharSequence, char, int) */
	@Test
	void testIndexOfIgnoreCase() {
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'f', 0), is(0));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'F', 0), is(0));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'f', 1), is(16));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'F', 1), is(16));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'f', 16), is(16));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'F', 16), is(16));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'f', 17), is(-1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'F', 17), is(-1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'o', 0), is(1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'O', 0), is(1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'x', 0), is(-1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", '5', 0), is(-1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'o', 1), is(1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'O', 1), is(1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'x', 1), is(-1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", '5', 1), is(-1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'o', 2), is(2));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'O', 2), is(2));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'x', 2), is(-1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", '5', 2), is(-1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'o', 3), is(14));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'O', 3), is(14));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'x', 3), is(-1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", '5', 3), is(-1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'o', 4), is(14));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'O', 4), is(14));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'x', 4), is(-1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", '5', 4), is(-1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'o', 16), is(-1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'O', 16), is(-1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'x', 16), is(-1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", '5', 16), is(-1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'o', 17), is(-1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'O', 17), is(-1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", 'x', 17), is(-1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", '5', 17), is(-1));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", '!', 0), is(17));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", '!', 1), is(17));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", '!', 8), is(17));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", '!', 16), is(17));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", '!', 17), is(17));
		assertThat(ASCII.indexOfIgnoreCase("foobar12321raboof!", '!', 18), is(17));
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
