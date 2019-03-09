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

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.*;

import org.junit.*;

import com.globalmentor.text.ASCII;
import com.globalmentor.text.Case;

/**
 * Tests for {@link StringBuilders}.
 * 
 * @author Magno Nascimento
 */
public class StringBuildersTest {

	/** Tests to see if the method {@link StringBuilders#escapeHex(StringBuilder, int, char, int)} isn't escaping anything from an empty {@link StringBuilder}. */
	@Test(expected = StringIndexOutOfBoundsException.class)
	public void testEscapeHexWithEmptyString() {
		final StringBuilder stringBuilder = new StringBuilder();

		StringBuilders.escapeHex(stringBuilder, 0, 1, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
	}

	/**
	 * Tests to see if the method {@link StringBuilders#escapeHex(StringBuilder, int, char, int)} is throwing an exception if a surrogate char is not used
	 * correctly.
	 */
	@Test(expected = IllegalStateException.class)
	public void testEscapeHexWithOnlyHighSurrogate() {
		final StringBuilder stringBuilder = new StringBuilder(String.valueOf(Character.toChars(0x10014)[0]));

		StringBuilders.escapeHex(stringBuilder, 0, 1, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
	}

	/**
	 * Tests to see if the method {@link StringBuilders#escapeHex(StringBuilder, int, char, int)} is throwing an exception if a surrogate char is not used
	 * correctly.
	 */
	@Test(expected = IllegalStateException.class)
	public void testEscapeHexWithOnlyHighSurrogateInTheBegginingOfAString() {
		final StringBuilder stringBuilder = new StringBuilder(Character.toChars(0x10014)[0] + "touché");

		StringBuilders.escapeHex(stringBuilder, 0, stringBuilder.length(), null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
	}

	/**
	 * Tests to see if the method {@link StringBuilders#escapeHex(StringBuilder, int, char, int)} is throwing an exception if a surrogate char is not used
	 * correctly.
	 */
	@Test(expected = IllegalStateException.class)
	public void testEscapeHexWithOnlyHighSurrogateInTheEndOfAString() {
		final StringBuilder stringBuilder = new StringBuilder("touché" + Character.toChars(0x10014)[0]);

		StringBuilders.escapeHex(stringBuilder, 0, stringBuilder.length(), null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
	}

	/**
	 * Tests to see if the method {@link StringBuilders#escapeHex(StringBuilder, int, char, int)} is throwing an exception if a surrogate char is not used
	 * correctly.
	 */
	@Test(expected = IllegalStateException.class)
	public void testEscapeHexWithOnlyLowSurrogate() {
		final StringBuilder stringBuilder = new StringBuilder(String.valueOf(Character.toChars(0x10014)[1]));

		StringBuilders.escapeHex(stringBuilder, 0, 1, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
	}

	/**
	 * Tests to see if the method {@link StringBuilders#escapeHex(StringBuilder, int, char, int)} is throwing an exception if a surrogate char is not used
	 * correctly.
	 */
	@Test(expected = IllegalStateException.class)
	public void testEscapeHexWithOnlyLowSurrogateInTheBegginingOfAString() {
		final StringBuilder stringBuilder = new StringBuilder(Character.toChars(0x10014)[1] + "touché");

		StringBuilders.escapeHex(stringBuilder, 0, stringBuilder.length(), null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
	}

	/**
	 * Tests to see if the method {@link StringBuilders#escapeHex(StringBuilder, int, char, int)} is throwing an exception if a surrogate char is not used
	 * correctly.
	 */
	@Test(expected = IllegalStateException.class)
	public void testEscapeHexWithOnlyLowSurrogateInTheEndOfAString() {
		final StringBuilder stringBuilder = new StringBuilder("touché" + Character.toChars(0x10014)[1]);

		StringBuilders.escapeHex(stringBuilder, 0, stringBuilder.length(), null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
	}

	/** Tests the method {@link StringBuilders#escapeHex(StringBuilder, int, char, int)} with a {@link StringBuilder} containing a single ASCII character. */
	@Test
	public void testEscapeHexWithSingleASCIICharacterString() {

		final StringBuilder stringBuilder = new StringBuilder("a");
		assertThat(StringBuilders.escapeHex(stringBuilder, 0, 1, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE).toString(), equalTo("a"));

	}

	/** Tests the method {@link StringBuilders#escapeHex(StringBuilder, int, char, int)} with a {@link StringBuilder} containing a single UTF-8 character */
	@Test
	public void testEscapeHexWithSingleCharacterString() {

		final StringBuilder stringBuilder = new StringBuilder("é");
		assertThat(StringBuilders.escapeHex(stringBuilder, 0, 1, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE).toString(), equalTo("&C3&A9"));

	}

	/** Tests the method {@link StringBuilders#escapeHex(StringBuilder, int, char, int)} with a {@link StringBuilder} containing a single surrogate pair. */
	@Test
	public void testEscapeHexWithSingleSurrogatePairString() {

		StringBuilder stringBuilder = new StringBuilder(String.valueOf(Character.toChars(0x10014)));
		final String escapedChar = StringBuilders.escapeHex(stringBuilder, 0, 2, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE).toString();
		assertThat(escapedChar, equalTo("&F0&90&80&94"));

	}

	/** Tests the method {@link StringBuilders#escapeHex(StringBuilder, int, char, int)} with a {@link StringBuilder} containing multiple ASCII characters. */
	@Test
	public void testEscapeHexWithMultipleASCIICharacterString() {

		StringBuilder stringBuilder = new StringBuilder("touch");
		StringBuilders.escapeHex(stringBuilder, 0, 1, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("t" + "ouch"));

		stringBuilder = new StringBuilder("touch");
		StringBuilders.escapeHex(stringBuilder, 1, 2, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("t" + "o" + "uch"));

		stringBuilder = new StringBuilder("touch");
		StringBuilders.escapeHex(stringBuilder, 2, 3, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("to" + "u" + "ch"));

		stringBuilder = new StringBuilder("touch");
		StringBuilders.escapeHex(stringBuilder, 3, 4, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("tou" + "c" + "h"));

		stringBuilder = new StringBuilder("touch");
		StringBuilders.escapeHex(stringBuilder, 4, 5, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("touc" + "h"));

		stringBuilder = new StringBuilder("touch");
		StringBuilders.escapeHex(stringBuilder, 0, 5, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("t" + "o" + "u" + "c" + "h"));
	}

	/** Tests the method {@link StringBuilders#escapeHex(StringBuilder, int, char, int)} with a {@link StringBuilder} containing multiple UTF-8 characters */
	@Test
	public void testEscapeHexWithMultipleCharacterString() {

		StringBuilder stringBuilder = new StringBuilder("touché");
		StringBuilders.escapeHex(stringBuilder, 0, 1, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("t" + "ouché"));

		stringBuilder = new StringBuilder("touché");
		StringBuilders.escapeHex(stringBuilder, 1, 2, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("t" + "o" + "uché"));

		stringBuilder = new StringBuilder("touché");
		StringBuilders.escapeHex(stringBuilder, 2, 3, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("to" + "u" + "ché"));

		stringBuilder = new StringBuilder("touché");
		StringBuilders.escapeHex(stringBuilder, 3, 4, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("tou" + "c" + "hé"));

		stringBuilder = new StringBuilder("touché");
		StringBuilders.escapeHex(stringBuilder, 4, 5, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("touc" + "h" + "é"));

		stringBuilder = new StringBuilder("touché");
		StringBuilders.escapeHex(stringBuilder, 5, 6, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("touch" + "&C3&A9"));

		stringBuilder = new StringBuilder("touché");
		StringBuilders.escapeHex(stringBuilder, 0, 6, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("t" + "o" + "u" + "c" + "h" + "&C3&A9"));
	}

	/**
	 * Tests the method {@link StringBuilders#escapeHex(StringBuilder, int, char, int)} with a {@link StringBuilder} containing multiple UTF-8 characters and a
	 * surrogate pair.
	 */
	@Test
	public void testEscapeHexWithMultipleCharacterStringWithSurrogatePair() {

		String surrogatePair = String.valueOf(Character.toChars(0x10014));

		StringBuilder stringBuilder = new StringBuilder("touché" + surrogatePair);
		StringBuilders.escapeHex(stringBuilder, 0, 1, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("t" + "ouché" + surrogatePair));

		stringBuilder = new StringBuilder("touché" + surrogatePair);
		StringBuilders.escapeHex(stringBuilder, 1, 2, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("t" + "o" + "uché" + surrogatePair));

		stringBuilder = new StringBuilder("touché" + surrogatePair);
		StringBuilders.escapeHex(stringBuilder, 2, 3, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("to" + "u" + "ché" + surrogatePair));

		stringBuilder = new StringBuilder("touché" + surrogatePair);
		StringBuilders.escapeHex(stringBuilder, 3, 4, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("tou" + "c" + "hé" + surrogatePair));

		stringBuilder = new StringBuilder("touché" + surrogatePair);
		StringBuilders.escapeHex(stringBuilder, 4, 5, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("touc" + "h" + "é" + surrogatePair));

		stringBuilder = new StringBuilder("touché" + surrogatePair);
		StringBuilders.escapeHex(stringBuilder, 5, 6, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("touch" + "&C3&A9" + surrogatePair));

		stringBuilder = new StringBuilder("touché" + surrogatePair);
		StringBuilders.escapeHex(stringBuilder, 6, 8, null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("touché" + "&F0&90&80&94"));

		stringBuilder = new StringBuilder("touché" + surrogatePair);
		StringBuilders.escapeHex(stringBuilder, 0, stringBuilder.length(), null, null, ASCII.MAX_VALUE, '&', 2, Case.UPPERCASE);
		assertThat(stringBuilder.toString(), equalTo("t" + "o" + "u" + "c" + "h" + "&C3&A9" + "&F0&90&80&94"));
	}

}
