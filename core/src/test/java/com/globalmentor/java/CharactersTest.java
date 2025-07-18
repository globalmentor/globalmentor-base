/*
 * Copyright © 2012 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static com.globalmentor.java.Characters.*;
import static java.nio.charset.StandardCharsets.*;
import static java.util.Arrays.*;
import static org.hamcrest.Matchers.*;

import static org.hamcrest.MatcherAssert.*;

import org.junit.jupiter.api.Test;

/**
 * Tests for {@link Characters}.
 * 
 * @author Garret Wilson
 * 
 */
public class CharactersTest {

	/** Tests the {@link Characters#split(CharSequence)} method. */
	@Test
	void testSplit() {
		assertThat(WHITESPACE_CHARACTERS.split("abc"), is(asList("abc")));
		assertThat(WHITESPACE_CHARACTERS.split(" abc"), is(asList("abc")));
		assertThat(WHITESPACE_CHARACTERS.split("abc "), is(asList("abc")));
		assertThat(WHITESPACE_CHARACTERS.split(" abc "), is(asList("abc")));
		assertThat(WHITESPACE_CHARACTERS.split("  abc  "), is(asList("abc")));
		assertThat(WHITESPACE_CHARACTERS.split(" \tabc \t"), is(asList("abc")));
		assertThat(WHITESPACE_CHARACTERS.split("abc def"), is(asList("abc", "def")));
		assertThat(WHITESPACE_CHARACTERS.split("  abc  def  "), is(asList("abc", "def")));
		assertThat(WHITESPACE_CHARACTERS.split("a b c d e f"), is(asList("a", "b", "c", "d", "e", "f")));
		assertThat(WHITESPACE_CHARACTERS.split("\r\na\tb\tc\td\te\tf\r\n"), is(asList("a", "b", "c", "d", "e", "f")));
	}

	/** @see Characters#isContinuousSequence(char...) */
	@Test
	void testIsContinuousSequence() {
		assertThat(Characters.isContinuousSequence(NO_CHARS), is(true));
		assertThat(Characters.isContinuousSequence('a'), is(true));
		assertThat(Characters.isContinuousSequence('a', 'b'), is(true));
		assertThat(Characters.isContinuousSequence('a', 'b', 'c'), is(true));
		assertThat(Characters.isContinuousSequence('a', 'c'), is(false));
		assertThat(Characters.isContinuousSequence('c', 'b', 'a'), is(false));
		assertThat(Characters.isContinuousSequence('a', 'b', 'a'), is(false));
		assertThat(Characters.isContinuousSequence('a', 'b', 'b'), is(false));
		assertThat(Characters.isContinuousSequence('a', 'b', 'c', 'd'), is(true));
		assertThat(Characters.isContinuousSequence('a', '1', '2', 'x'), is(false));
		assertThat(Characters.isContinuousSequence('a', 'z'), is(false));
	}

	/** @see Characters#toByteArray(char[]) */
	@Test
	void testToByteArray() {
		assertThat(Characters.toByteArray(new char[] {'t', 'o', 'u', 'c', 'h', 'é', '™'}, UTF_8), is("touché™".getBytes(UTF_8)));
	}

	/** @see Characters#appendUnicodeCodePointLabel(StringBuilder, int) */
	@Test
	void testAppendUnicodeCodePointLabel() {
		assertThat(Characters.appendUnicodeCodePointLabel(new StringBuilder(), 0x00), hasToString("U+0000"));
		assertThat(Characters.appendUnicodeCodePointLabel(new StringBuilder(), 'A'), hasToString("U+0041"));
		assertThat(Characters.appendUnicodeCodePointLabel(new StringBuilder(), 'x'), hasToString("U+0078"));
		assertThat(Characters.appendUnicodeCodePointLabel(new StringBuilder(), 'é'), hasToString("U+00E9"));
		assertThat(Characters.appendUnicodeCodePointLabel(new StringBuilder(), 0xFF), hasToString("U+00FF"));
		assertThat(Characters.appendUnicodeCodePointLabel(new StringBuilder(), '™'), hasToString("U+2122"));
		assertThat(Characters.appendUnicodeCodePointLabel(new StringBuilder(), 0x1F602), hasToString("U+01F602")); //face with tears of joy emoji `😂`
	}

	/** @see Characters#toDisplay(char) */
	@Test
	void testToDisplay() {
		assertThat(Characters.toDisplay((char)0x00), is('␀'));
		assertThat(Characters.toDisplay((char)0x07), is('␇'));
		assertThat(Characters.toDisplay((char)0x0A), is('␊'));
		assertThat(Characters.toDisplay((char)0x0D), is('␍'));
		assertThat(Characters.toDisplay((char)0x1F), is('␟'));
		assertThat(Characters.toDisplay(' '), is(' '));
		assertThat(Characters.toDisplay((char)0x7F), is('␡'));
		assertThat(Characters.toDisplay('A'), is('A'));
		assertThat(Characters.toDisplay('x'), is('x'));
		assertThat(Characters.toDisplay('é'), is('é'));
		assertThat(Characters.toDisplay((char)0xFF), is((char)0xFF));
		assertThat(Characters.toDisplay('™'), is('™'));
	}

}
