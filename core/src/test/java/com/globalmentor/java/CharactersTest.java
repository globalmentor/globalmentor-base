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
		assertThat("NULL is replaced with symbol.", Characters.toDisplay((char)0x00), is('␀'));
		assertThat("BEL C0 control is replaced with symbol.", Characters.toDisplay((char)0x07), is('␇'));
		assertThat("LF C0 control is replaced with symbol.", Characters.toDisplay((char)0x0A), is('␊'));
		assertThat("CR C0 control is replaced with symbol.", Characters.toDisplay((char)0x0D), is('␍'));
		assertThat("UNIT SEPARATOR C0 control is replaced with symbol.", Characters.toDisplay((char)0x1F), is('␟'));
		assertThat("Space is not replaced.", Characters.toDisplay(' '), is(' '));
		assertThat("DELETE is replaced with symbol.", Characters.toDisplay((char)0x7F), is('␡'));
		assertThat("Latin letter A is not replaced.", Characters.toDisplay('A'), is('A'));
		assertThat("Latin letter x is not replaced.", Characters.toDisplay('x'), is('x'));
		assertThat("Latin letter é is not replaced.", Characters.toDisplay('é'), is('é'));
		assertThat("Non-control character 0xFF is not replaced.", Characters.toDisplay((char)0xFF), is((char)0xFF));
		assertThat("Trademark symbol is not replaced.", Characters.toDisplay('™'), is('™'));
		assertThat("C1 control character is not replaced.", Characters.toDisplay((char)0x85), is((char)0x85));
	}

	/** @see Characters#appendLabel(StringBuilder, int) */
	@Test
	void testAppendLabel() {
		assertThat("Tab control character uses escape sequence.", Characters.appendLabel(new StringBuilder(), '\t'), hasToString("'\\t'"));
		assertThat("CR control character uses escape sequence.", Characters.appendLabel(new StringBuilder(), '\r'), hasToString("'\\r'"));
		assertThat("LF control character uses escape sequence.", Characters.appendLabel(new StringBuilder(), '\n'), hasToString("'\\n'"));
		assertThat("NULL control character uses Unicode code point.", Characters.appendLabel(new StringBuilder(), 0x00), hasToString("U+0000"));
		assertThat("BEL C0 control uses Unicode code point.", Characters.appendLabel(new StringBuilder(), 0x07), hasToString("U+0007"));
		assertThat("DELETE control uses Unicode code point.", Characters.appendLabel(new StringBuilder(), 0x7F), hasToString("U+007F"));
		assertThat("NEL C1 control uses Unicode code point.", Characters.appendLabel(new StringBuilder(), 0x85), hasToString("U+0085"));
		assertThat("High surrogate uses Unicode code point.", Characters.appendLabel(new StringBuilder(), 0xD800), hasToString("U+D800"));
		assertThat("Low surrogate uses Unicode code point.", Characters.appendLabel(new StringBuilder(), 0xDFFF), hasToString("U+DFFF"));
		assertThat("Regular ASCII character is quoted.", Characters.appendLabel(new StringBuilder(), 'A'), hasToString("'A'"));
		assertThat("Space is quoted.", Characters.appendLabel(new StringBuilder(), ' '), hasToString("' '"));
		assertThat("Latin letter with diacritic is quoted.", Characters.appendLabel(new StringBuilder(), 'é'), hasToString("'é'"));
		assertThat("Trademark symbol is quoted.", Characters.appendLabel(new StringBuilder(), '™'), hasToString("'™'"));
		assertThat("Supplementary code point is quoted.", Characters.appendLabel(new StringBuilder(), 0x1F602), hasToString("'😂'")); //face with tears of joy emoji
	}

	/** @see Characters#toLabel(int) */
	@Test
	void testToLabel() {
		assertThat("Tab control character uses escape sequence.", Characters.toLabel('\t'), is("'\\t'"));
		assertThat("Regular ASCII character is quoted.", Characters.toLabel('x'), is("'x'"));
		assertThat("Supplementary code point is quoted.", Characters.toLabel(0x1F602), is("'😂'")); //face with tears of joy emoji
	}

	/** @see Characters#getLabel(int) */
	@SuppressWarnings("removal")
	@Test
	void testGetLabel() {
		assertThat("Deprecated method delegates correctly.", Characters.getLabel('A'), is("'A'"));
	}

	/** @see Characters#appendLabelArrayString(StringBuilder, char[]) */
	@Test
	void testAppendLabelArrayString() {
		assertThat("Empty array produces empty brackets.", Characters.appendLabelArrayString(new StringBuilder(), new char[] {}), hasToString("[]"));
		assertThat("Single character is bracketed.", Characters.appendLabelArrayString(new StringBuilder(), new char[] {'a'}), hasToString("['a']"));
		assertThat("Multiple regular characters are comma-separated.", Characters.appendLabelArrayString(new StringBuilder(), new char[] {'a', 'b', 'c'}),
				hasToString("['a', 'b', 'c']"));
		assertThat("Control characters use appropriate format.", Characters.appendLabelArrayString(new StringBuilder(), new char[] {'a', '\t', '\n'}),
				hasToString("['a', '\\t', '\\n']"));
		assertThat("Mixed character types are formatted appropriately.",
				Characters.appendLabelArrayString(new StringBuilder(), new char[] {'a', ' ', '\t', 'é', '™'}), hasToString("['a', ' ', '\\t', 'é', '™']"));
	}

	/** @see Characters#toLabelArrayString(char...) */
	@Test
	void testToLabelArrayStringCharArray() {
		assertThat("Empty array produces empty brackets.", Characters.toLabelArrayString(new char[] {}), is("[]"));
		assertThat("Multiple characters are formatted.", Characters.toLabelArrayString(new char[] {'x', 'y', 'z'}), is("['x', 'y', 'z']"));
	}

	/** @see Characters#toLabelArrayString(CharSequence) */
	@Test
	void testToLabelArrayStringCharSequence() {
		assertThat("String is converted to array format.", Characters.toLabelArrayString("abc"), is("['a', 'b', 'c']"));
	}

	/** @see Characters#toLabelArrayString() */
	@Test
	void testToLabelArrayStringInstance() {
		assertThat("Instance method formats characters.", Characters.of('x', 'y', 'z').toLabelArrayString(), is("['x', 'y', 'z']"));
	}

}
