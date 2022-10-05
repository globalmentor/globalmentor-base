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

	/** @see Characters#isContinuousSequence(char...)) */
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

}
