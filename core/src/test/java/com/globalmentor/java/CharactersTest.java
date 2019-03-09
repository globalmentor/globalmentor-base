/*
 * Copyright © 2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.java.Characters.*;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.util.Arrays;

import org.junit.Test;

/**
 * Tests for {@link Characters}.
 * 
 * @author Garret Wilson
 * 
 */
public class CharactersTest {

	/** Tests the {@link Characters#split(CharSequence)} method. */
	@Test
	public void testSplit() {
		assertThat(WHITESPACE_CHARACTERS.split("abc"), is(Arrays.<CharSequence> asList("abc")));
		assertThat(WHITESPACE_CHARACTERS.split(" abc"), is(Arrays.<CharSequence> asList("abc")));
		assertThat(WHITESPACE_CHARACTERS.split("abc "), is(Arrays.<CharSequence> asList("abc")));
		assertThat(WHITESPACE_CHARACTERS.split(" abc "), is(Arrays.<CharSequence> asList("abc")));
		assertThat(WHITESPACE_CHARACTERS.split("  abc  "), is(Arrays.<CharSequence> asList("abc")));
		assertThat(WHITESPACE_CHARACTERS.split(" \tabc \t"), is(Arrays.<CharSequence> asList("abc")));
		assertThat(WHITESPACE_CHARACTERS.split("abc def"), is(Arrays.<CharSequence> asList("abc", "def")));
		assertThat(WHITESPACE_CHARACTERS.split("  abc  def  "), is(Arrays.<CharSequence> asList("abc", "def")));
		assertThat(WHITESPACE_CHARACTERS.split("a b c d e f"), is(Arrays.<CharSequence> asList("a", "b", "c", "d", "e", "f")));
		assertThat(WHITESPACE_CHARACTERS.split("\r\na\tb\tc\td\te\tf\r\n"), is(Arrays.<CharSequence> asList("a", "b", "c", "d", "e", "f")));
	}

}
