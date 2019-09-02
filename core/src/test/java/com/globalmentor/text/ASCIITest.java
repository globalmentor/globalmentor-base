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
import org.junit.jupiter.api.*;

/**
 * Tests of ASCII utilities.
 * @author Garret Wilson
 * @see ASCII
 */
public class ASCIITest {

	/** @see ASCII#equalsIgnoreCase(CharSequence, CharSequence) */
	@Test
	public void testEqualsIgnoreCaseTwoStrings() {
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

}
