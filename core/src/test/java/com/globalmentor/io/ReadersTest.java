/*
 * Copyright © 2020 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.io;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.io.*;

import org.junit.jupiter.api.Test;

/**
 * Tests of {@link Readers}.
 * @author Garret Wilson
 */
public class ReadersTest {

	/** @see Readers#readString(Reader), int) */
	@Test
	public void testReadString() throws IOException {
		assertThat(Readers.readString(new StringReader(""), 0), is(""));
		assertThat(Readers.readString(new StringReader(""), 1), is(""));
		assertThat(Readers.readString(new StringReader(""), 2), is(""));
		assertThat(Readers.readString(new StringReader(""), Integer.MAX_VALUE), is(""));
		assertThat(Readers.readString(new StringReader("x"), 0), is(""));
		assertThat(Readers.readString(new StringReader("x"), 1), is("x"));
		assertThat(Readers.readString(new StringReader("x"), 2), is("x"));
		assertThat(Readers.readString(new StringReader("x"), 3), is("x"));
		assertThat(Readers.readString(new StringReader("x"), Integer.MAX_VALUE), is("x"));
		assertThat(Readers.readString(new StringReader("abc"), 0), is(""));
		assertThat(Readers.readString(new StringReader("abc"), 1), is("a"));
		assertThat(Readers.readString(new StringReader("abc"), 2), is("ab"));
		assertThat(Readers.readString(new StringReader("abc"), 3), is("abc"));
		assertThat(Readers.readString(new StringReader("abc"), 4), is("abc"));
		assertThat(Readers.readString(new StringReader("abc"), Integer.MAX_VALUE), is("abc"));
		assertThat(Readers.readString(new StringReader("foo bar"), 0), is(""));
		assertThat(Readers.readString(new StringReader("foo bar"), 1), is("f"));
		assertThat(Readers.readString(new StringReader("foo bar"), 2), is("fo"));
		assertThat(Readers.readString(new StringReader("foo bar"), 3), is("foo"));
		assertThat(Readers.readString(new StringReader("foo bar"), 4), is("foo "));
		assertThat(Readers.readString(new StringReader("foo bar"), 5), is("foo b"));
		assertThat(Readers.readString(new StringReader("foo bar"), 100), is("foo bar"));
		assertThat(Readers.readString(new StringReader("foo bar"), Integer.MAX_VALUE), is("foo bar"));
		assertThrows(IllegalArgumentException.class, () -> Readers.readString(new StringReader(""), -1));
		assertThrows(IllegalArgumentException.class, () -> Readers.readString(new StringReader(""), -123));
		assertThrows(IllegalArgumentException.class, () -> Readers.readString(new StringReader(""), Integer.MIN_VALUE));
		assertThrows(IllegalArgumentException.class, () -> Readers.readString(new StringReader("abc"), -1));
		assertThrows(IllegalArgumentException.class, () -> Readers.readString(new StringReader("abc"), -123));
		assertThrows(IllegalArgumentException.class, () -> Readers.readString(new StringReader("abc"), Integer.MIN_VALUE));
	}

}
