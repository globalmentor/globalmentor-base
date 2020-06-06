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

import static com.globalmentor.io.Readers.*;
import static com.globalmentor.java.Characters.*;
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

	/** @see Readers#read(Reader, char[]) */
	@Test
	public void testRead() throws IOException {
		final Reader reader = new StringReader("foobar");
		final char[] buffer = new char[6]; //initializes to all zeros
		assertThat(Readers.read(reader, buffer), is(6)); //correct number of bytes read
		assertThat(buffer, is(new char[] {'f', 'o', 'o', 'b', 'a', 'r'})); //correct data read
		assertThat(readString(reader), is("")); //no data remaining
	}

	/** @see Readers#read(Reader, char[]) */
	@Test
	public void testReadEmptyReader() throws IOException {
		final Reader reader = new StringReader("");
		final char[] buffer = new char[6]; //initializes to all zeros
		assertThat(Readers.read(reader, buffer), is(0)); //correct number of bytes read
		assertThat(buffer, is(new char[] {0, 0, 0, 0, 0, 0})); //correct data read
		assertThat(readString(reader), is("")); //no data remaining
	}

	/** @see Readers#read(Reader, char[]) */
	@Test
	public void testReadEmptyBuffer() throws IOException {
		final Reader reader = new StringReader("foobar");
		final char[] buffer = new char[0]; //initializes to all zeros
		assertThat(Readers.read(reader, buffer), is(0)); //correct number of bytes read
		assertThat(buffer, is(NO_CHARS)); //no data read
		assertThat(readString(reader), is("foobar")); //correct data remaining
	}

	/** @see Readers#read(Reader, char[], int) */
	@Test
	public void testReadLength() throws IOException {
		final Reader reader = new StringReader("foobar");
		final char[] buffer = new char[8]; //initializes to all zeros
		assertThat(Readers.read(reader, buffer, 3), is(3)); //correct number of bytes read
		assertThat(buffer, is(new char[] {'f', 'o', 'o', 0, 0, 0, 0, 0})); //correct data read
		assertThat(readString(reader), is("bar")); //correct data still remaining
	}

	/** @see Readers#read(Reader, char[], int) */
	@Test
	public void testReadNegativeLengthThrowsException() throws IOException {
		final Reader reader = new StringReader("foobar");
		final char[] buffer = new char[8]; //initializes to all zeros
		assertThrows(IndexOutOfBoundsException.class, () -> Readers.read(reader, buffer, -1));
		assertThrows(IndexOutOfBoundsException.class, () -> Readers.read(reader, buffer, -2));
		assertThrows(IndexOutOfBoundsException.class, () -> Readers.read(reader, buffer, -123));
		assertThat(buffer, is(new char[] {0, 0, 0, 0, 0, 0, 0, 0})); //correct data read
		assertThat(readString(reader), is("foobar")); //correct data still remaining
	}

	/** @see Readers#read(Reader, char[], int, int) */
	@Test
	public void testReadWithRemainingData() throws IOException {
		final Reader reader = new StringReader("extraordinary");
		final char[] buffer = new char[10]; //initializes to all zeros
		assertThat(Readers.read(reader, buffer, 2, 5), is(5)); //correct number of bytes read
		assertThat(buffer, is(new char[] {0, 0, 'e', 'x', 't', 'r', 'a', 0, 0, 0})); //correct data read
		assertThat(readString(reader), is("ordinary")); //correct data still remaining
	}

	/** @see Readers#read(Reader, char[], int, int) */
	@Test
	public void testReadBufferExactLengthRequested() throws IOException {
		final Reader reader = new StringReader("foobar");
		final char[] buffer = new char[8]; //initializes to all zeros
		assertThat(Readers.read(reader, buffer, 1, 6), is(6)); //correct number of bytes read
		assertThat(buffer, is(new char[] {0, 'f', 'o', 'o', 'b', 'a', 'r', 0})); //correct data read
		assertThat(readString(reader), is("")); //no data remaining
	}

	/** @see Readers#read(Reader, char[], int, int) */
	@Test
	public void testReadRemainingBuffer() throws IOException {
		final Reader reader = new StringReader("foo");
		final char[] buffer = new char[8]; //initializes to all zeros
		assertThat(Readers.read(reader, buffer, 1, 3), is(3)); //correct number of bytes read
		assertThat(buffer, is(new char[] {0, 'f', 'o', 'o', 0, 0, 0, 0})); //correct data read
		assertThat(readString(reader), is("")); //no data remaining
	}

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
