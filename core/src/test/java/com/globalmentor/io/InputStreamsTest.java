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

import static com.globalmentor.io.InputStreams.*;
import static com.globalmentor.java.Bytes.*;
import static java.nio.charset.StandardCharsets.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

import org.junit.jupiter.api.Test;

/**
 * Tests of {@link InputStreams}.
 * @author Garret Wilson
 */
public class InputStreamsTest {

	/** @see InputStreams#read(InputStream, byte[]) */
	@Test
	public void testRead() throws IOException {
		final InputStream inputStream = new ByteArrayInputStream("foobar".getBytes(US_ASCII));
		final byte[] buffer = new byte[6]; //initializes to all zeros
		assertThat(InputStreams.read(inputStream, buffer), is(6)); //correct number of bytes read
		assertThat(buffer, is(new byte[] {'f', 'o', 'o', 'b', 'a', 'r'})); //correct data read
		assertThat(readBytes(inputStream), is(NO_BYTES)); //no data remaining
	}

	/** @see InputStreams#read(InputStream, byte[]) */
	@Test
	public void testReadEmptyStream() throws IOException {
		final InputStream inputStream = new EmptyInputStream();
		final byte[] buffer = new byte[6]; //initializes to all zeros
		assertThat(InputStreams.read(inputStream, buffer), is(0)); //correct number of bytes read
		assertThat(buffer, is(new byte[] {0, 0, 0, 0, 0, 0})); //correct data read
		assertThat(readBytes(inputStream), is(NO_BYTES)); //no data remaining
	}

	/** @see InputStreams#read(InputStream, byte[]) */
	@Test
	public void testReadEmptyBuffer() throws IOException {
		final InputStream inputStream = new ByteArrayInputStream("foobar".getBytes(US_ASCII));
		final byte[] buffer = new byte[0]; //initializes to all zeros
		assertThat(InputStreams.read(inputStream, buffer), is(0)); //correct number of bytes read
		assertThat(buffer, is(NO_BYTES)); //no data read
		assertThat(readBytes(inputStream), is(new byte[] {'f', 'o', 'o', 'b', 'a', 'r'})); //correct data remaining
	}

	/** @see InputStreams#read(InputStream, byte[], int) */
	@Test
	public void testReadLength() throws IOException {
		final InputStream inputStream = new ByteArrayInputStream("foobar".getBytes(US_ASCII));
		final byte[] buffer = new byte[8]; //initializes to all zeros
		assertThat(InputStreams.read(inputStream, buffer, 3), is(3)); //correct number of bytes read
		assertThat(buffer, is(new byte[] {'f', 'o', 'o', 0, 0, 0, 0, 0})); //correct data read
		assertThat(readBytes(inputStream), is("bar".getBytes(US_ASCII))); //correct data still remaining
	}

	/** @see InputStreams#read(InputStream, byte[], int) */
	@Test
	public void testReadNegativeLengthThrowsException() throws IOException {
		final InputStream inputStream = new ByteArrayInputStream("foobar".getBytes(US_ASCII));
		final byte[] buffer = new byte[8]; //initializes to all zeros
		assertThrows(IndexOutOfBoundsException.class, () -> InputStreams.read(inputStream, buffer, -1));
		assertThrows(IndexOutOfBoundsException.class, () -> InputStreams.read(inputStream, buffer, -2));
		assertThrows(IndexOutOfBoundsException.class, () -> InputStreams.read(inputStream, buffer, -123));
		assertThat(buffer, is(new byte[] {0, 0, 0, 0, 0, 0, 0, 0})); //correct data read
		assertThat(readBytes(inputStream), is("foobar".getBytes(US_ASCII))); //correct data still remaining
	}

	/** @see InputStreams#read(InputStream, byte[], int, int) */
	@Test
	public void testReadWithRemainingData() throws IOException {
		final InputStream inputStream = new ByteArrayInputStream("extraordinary".getBytes(US_ASCII));
		final byte[] buffer = new byte[10]; //initializes to all zeros
		assertThat(InputStreams.read(inputStream, buffer, 2, 5), is(5)); //correct number of bytes read
		assertThat(buffer, is(new byte[] {0, 0, 'e', 'x', 't', 'r', 'a', 0, 0, 0})); //correct data read
		assertThat(readBytes(inputStream), is("ordinary".getBytes(US_ASCII))); //correct data still remaining
	}

	/** @see InputStreams#read(InputStream, byte[], int, int) */
	@Test
	public void testReadBufferExactLengthRequested() throws IOException {
		final InputStream inputStream = new ByteArrayInputStream("foobar".getBytes(US_ASCII));
		final byte[] buffer = new byte[8]; //initializes to all zeros
		assertThat(InputStreams.read(inputStream, buffer, 1, 6), is(6)); //correct number of bytes read
		assertThat(buffer, is(new byte[] {0, 'f', 'o', 'o', 'b', 'a', 'r', 0})); //correct data read
		assertThat(readBytes(inputStream), is(NO_BYTES)); //no data remaining
	}

	/** @see InputStreams#read(InputStream, byte[], int, int) */
	@Test
	public void testReadRemainingBuffer() throws IOException {
		final InputStream inputStream = new ByteArrayInputStream("foo".getBytes(US_ASCII));
		final byte[] buffer = new byte[8]; //initializes to all zeros
		assertThat(InputStreams.read(inputStream, buffer, 1, 3), is(3)); //correct number of bytes read
		assertThat(buffer, is(new byte[] {0, 'f', 'o', 'o', 0, 0, 0, 0})); //correct data read
		assertThat(readBytes(inputStream), is(NO_BYTES)); //no data remaining
	}

}
