/*
 * Copyright © 2020 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.io;

import static com.globalmentor.io.InputStreams.*;
import static com.globalmentor.java.Bytes.*;
import static java.nio.charset.StandardCharsets.*;
import static java.util.Arrays.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.io.*;
import java.util.Random;

import org.junit.jupiter.api.Test;

import com.globalmentor.java.*;

/**
 * Tests of {@link InputStreams}.
 * @author Garret Wilson
 */
public class InputStreamsTest {

	/** @see InputStreams#readBytes(InputStream) */
	@SuppressWarnings("removal")
	@Test
	public void testReadBytes() throws IOException {
		final Random random = new Random(20201128);
		for(final int length : asList(0, 1, 100, INITIAL_READ_BUFFER_SIZE - 1, INITIAL_READ_BUFFER_SIZE + 1,
				//MAX_READ_BUFFER_SIZE * 4 (4194304) is more than twice the total (2088960) needed to reach the maximum buffer (1048576) doubling from the initial (8192)
				INITIAL_READ_BUFFER_SIZE * 2 - 1, INITIAL_READ_BUFFER_SIZE * 2, INITIAL_READ_BUFFER_SIZE * 2 + 1, MAX_READ_BUFFER_SIZE * 4)) {
			final byte[] bytes = Bytes.generateRandom(length, random);
			try (final InputStream inputStream = new ByteArrayInputStream(bytes)) {
				assertThat("Read all %d bytes.".formatted(length), InputStreams.readBytes(inputStream), is(bytes));
			}
		}
	}

	/** @see InputStreams#read(InputStream, byte[]) */
	@SuppressWarnings("removal")
	@Test
	public void testRead() throws IOException {
		final InputStream inputStream = new ByteArrayInputStream("foobar".getBytes(US_ASCII));
		final byte[] buffer = new byte[6]; //initializes to all zeros
		assertThat(InputStreams.read(inputStream, buffer), is(6)); //correct number of bytes read
		assertThat(buffer, is(new byte[] {'f', 'o', 'o', 'b', 'a', 'r'})); //correct data read
		assertThat(readBytes(inputStream), is(NO_BYTES)); //no data remaining
	}

	/** @see InputStreams#read(InputStream, byte[]) */
	@SuppressWarnings("removal")
	@Test
	public void testReadEmptyStream() throws IOException {
		final InputStream inputStream = new EmptyInputStream();
		final byte[] buffer = new byte[6]; //initializes to all zeros
		assertThat(InputStreams.read(inputStream, buffer), is(0)); //correct number of bytes read
		assertThat(buffer, is(new byte[] {0, 0, 0, 0, 0, 0})); //correct data read
		assertThat(readBytes(inputStream), is(NO_BYTES)); //no data remaining
	}

	/** @see InputStreams#read(InputStream, byte[]) */
	@SuppressWarnings("removal")
	@Test
	public void testReadEmptyBuffer() throws IOException {
		final InputStream inputStream = new ByteArrayInputStream("foobar".getBytes(US_ASCII));
		final byte[] buffer = new byte[0]; //initializes to all zeros
		assertThat(InputStreams.read(inputStream, buffer), is(0)); //correct number of bytes read
		assertThat(buffer, is(NO_BYTES)); //no data read
		assertThat(readBytes(inputStream), is(new byte[] {'f', 'o', 'o', 'b', 'a', 'r'})); //correct data remaining
	}

	/** @see InputStreams#read(InputStream, byte[], int) */
	@SuppressWarnings("removal")
	@Test
	public void testReadLength() throws IOException {
		final InputStream inputStream = new ByteArrayInputStream("foobar".getBytes(US_ASCII));
		final byte[] buffer = new byte[8]; //initializes to all zeros
		assertThat(InputStreams.read(inputStream, buffer, 3), is(3)); //correct number of bytes read
		assertThat(buffer, is(new byte[] {'f', 'o', 'o', 0, 0, 0, 0, 0})); //correct data read
		assertThat(readBytes(inputStream), is("bar".getBytes(US_ASCII))); //correct data still remaining
	}

	/** @see InputStreams#read(InputStream, byte[], int) */
	@SuppressWarnings("removal")
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
	@SuppressWarnings("removal")
	@Test
	public void testReadWithRemainingData() throws IOException {
		final InputStream inputStream = new ByteArrayInputStream("extraordinary".getBytes(US_ASCII));
		final byte[] buffer = new byte[10]; //initializes to all zeros
		assertThat(InputStreams.read(inputStream, buffer, 2, 5), is(5)); //correct number of bytes read
		assertThat(buffer, is(new byte[] {0, 0, 'e', 'x', 't', 'r', 'a', 0, 0, 0})); //correct data read
		assertThat(readBytes(inputStream), is("ordinary".getBytes(US_ASCII))); //correct data still remaining
	}

	/** @see InputStreams#read(InputStream, byte[], int, int) */
	@SuppressWarnings("removal")
	@Test
	public void testReadBufferExactLengthRequested() throws IOException {
		final InputStream inputStream = new ByteArrayInputStream("foobar".getBytes(US_ASCII));
		final byte[] buffer = new byte[8]; //initializes to all zeros
		assertThat(InputStreams.read(inputStream, buffer, 1, 6), is(6)); //correct number of bytes read
		assertThat(buffer, is(new byte[] {0, 'f', 'o', 'o', 'b', 'a', 'r', 0})); //correct data read
		assertThat(readBytes(inputStream), is(NO_BYTES)); //no data remaining
	}

	/** @see InputStreams#read(InputStream, byte[], int, int) */
	@SuppressWarnings("removal")
	@Test
	public void testReadRemainingBuffer() throws IOException {
		final InputStream inputStream = new ByteArrayInputStream("foo".getBytes(US_ASCII));
		final byte[] buffer = new byte[8]; //initializes to all zeros
		assertThat(InputStreams.read(inputStream, buffer, 1, 3), is(3)); //correct number of bytes read
		assertThat(buffer, is(new byte[] {0, 'f', 'o', 'o', 0, 0, 0, 0})); //correct data read
		assertThat(readBytes(inputStream), is(NO_BYTES)); //no data remaining
	}

	//## readByteSequence

	/** Tests for {@link InputStreams#readByteSequence(InputStream)}. */
	@Test
	void testReadByteSequence() throws IOException {
		final byte[] data = {0x01, 0x02, 0x03, 0x04, 0x05};
		try (final InputStream inputStream = new ByteArrayInputStream(data)) {
			final ByteSequence result = InputStreams.readByteSequence(inputStream);
			assertThat("readByteSequence returns correct content", result.toByteArray(), is(data));
			assertThat("readByteSequence returns correct length", result.length(), is(5));
		}
	}

	/** Tests that {@link InputStreams#readByteSequence(InputStream)} returns empty singleton for empty stream. */
	@Test
	void testReadByteSequenceEmpty() throws IOException {
		try (final InputStream inputStream = InputStream.nullInputStream()) {
			final ByteSequence result = InputStreams.readByteSequence(inputStream);
			assertThat("readByteSequence of empty stream returns empty singleton", result, is(sameInstance(ByteSequence.empty())));
		}
	}

	/** Tests that {@link InputStreams#readByteSequence(InputStream)} reads all bytes from a large stream. */
	@Test
	void testReadByteSequenceLarge() throws IOException {
		final byte[] data = Bytes.generateRandom(100_000, new Random(20251207));
		try (final InputStream inputStream = new ByteArrayInputStream(data)) {
			final ByteSequence result = InputStreams.readByteSequence(inputStream);
			assertThat("readByteSequence handles large data", result.toByteArray(), is(data));
		}
	}

	/** Tests for {@link InputStreams#readByteSequence(InputStream, int)}. */
	@Test
	void testReadByteSequenceWithLength() throws IOException {
		final byte[] data = {0x01, 0x02, 0x03, 0x04, 0x05};
		try (final InputStream inputStream = new ByteArrayInputStream(data)) {
			final ByteSequence result = InputStreams.readByteSequence(inputStream, 3);
			assertThat("readByteSequence with length returns correct content", result.toByteArray(), is(new byte[] {0x01, 0x02, 0x03}));
			assertThat("readByteSequence with length returns correct length", result.length(), is(3));
			assertThat("remaining bytes still available", inputStream.readAllBytes(), is(new byte[] {0x04, 0x05}));
		}
	}

	/** Tests that {@link InputStreams#readByteSequence(InputStream, int)} returns fewer bytes at end-of-stream. */
	@Test
	void testReadByteSequenceWithLengthAtEndOfStream() throws IOException {
		final byte[] data = {0x01, 0x02, 0x03};
		try (final InputStream inputStream = new ByteArrayInputStream(data)) {
			final ByteSequence result = InputStreams.readByteSequence(inputStream, 10); // request more than available
			assertThat("readByteSequence returns available bytes at EOF", result.toByteArray(), is(data));
			assertThat("readByteSequence returns actual length at EOF", result.length(), is(3));
		}
	}

	/** Tests that {@link InputStreams#readByteSequence(InputStream, int)} with zero length returns empty singleton. */
	@Test
	void testReadByteSequenceWithZeroLength() throws IOException {
		final byte[] data = {0x01, 0x02, 0x03};
		try (final InputStream inputStream = new ByteArrayInputStream(data)) {
			final ByteSequence result = InputStreams.readByteSequence(inputStream, 0);
			assertThat("readByteSequence with zero length returns empty singleton", result, is(sameInstance(ByteSequence.empty())));
			assertThat("all bytes still available after zero-length read", inputStream.readAllBytes(), is(data));
		}
	}

	/** Tests that {@link InputStreams#readByteSequence(InputStream, int)} throws for negative length. */
	@Test
	void testReadByteSequenceWithNegativeLengthThrows() throws IOException {
		try (final InputStream inputStream = new ByteArrayInputStream(new byte[] {0x01, 0x02})) {
			assertThrows(IllegalArgumentException.class, () -> InputStreams.readByteSequence(inputStream, -1), "negative length");
		}
	}

}
