/*
 * Copyright © 2022-2025 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

/**
 * Tests of {@link Bytes}.
 * @author Garret Wilson
 */
public class BytesTest {

	/** Tests for {@link Bytes#asByteSequence(byte[])}. */
	@Test
	void testAsByteSequence() {
		final byte[] data = {0x01, 0x02, 0x03};
		final ByteSequence bs = Bytes.asByteSequence(data);
		assertThat("length matches array length", bs.length(), is(3));
		assertThat("byteAt returns correct values", bs.byteAt(0), is((byte)0x01));
		assertThat("byteAt returns correct values", bs.byteAt(1), is((byte)0x02));
		assertThat("byteAt returns correct values", bs.byteAt(2), is((byte)0x03));
	}

	/** Tests that {@link Bytes#asByteSequence(byte[])} returns the empty singleton for an empty array. */
	@Test
	void testAsByteSequenceEmptyArray() {
		assertThat("empty array returns empty singleton", Bytes.asByteSequence(new byte[0]), is(sameInstance(ByteSequence.empty())));
	}

	/** Tests that {@link Bytes#asByteSequence(byte[])} throws for null. */
	@Test
	void testAsByteSequenceNullThrows() {
		assertThrows(NullPointerException.class, () -> Bytes.asByteSequence(null), "null array should throw");
	}

	/** Tests for {@link Bytes#startsWith(byte[], byte[])}. */
	@SuppressWarnings("removal")
	@Test
	void testStartsWith() {
		assertThat("starts with prefix", Bytes.startsWith(new byte[] {0x01, 0x02, 0x03}, new byte[] {0x01, 0x02}), is(true));
		assertThat("starts with self", Bytes.startsWith(new byte[] {0x01, 0x02}, new byte[] {0x01, 0x02}), is(true));
		assertThat("starts with empty", Bytes.startsWith(new byte[] {0x01, 0x02}, new byte[0]), is(true));
		assertThat("does not start with different", Bytes.startsWith(new byte[] {0x01, 0x02, 0x03}, new byte[] {0x02, 0x03}), is(false));
		assertThat("does not start with longer", Bytes.startsWith(new byte[] {0x01, 0x02}, new byte[] {0x01, 0x02, 0x03}), is(false));
	}

	/** Tests for {@link Bytes#generateRandom(int)}. */
	@Test
	void testGenerateRandom() {
		final byte[] random = Bytes.generateRandom(10);
		assertThat("generated array has correct length", random.length, is(10));
	}

	/** Tests that {@link Bytes#generateRandom(int)} throws for negative length. */
	@Test
	void testGenerateRandomNegativeThrows() {
		assertThrows(IllegalArgumentException.class, () -> Bytes.generateRandom(-1), "negative length should throw");
	}

	/** Tests for zero-length {@link Bytes#generateRandom(int)}. */
	@Test
	void testGenerateRandomZeroLength() {
		final byte[] random = Bytes.generateRandom(0);
		assertThat("zero length generates empty array", random.length, is(0));
	}

}
