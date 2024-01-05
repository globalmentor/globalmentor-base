/*
 * Copyright © 2022 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import java.nio.ByteBuffer;
import java.util.stream.LongStream;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link Longs}
 * @author Garret Wilson
 */
public class LongsTest {

	private final long LONG_SEQUENCE_BYTES = ByteBuffer.wrap(new byte[] {1, 2, 3, 4, 5, 6, 7, 8}).getLong();

	private final long LONG_UPPER_BYTES = ByteBuffer
			.wrap(new byte[] {(byte)0x80, (byte)0x91, (byte)0xA2, (byte)0xB3, (byte)0xC4, (byte)0xD5, (byte)0xE6, (byte)0xF7}).getLong();

	private final long LONG_MIXED_BYTES = ByteBuffer.wrap(new byte[] {0, (byte)0x91, 2, (byte)0xB3, 4, (byte)0xD5, 6, (byte)0xF7}).getLong();

	/**
	 * Returns a stream of diverse <code>long</code> values for testing. The stream will always contain the same values.
	 * @return A stream of test <code>long</code> values.
	 */
	LongStream testLongs() {
		return LongStream.of(0L, 1L, -1L, 12345678910L, -12345678910L, LONG_SEQUENCE_BYTES, -LONG_MIXED_BYTES, LONG_UPPER_BYTES, -LONG_UPPER_BYTES,
				LONG_MIXED_BYTES, -LONG_MIXED_BYTES, Long.MAX_VALUE, Long.MIN_VALUE);
	}

	/** @see Longs#fromBytes(byte[]) */
	@Test
	void testFromBytesArray() {
		testLongs().forEach(value -> {
			assertThat(Longs.fromBytes(ByteBuffer.allocate(Long.BYTES).putLong(value).array()), is(value));
			assertThrows(IllegalArgumentException.class, () -> Longs.fromBytes(ByteBuffer.allocate(Long.BYTES + 1).putLong(value).put((byte)123).array()));
			assertThrows(IllegalArgumentException.class,
					() -> Longs.fromBytes(ByteBuffer.allocate(Long.BYTES + 2).putLong(value).put((byte)0x12).put((byte)0xFF).array()));
		});
	}

	/** @see Longs#fromBytes(byte[], int) */
	@Test
	void testFromBytesArrayIndex() {
		testLongs().forEach(value -> {
			assertThat(Longs.fromBytes(ByteBuffer.allocate(Long.BYTES).putLong(value).array(), 0), is(value));
			assertThat(Longs.fromBytes(ByteBuffer.allocate(Long.BYTES + 1).putLong(value).put((byte)123).array(), 0), is(value));
			assertThat(Longs.fromBytes(ByteBuffer.allocate(Long.BYTES + 1).put((byte)123).putLong(value).array(), 1), is(value));
			assertThat(Longs.fromBytes(ByteBuffer.allocate(Long.BYTES + 2).putLong(value).put((byte)0x12).put((byte)0xFF).array(), 0), is(value));
			assertThat(Longs.fromBytes(ByteBuffer.allocate(Long.BYTES + 2).put((byte)0xFF).putLong(value).put((byte)0x12).array(), 1), is(value));
			assertThat(Longs.fromBytes(ByteBuffer.allocate(Long.BYTES + 2).put((byte)0x12).put((byte)0xFF).putLong(value).array(), 2), is(value));
		});
	}

	/** @see Longs#toBytes(long) */
	@Test
	void testToBytes() {
		testLongs().forEach(value -> {
			assertThat(Longs.toBytes(value), is(ByteBuffer.allocate(Long.BYTES).putLong(value).array()));
		});
	}

	/** @see Longs#toBytes(long, byte[]) */
	@Test
	void testToBytesArray() {
		testLongs().forEach(value -> {
			assertThat(Longs.toBytes(value, new byte[Long.BYTES]), is(ByteBuffer.allocate(Long.BYTES).putLong(value).array()));
			assertThat(Longs.toBytes(value, new byte[Long.BYTES + 1]), is(ByteBuffer.allocate(Long.BYTES + 1).putLong(value).put((byte)0).array()));
			assertThat(Longs.toBytes(value, new byte[Long.BYTES + 2]), is(ByteBuffer.allocate(Long.BYTES + 2).putLong(value).put((byte)0).put((byte)0).array()));
		});
	}

	/** @see Longs#toBytes(long, byte[], int) */
	@Test
	void testToBytesArrayIndex() {
		testLongs().forEach(value -> {
			assertThat(Longs.toBytes(value, new byte[Long.BYTES], 0), is(ByteBuffer.allocate(Long.BYTES).putLong(value).array()));
			assertThat(Longs.toBytes(value, new byte[Long.BYTES + 1], 0), is(ByteBuffer.allocate(Long.BYTES + 1).putLong(value).put((byte)0).array()));
			assertThat(Longs.toBytes(value, new byte[Long.BYTES + 1], 1), is(ByteBuffer.allocate(Long.BYTES + 1).put((byte)0).putLong(value).array()));
			assertThat(Longs.toBytes(value, new byte[Long.BYTES + 2], 0), is(ByteBuffer.allocate(Long.BYTES + 2).putLong(value).put((byte)0).put((byte)0).array()));
			assertThat(Longs.toBytes(value, new byte[Long.BYTES + 2], 1), is(ByteBuffer.allocate(Long.BYTES + 2).put((byte)0).putLong(value).put((byte)0).array()));
			assertThat("Other bytes aren't disturbed.", Longs.toBytes(value, new byte[] {12, 23, 34, 45, 56, 67, 78, 89, 90, (byte)0xF0}, 1),
					is(ByteBuffer.allocate(Long.BYTES + 2).put((byte)12).putLong(value).put((byte)0xF0).array()));
			assertThat(Longs.toBytes(value, new byte[Long.BYTES + 2], 2), is(ByteBuffer.allocate(Long.BYTES + 2).put((byte)0).put((byte)0).putLong(value).array()));
		});
	}

}
