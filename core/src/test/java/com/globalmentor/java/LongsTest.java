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

import java.nio.ByteBuffer;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

/**
 * Tests of {@link Longs}
 * @author Garret Wilson
 */
public class LongsTest {

	private final long LONG_SEQUENCE_BYTES = ByteBuffer.wrap(new byte[] {1, 2, 3, 4, 5, 6, 7, 8}).getLong();

	private final long LONG_UPPER_BYTES = ByteBuffer
			.wrap(new byte[] {(byte)0x80, (byte)0x91, (byte)0xA2, (byte)0xB3, (byte)0xC4, (byte)0xD5, (byte)0xE6, (byte)0xF7}).getLong();

	private final long LONG_MIXED_BYTES = ByteBuffer.wrap(new byte[] {0, (byte)0x91, 2, (byte)0xB3, 4, (byte)0xD5, 6, (byte)0xF7}).getLong();

	/** @see Longs#toBytes(long) */
	@Test
	void testToBytes() {
		Stream.of(0L, 1L, -1L, 123456789L, -123456789L, LONG_SEQUENCE_BYTES, -LONG_MIXED_BYTES, LONG_UPPER_BYTES, -LONG_UPPER_BYTES, LONG_MIXED_BYTES,
				-LONG_MIXED_BYTES, Long.MAX_VALUE, Long.MIN_VALUE).forEach(value -> {
					assertThat(Longs.toBytes(value), is(ByteBuffer.allocate(Long.BYTES).putLong(value).array()));
				});
	}

	/** @see Longs#toLong(byte[]) */
	@Test
	void testToLong() {
		Stream.of(0L, 1L, -1L, 123456789L, -123456789L, LONG_SEQUENCE_BYTES, -LONG_MIXED_BYTES, LONG_UPPER_BYTES, -LONG_UPPER_BYTES, LONG_MIXED_BYTES,
				-LONG_MIXED_BYTES, Long.MAX_VALUE, Long.MIN_VALUE).forEach(value -> {
					assertThat(Longs.toLong(ByteBuffer.allocate(Long.BYTES).putLong(value).array()), is(value));
				});
	}

}
