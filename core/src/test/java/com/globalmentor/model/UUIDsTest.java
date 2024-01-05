/*
 * Copyright © 2024 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.model;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.nio.ByteBuffer;
import java.util.UUID;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link UUIDs}
 * @author Garret Wilson
 */
public class UUIDsTest {

	/** Arbitrary but deterministic UUID for testing. */
	static final UUID TEST_UUID = UUID.fromString("6fbc8719-8564-49da-823f-eaf797ed8251");

	/**
	 * @see UUIDs#toBytes(UUID)
	 * @see UUIDs#fromBytes(byte[])
	 */
	@Test
	void testBytesRoundTrip() {
		assertThat(UUIDs.fromBytes(UUIDs.toBytes(TEST_UUID)), is(TEST_UUID));
	}

	/** @see UUIDs#fromBytes(byte[]) */
	@Test
	void testFromBytesArray() {
		assertThat(
				UUIDs.fromBytes(ByteBuffer.allocate(UUIDs.BYTE_COUNT).putLong(TEST_UUID.getMostSignificantBits()).putLong(TEST_UUID.getLeastSignificantBits()).array()),
				is(TEST_UUID));
		assertThrows(IllegalArgumentException.class, () -> UUIDs.fromBytes(ByteBuffer.allocate(UUIDs.BYTE_COUNT + 1).putLong(TEST_UUID.getMostSignificantBits())
				.putLong(TEST_UUID.getLeastSignificantBits()).put((byte)123).array()));
		assertThrows(IllegalArgumentException.class, () -> UUIDs.fromBytes(ByteBuffer.allocate(UUIDs.BYTE_COUNT + 2).putLong(TEST_UUID.getMostSignificantBits())
				.putLong(TEST_UUID.getLeastSignificantBits()).put((byte)0x12).put((byte)0xFF).array()));
	}

	/** @see UUIDs#fromBytes(byte[], int) */
	@Test
	void testFromBytesArrayIndex() {
		assertThat(UUIDs
				.fromBytes(ByteBuffer.allocate(UUIDs.BYTE_COUNT).putLong(TEST_UUID.getMostSignificantBits()).putLong(TEST_UUID.getLeastSignificantBits()).array(), 0),
				is(TEST_UUID));
		assertThat(UUIDs.fromBytes(ByteBuffer.allocate(UUIDs.BYTE_COUNT + 1).putLong(TEST_UUID.getMostSignificantBits())
				.putLong(TEST_UUID.getLeastSignificantBits()).put((byte)123).array(), 0), is(TEST_UUID));
		assertThat(UUIDs.fromBytes(ByteBuffer.allocate(UUIDs.BYTE_COUNT + 1).put((byte)123).putLong(TEST_UUID.getMostSignificantBits())
				.putLong(TEST_UUID.getLeastSignificantBits()).array(), 1), is(TEST_UUID));
		assertThat(UUIDs.fromBytes(ByteBuffer.allocate(UUIDs.BYTE_COUNT + 2).putLong(TEST_UUID.getMostSignificantBits())
				.putLong(TEST_UUID.getLeastSignificantBits()).put((byte)0x12).put((byte)0xFF).array(), 0), is(TEST_UUID));
		assertThat(UUIDs.fromBytes(ByteBuffer.allocate(UUIDs.BYTE_COUNT + 2).put((byte)0xFF).putLong(TEST_UUID.getMostSignificantBits())
				.putLong(TEST_UUID.getLeastSignificantBits()).put((byte)0x12).array(), 1), is(TEST_UUID));
		assertThat(UUIDs.fromBytes(ByteBuffer.allocate(UUIDs.BYTE_COUNT + 2).put((byte)0x12).put((byte)0xFF).putLong(TEST_UUID.getMostSignificantBits())
				.putLong(TEST_UUID.getLeastSignificantBits()).array(), 2), is(TEST_UUID));
	}

	/** @see UUIDs#toBytes(UUID) */
	@Test
	void testToBytes() {
		assertThat(UUIDs.toBytes(TEST_UUID),
				is(ByteBuffer.allocate(UUIDs.BYTE_COUNT).putLong(TEST_UUID.getMostSignificantBits()).putLong(TEST_UUID.getLeastSignificantBits()).array()));
	}

	/** @see UUIDs#toBytes(UUID, byte[]) */
	@Test
	void testToBytesArray() {
		assertThat(UUIDs.toBytes(TEST_UUID, new byte[UUIDs.BYTE_COUNT]),
				is(ByteBuffer.allocate(UUIDs.BYTE_COUNT).putLong(TEST_UUID.getMostSignificantBits()).putLong(TEST_UUID.getLeastSignificantBits()).array()));
		assertThat(UUIDs.toBytes(TEST_UUID, new byte[UUIDs.BYTE_COUNT + 1]), is(ByteBuffer.allocate(UUIDs.BYTE_COUNT + 1)
				.putLong(TEST_UUID.getMostSignificantBits()).putLong(TEST_UUID.getLeastSignificantBits()).put((byte)0).array()));
		assertThat(UUIDs.toBytes(TEST_UUID, new byte[UUIDs.BYTE_COUNT + 2]), is(ByteBuffer.allocate(UUIDs.BYTE_COUNT + 2)
				.putLong(TEST_UUID.getMostSignificantBits()).putLong(TEST_UUID.getLeastSignificantBits()).put((byte)0).put((byte)0).array()));
	}

	/** @see UUIDs#toBytes(UUID, byte[], int) */
	@Test
	void testToBytesArrayIndex() {
		assertThat(UUIDs.toBytes(TEST_UUID, new byte[UUIDs.BYTE_COUNT], 0),
				is(ByteBuffer.allocate(UUIDs.BYTE_COUNT).putLong(TEST_UUID.getMostSignificantBits()).putLong(TEST_UUID.getLeastSignificantBits()).array()));
		assertThat(UUIDs.toBytes(TEST_UUID, new byte[UUIDs.BYTE_COUNT + 1], 0), is(ByteBuffer.allocate(UUIDs.BYTE_COUNT + 1)
				.putLong(TEST_UUID.getMostSignificantBits()).putLong(TEST_UUID.getLeastSignificantBits()).put((byte)0).array()));
		assertThat(UUIDs.toBytes(TEST_UUID, new byte[UUIDs.BYTE_COUNT + 1], 1), is(ByteBuffer.allocate(UUIDs.BYTE_COUNT + 1).put((byte)0)
				.putLong(TEST_UUID.getMostSignificantBits()).putLong(TEST_UUID.getLeastSignificantBits()).array()));
		assertThat(UUIDs.toBytes(TEST_UUID, new byte[UUIDs.BYTE_COUNT + 2], 0), is(ByteBuffer.allocate(UUIDs.BYTE_COUNT + 2)
				.putLong(TEST_UUID.getMostSignificantBits()).putLong(TEST_UUID.getLeastSignificantBits()).put((byte)0).put((byte)0).array()));
		assertThat(UUIDs.toBytes(TEST_UUID, new byte[UUIDs.BYTE_COUNT + 2], 1), is(ByteBuffer.allocate(UUIDs.BYTE_COUNT + 2).put((byte)0)
				.putLong(TEST_UUID.getMostSignificantBits()).putLong(TEST_UUID.getLeastSignificantBits()).put((byte)0).array()));
		assertThat("Other bytes aren't disturbed.",
				UUIDs.toBytes(TEST_UUID,
						new byte[] {12, 23, 34, 45, 56, 67, 78, 89, 90, 100, 110, 120, (byte)130, (byte)140, (byte)150, (byte)160, (byte)170, (byte)0xF0}, 1),
				is(ByteBuffer.allocate(UUIDs.BYTE_COUNT + 2).put((byte)12).putLong(TEST_UUID.getMostSignificantBits()).putLong(TEST_UUID.getLeastSignificantBits())
						.put((byte)0xF0).array()));
		assertThat(UUIDs.toBytes(TEST_UUID, new byte[UUIDs.BYTE_COUNT + 2], 2), is(ByteBuffer.allocate(UUIDs.BYTE_COUNT + 2).put((byte)0).put((byte)0)
				.putLong(TEST_UUID.getMostSignificantBits()).putLong(TEST_UUID.getLeastSignificantBits()).array()));
	}

}
