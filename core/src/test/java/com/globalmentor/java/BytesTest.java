/*
 * Copyright © 2022 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.java.Bytes.NO_BYTES;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link Bytes}.
 * @author Garret Wilson
 */
public class BytesTest {

	/** @see Bytes#toHexString(byte[]) */
	@Test
	void testToHexString() {
		assertThat(Bytes.toHexString(NO_BYTES), is(""));
		assertThat(Bytes.toHexString(new byte[] {0}), is("00"));
		assertThat(Bytes.toHexString(new byte[] {(byte)0xFF}), is("ff"));
		assertThat(Bytes.toHexString(new byte[] {0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11}),
				is("000102030405060708090a0b0c0d0e0f1011"));
		assertThat(Bytes.toHexString(new byte[] {0x12, 0x34, 0x56, 0x78, (byte)0x9A, (byte)0xBC, (byte)0xDE, (byte)0xF0}), is("123456789abcdef0"));
	}

	/** @see Bytes#fromHexString(CharSequence) */
	@Test
	void testFromHexString() {
		assertThat(Bytes.fromHexString(""), is(NO_BYTES));
		assertThat(Bytes.fromHexString("00"), is(new byte[] {0}));
		assertThat(Bytes.fromHexString("ff"), is(new byte[] {(byte)0xFF}));
		assertThat(Bytes.fromHexString("FF"), is(new byte[] {(byte)0xFF}));
		assertThat(Bytes.fromHexString("000102030405060708090a0b0c0d0e0f1011"),
				is(new byte[] {0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11}));
		assertThat(Bytes.fromHexString("000102030405060708090A0B0C0D0E0F1011"),
				is(new byte[] {0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11}));
		assertThat(Bytes.fromHexString("123456789abcdef0"), is(new byte[] {0x12, 0x34, 0x56, 0x78, (byte)0x9A, (byte)0xBC, (byte)0xDE, (byte)0xF0}));
		assertThat(Bytes.fromHexString("123456789ABCDEF0"), is(new byte[] {0x12, 0x34, 0x56, 0x78, (byte)0x9A, (byte)0xBC, (byte)0xDE, (byte)0xF0}));
		assertThrows(IllegalArgumentException.class, () -> Bytes.fromHexString("0"));
		assertThrows(IllegalArgumentException.class, () -> Bytes.fromHexString("123"));
		assertThrows(IllegalArgumentException.class, () -> Bytes.fromHexString("x"));
		assertThrows(IllegalArgumentException.class, () -> Bytes.fromHexString("123!45"));
	}

}
