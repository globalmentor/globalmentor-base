/*
 * Copyright © 2016 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.*;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

import static com.globalmentor.io.UTF8.*;
import static java.nio.charset.StandardCharsets.*;

/**
 * Tests of {@link UTF8}.
 * @author Garret Wilson
 */
public class UTF8Test {

	/** @see UTF8#getEncodedByteCountFromInitialOctet(int) */
	@Test
	public void testGetInitialOctetEncodedByteCount() {
		assertThat(getEncodedByteCountFromInitialOctet(Byte.toUnsignedInt("$".getBytes(UTF_8)[0])), is(1));
		assertThat(getEncodedByteCountFromInitialOctet(Byte.toUnsignedInt("¢".getBytes(UTF_8)[0])), is(2));
		assertThat(getEncodedByteCountFromInitialOctet(Byte.toUnsignedInt("€".getBytes(UTF_8)[0])), is(3));
		assertThat(getEncodedByteCountFromInitialOctet(Byte.toUnsignedInt("😂".getBytes(UTF_8)[0])), is(4));
	}

	/** @see UTF8#getEncodedByteCountFromInitialOctet(int) */
	@Test
	public void testGetInitialOctetEncodedByteCountISO_8859_1() {
		assertThrows(IllegalArgumentException.class, () -> getEncodedByteCountFromInitialOctet('¢'));
	}

	/** @see UTF8#getEncodedByteCountFromInitialOctet(int) */
	@Test
	public void testGetInitialOctetEncodedByteCountHigh() {
		assertThrows(IllegalArgumentException.class, () -> getEncodedByteCountFromInitialOctet(0b11111000));
	}
}
