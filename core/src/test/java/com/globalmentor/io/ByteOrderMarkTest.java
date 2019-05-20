/*
 * Copyright © 2016 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.*;

import org.junit.jupiter.api.Test;

import java.util.Optional;

public class ByteOrderMarkTest {

	/** @see ByteOrderMark#detect(byte[]) */
	@Test
	public void testDetect() {
		assertThat(ByteOrderMark.detect(new byte[0]), is(Optional.<ByteOrderMark>empty()));
		//UTF-8
		assertThat(ByteOrderMark.detect(new byte[] {(byte)0xEF}), is(Optional.<ByteOrderMark>empty()));
		assertThat(ByteOrderMark.detect(new byte[] {(byte)0xEF, (byte)0xBB}), is(Optional.<ByteOrderMark>empty()));
		assertThat(ByteOrderMark.detect(new byte[] {(byte)0xEF, (byte)0xBB, (byte)0xBF}), is(Optional.of(ByteOrderMark.UTF_8)));
		assertThat(ByteOrderMark.detect(new byte[] {(byte)0xEF, (byte)0xBB, (byte)0xBF, 0x40}), is(Optional.of(ByteOrderMark.UTF_8)));
		assertThat(ByteOrderMark.detect(new byte[] {(byte)0xBB, (byte)0xBF}), is(Optional.<ByteOrderMark>empty()));
		assertThat(ByteOrderMark.detect(new byte[] {(byte)0xBF}), is(Optional.<ByteOrderMark>empty()));
		//UTF-16
		assertThat(ByteOrderMark.detect(new byte[] {(byte)0xFE}), is(Optional.<ByteOrderMark>empty()));
		assertThat(ByteOrderMark.detect(new byte[] {(byte)0xFF}), is(Optional.<ByteOrderMark>empty()));
		//UTF-16BE
		assertThat(ByteOrderMark.detect(new byte[] {(byte)0xFE, (byte)0xFF}), is(Optional.of(ByteOrderMark.UTF_16BE)));
		assertThat(ByteOrderMark.detect(new byte[] {(byte)0xFE, (byte)0xFF, 0x00, 0x40}), is(Optional.of(ByteOrderMark.UTF_16BE)));
		//UTF-16LE
		assertThat(ByteOrderMark.detect(new byte[] {(byte)0xFF, (byte)0xFE}), is(Optional.of(ByteOrderMark.UTF_16LE)));
		assertThat(ByteOrderMark.detect(new byte[] {(byte)0xFF, (byte)0xFE, 0x40, 0x00}), is(Optional.of(ByteOrderMark.UTF_16LE)));
		//UTF-32
		assertThat(ByteOrderMark.detect(new byte[] {0x00, 0x00, (byte)0xFF, (byte)0xFE}), is(Optional.of(ByteOrderMark.UTF_32BE_MIXED)));
		assertThat(ByteOrderMark.detect(new byte[] {0x00, 0x00, (byte)0xFF, (byte)0xFE, 0x00, 0x00, 0x40, 0x00}), is(Optional.of(ByteOrderMark.UTF_32BE_MIXED)));
		assertThat(ByteOrderMark.detect(new byte[] {(byte)0xFE, (byte)0xFF, 0x00, 0x00}), is(Optional.of(ByteOrderMark.UTF_32LE_MIXED)));
		assertThat(ByteOrderMark.detect(new byte[] {(byte)0xFE, (byte)0xFF, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00}), is(Optional.of(ByteOrderMark.UTF_32LE_MIXED)));
		//UTF32BE
		assertThat(ByteOrderMark.detect(new byte[] {0x00}), is(Optional.<ByteOrderMark>empty()));
		assertThat(ByteOrderMark.detect(new byte[] {0x00, 0x00}), is(Optional.<ByteOrderMark>empty()));
		assertThat(ByteOrderMark.detect(new byte[] {0x00, 0x00, (byte)0xFE}), is(Optional.<ByteOrderMark>empty()));
		assertThat(ByteOrderMark.detect(new byte[] {0x00, 0x00, (byte)0xFE, (byte)0xFF}), is(Optional.of(ByteOrderMark.UTF_32BE)));
		assertThat(ByteOrderMark.detect(new byte[] {0x00, 0x00, (byte)0xFE, (byte)0xFF, 0x00, 0x00, 0x00, 0x40}), is(Optional.of(ByteOrderMark.UTF_32BE)));
		assertThat(ByteOrderMark.detect(new byte[] {0x00, (byte)0xFE, (byte)0xFF}), is(Optional.<ByteOrderMark>empty()));
		//UTF32LE
		assertThat(ByteOrderMark.detect(new byte[] {(byte)0xFF, (byte)0xFE, 0x00, 0x00}), is(Optional.of(ByteOrderMark.UTF_32LE)));
		assertThat(ByteOrderMark.detect(new byte[] {(byte)0xFF, (byte)0xFE, 0x00, 0x00, 0x40, 0x00, 0x00, 0x00}), is(Optional.of(ByteOrderMark.UTF_32LE)));
	}
}
