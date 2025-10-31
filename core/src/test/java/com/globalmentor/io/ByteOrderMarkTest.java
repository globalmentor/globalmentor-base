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

import org.junit.jupiter.api.Test;

import java.util.Optional;

import com.globalmentor.model.MutableReference;

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

	/** Tests for {@link ByteOrderMark#impute(byte[], CharSequence, MutableReference)}. */
	@Test
	public void testImputeWithActualBOM() {
		final MutableReference<ByteOrderMark> actualBOM = new MutableReference<>();

		// UTF-8 BOM
		assertThat("imputed BOM", ByteOrderMark.impute(new byte[] {(byte)0xEF, (byte)0xBB, (byte)0xBF, 0x3C, 0x3F, 0x78, 0x6D}, "<?xml", actualBOM),
				is(Optional.of(ByteOrderMark.UTF_8)));
		assertThat("actual BOM is set", actualBOM.get(), is(ByteOrderMark.UTF_8));

		actualBOM.set(null);

		// UTF-16BE BOM
		assertThat("imputed BOM", ByteOrderMark.impute(new byte[] {(byte)0xFE, (byte)0xFF, 0x00, 0x3C, 0x00, 0x3F, 0x00, 0x78}, "<?xml", actualBOM),
				is(Optional.of(ByteOrderMark.UTF_16BE)));
		assertThat("actual BOM is set", actualBOM.get(), is(ByteOrderMark.UTF_16BE));

		actualBOM.set(null);

		// UTF-16LE BOM
		assertThat("imputed BOM", ByteOrderMark.impute(new byte[] {(byte)0xFF, (byte)0xFE, 0x3C, 0x00, 0x3F, 0x00, 0x78, 0x00}, "<?xml", actualBOM),
				is(Optional.of(ByteOrderMark.UTF_16LE)));
		assertThat("actual BOM is set", actualBOM.get(), is(ByteOrderMark.UTF_16LE));

		actualBOM.set(null);

		// UTF-32BE BOM
		assertThat("imputed BOM",
				ByteOrderMark.impute(new byte[] {0x00, 0x00, (byte)0xFE, (byte)0xFF, 0x00, 0x00, 0x00, 0x3C, 0x00, 0x00, 0x00, 0x3F}, "<?xml", actualBOM),
				is(Optional.of(ByteOrderMark.UTF_32BE)));
		assertThat("actual BOM is set", actualBOM.get(), is(ByteOrderMark.UTF_32BE));

		actualBOM.set(null);

		// UTF-32LE BOM
		assertThat("imputed BOM",
				ByteOrderMark.impute(new byte[] {(byte)0xFF, (byte)0xFE, 0x00, 0x00, 0x3C, 0x00, 0x00, 0x00, 0x3F, 0x00, 0x00, 0x00}, "<?xml", actualBOM),
				is(Optional.of(ByteOrderMark.UTF_32LE)));
		assertThat("actual BOM is set", actualBOM.get(), is(ByteOrderMark.UTF_32LE));

		actualBOM.set(null);

		// UTF-32BE_MIXED BOM
		assertThat("imputed BOM",
				ByteOrderMark.impute(new byte[] {0x00, 0x00, (byte)0xFF, (byte)0xFE, 0x00, 0x00, 0x3C, 0x00, 0x00, 0x00, 0x3F, 0x00}, "<?xml", actualBOM),
				is(Optional.of(ByteOrderMark.UTF_32BE_MIXED)));
		assertThat("actual BOM is set", actualBOM.get(), is(ByteOrderMark.UTF_32BE_MIXED));

		actualBOM.set(null);

		// UTF-32LE_MIXED BOM
		assertThat("imputed BOM",
				ByteOrderMark.impute(new byte[] {(byte)0xFE, (byte)0xFF, 0x00, 0x00, 0x00, 0x3C, 0x00, 0x00, 0x00, 0x3F, 0x00, 0x00}, "<?xml", actualBOM),
				is(Optional.of(ByteOrderMark.UTF_32LE_MIXED)));
		assertThat("actual BOM is set", actualBOM.get(), is(ByteOrderMark.UTF_32LE_MIXED));
	}

	/** Tests for {@link ByteOrderMark#impute(byte[], CharSequence, MutableReference)}. */
	@Test
	public void testImputeWithoutBOMUTF32() {
		final MutableReference<ByteOrderMark> actualBOM = new MutableReference<>();

		// UTF-32BE without BOM: 00 00 00 X1
		assertThat("imputed BOM", ByteOrderMark.impute(new byte[] {0x00, 0x00, 0x00, 0x3C, 0x00, 0x00, 0x00, 0x3F}, "<?xml", actualBOM),
				is(Optional.of(ByteOrderMark.UTF_32BE)));
		assertThat("actual BOM is not set", actualBOM.get(), is(nullValue()));

		actualBOM.set(null);

		// UTF-32LE without BOM: X1 00 00 00
		assertThat("imputed BOM", ByteOrderMark.impute(new byte[] {0x3C, 0x00, 0x00, 0x00, 0x3F, 0x00, 0x00, 0x00}, "<?xml", actualBOM),
				is(Optional.of(ByteOrderMark.UTF_32LE)));
		assertThat("actual BOM is not set", actualBOM.get(), is(nullValue()));

		actualBOM.set(null);

		// UTF-32BE_MIXED without BOM: 00 00 X1 00
		assertThat("imputed BOM", ByteOrderMark.impute(new byte[] {0x00, 0x00, 0x3C, 0x00, 0x00, 0x00, 0x3F, 0x00}, "<?xml", actualBOM),
				is(Optional.of(ByteOrderMark.UTF_32BE_MIXED)));
		assertThat("actual BOM is not set", actualBOM.get(), is(nullValue()));

		actualBOM.set(null);

		// UTF-32LE_MIXED without BOM: 00 X1 00 00
		assertThat("imputed BOM", ByteOrderMark.impute(new byte[] {0x00, 0x3C, 0x00, 0x00, 0x00, 0x3F, 0x00, 0x00}, "<?xml", actualBOM),
				is(Optional.of(ByteOrderMark.UTF_32LE_MIXED)));
		assertThat("actual BOM is not set", actualBOM.get(), is(nullValue()));
	}

	/** Tests for {@link ByteOrderMark#impute(byte[], CharSequence, MutableReference)}. */
	@Test
	public void testImputeWithoutBOMUTF16() {
		final MutableReference<ByteOrderMark> actualBOM = new MutableReference<>();

		// UTF-16BE without BOM: 00 X1 00 X2
		assertThat("imputed BOM", ByteOrderMark.impute(new byte[] {0x00, 0x3C, 0x00, 0x3F, 0x00, 0x78, 0x00, 0x6D}, "<?xml", actualBOM),
				is(Optional.of(ByteOrderMark.UTF_16BE)));
		assertThat("actual BOM is not set", actualBOM.get(), is(nullValue()));

		actualBOM.set(null);

		// UTF-16LE without BOM: X1 00 X2 00
		assertThat("imputed BOM", ByteOrderMark.impute(new byte[] {0x3C, 0x00, 0x3F, 0x00, 0x78, 0x00, 0x6D, 0x00}, "<?xml", actualBOM),
				is(Optional.of(ByteOrderMark.UTF_16LE)));
		assertThat("actual BOM is not set", actualBOM.get(), is(nullValue()));
	}

	/** Tests for {@link ByteOrderMark#impute(byte[], CharSequence, MutableReference)}. */
	@Test
	public void testImputeWithoutBOMUTF8() {
		final MutableReference<ByteOrderMark> actualBOM = new MutableReference<>();

		// UTF-8 without BOM: X1 X2 X3 X4
		assertThat("imputed BOM", ByteOrderMark.impute(new byte[] {0x3C, 0x3F, 0x78, 0x6D, 0x6C, 0x20}, "<?xml", actualBOM), is(Optional.of(ByteOrderMark.UTF_8)));
		assertThat("actual BOM is not set", actualBOM.get(), is(nullValue()));
	}

	/** Tests for {@link ByteOrderMark#impute(byte[], CharSequence, MutableReference)}. */
	@Test
	public void testImputeInsufficientBytes() {
		final MutableReference<ByteOrderMark> actualBOM = new MutableReference<>();

		// Less than 4 bytes
		assertThat("imputed BOM with 3 bytes", ByteOrderMark.impute(new byte[] {0x3C, 0x3F, 0x78}, "<?xml", actualBOM), is(Optional.empty()));
		assertThat("actual BOM is not set", actualBOM.get(), is(nullValue()));

		actualBOM.set(null);

		// Empty array
		assertThat("imputed BOM with empty array", ByteOrderMark.impute(new byte[] {}, "<?xml", actualBOM), is(Optional.empty()));
		assertThat("actual BOM is not set", actualBOM.get(), is(nullValue()));
	}

	/** Tests for {@link ByteOrderMark#impute(byte[], CharSequence, MutableReference)}. */
	@Test
	public void testImputeInsufficientExpectedChars() {
		final MutableReference<ByteOrderMark> actualBOM = new MutableReference<>();

		// Empty expected chars
		assertThat("imputed BOM with empty expected chars", ByteOrderMark.impute(new byte[] {0x3C, 0x3F, 0x78, 0x6D}, "", actualBOM), is(Optional.empty()));
		assertThat("actual BOM is not set", actualBOM.get(), is(nullValue()));

		actualBOM.set(null);

		// Only one expected char - can detect UTF-32 variants
		assertThat("imputed BOM with one expected char", ByteOrderMark.impute(new byte[] {0x00, 0x00, 0x00, 0x3C, 0x00, 0x00, 0x00, 0x3F}, "<", actualBOM),
				is(Optional.of(ByteOrderMark.UTF_32BE)));
		assertThat("actual BOM is not set", actualBOM.get(), is(nullValue()));

		actualBOM.set(null);

		// Two expected chars - can detect UTF-16 variants but not UTF-8
		assertThat("imputed BOM with two expected chars UTF-16BE",
				ByteOrderMark.impute(new byte[] {0x00, 0x3C, 0x00, 0x3F, 0x00, 0x78, 0x00, 0x6D}, "<?", actualBOM), is(Optional.of(ByteOrderMark.UTF_16BE)));
		assertThat("actual BOM is not set", actualBOM.get(), is(nullValue()));

		actualBOM.set(null);

		// Three expected chars - still cannot detect UTF-8 (needs 4)
		assertThat("imputed BOM with three expected chars UTF-8", ByteOrderMark.impute(new byte[] {0x3C, 0x3F, 0x78, 0x6D}, "<?x", actualBOM),
				is(Optional.empty()));
		assertThat("actual BOM is not set", actualBOM.get(), is(nullValue()));
	}

	/** Tests for {@link ByteOrderMark#impute(byte[], CharSequence, MutableReference)}. */
	@Test
	public void testImputeNoMatch() {
		final MutableReference<ByteOrderMark> actualBOM = new MutableReference<>();

		// Bytes that don't match any encoding pattern
		assertThat("imputed BOM with non-matching bytes", ByteOrderMark.impute(new byte[] {0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08}, "<?xml", actualBOM),
				is(Optional.empty()));
		assertThat("actual BOM is not set", actualBOM.get(), is(nullValue()));
	}

	/** Tests for {@link ByteOrderMark#impute(byte[], CharSequence, MutableReference)}. */
	@Test
	public void testImputePriority() {
		final MutableReference<ByteOrderMark> actualBOM = new MutableReference<>();

		// Actual BOM takes priority over imputed encoding
		// UTF-8 BOM followed by UTF-16LE-looking data
		assertThat("actual BOM takes priority", ByteOrderMark.impute(new byte[] {(byte)0xEF, (byte)0xBB, (byte)0xBF, 0x3C, 0x00, 0x3F, 0x00}, "<?", actualBOM),
				is(Optional.of(ByteOrderMark.UTF_8)));
		assertThat("actual BOM is set to UTF-8", actualBOM.get(), is(ByteOrderMark.UTF_8));
	}
}
