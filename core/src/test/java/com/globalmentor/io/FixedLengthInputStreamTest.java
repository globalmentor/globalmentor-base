/*
 * Copyright © 1996-2020 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static com.globalmentor.io.IOStreams.*;
import static java.lang.Math.*;
import static java.util.Arrays.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.*;
import java.util.*;

import org.junit.jupiter.api.Test;

import com.globalmentor.java.Bytes;

/**
 * Tests {@link FixedLengthInputStream}.
 * @author Garret Wilson
 */
public class FixedLengthInputStreamTest {

	/** Tests that a fixed-length input stream reads the correct data. */
	@Test
	public void testFixedLengthInputStream() throws IOException {
		final Random random = new Random(20201128);
		final byte[] testData = Bytes.generateRandom(Short.MAX_VALUE, random);
		final InputStream testInputStream = new ByteArrayInputStream(testData);
		int total = 0;
		do {
			final int length = min(random.nextInt(Short.MAX_VALUE / 3), testData.length - total); //get the next size to retrieve; require at least three passes
			final InputStream inputStream = new FixedLengthInputStream(testInputStream, length, false); //don't close the underlying stream on each pass
			final ByteArrayOutputStream output = new ByteArrayOutputStream();
			copy(inputStream, output);
			assertThat(output.toByteArray().length, is(length));
			assertThat(output.toByteArray(), is(copyOfRange(testData, total, total + length)));
			total += length;
		} while(total < testData.length);
		testInputStream.close();
	}

}
