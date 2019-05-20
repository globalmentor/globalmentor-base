/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static org.junit.jupiter.api.Assertions.*;

import java.io.*;
import java.util.*;

import org.junit.jupiter.api.Test;

/**
 * Various tests for streams.
 * @author Garret Wilson
 */
public class StreamsTest {

	/**
	 * Generates a buffer of data for testing using sequential bytes.
	 * @param length The number of bytes to return.
	 * @return A buffer of data for use in testing.
	 */
	public static byte[] generateSequentialTestData(final int length) {
		final byte[] testData = new byte[length];
		short b = 0;
		for(int i = 0; i < testData.length; ++i) {
			testData[i] = (byte)b++; //store this byte and increment the value
			if(b > 0xff) { //if we go over the maximum byte size
				b = 0; //start over for byte values
			}
		}
		return testData;
	}

	/** Tests that a fixed-length input stream reads the correct data. */
	@Test
	public void testFixedLengthInputStream() throws IOException {
		final byte[] testData = generateSequentialTestData(Short.MAX_VALUE);
		final InputStream testInputStream = new ByteArrayInputStream(testData);
		final Random random = new Random();
		int total = 0;
		do {
			final int length = Math.min(random.nextInt(Short.MAX_VALUE / 3), testData.length - total); //get the next size to retrieve; require at least three passes
			final InputStream inputStream = new FixedLengthInputStream(testInputStream, length, false); //don't close the underlying stream on each pass
			final ByteArrayOutputStream output = new ByteArrayOutputStream();
			Streams.copy(inputStream, output);
			assertEquals(length, output.toByteArray().length);
			assertTrue(Arrays.equals(Arrays.copyOfRange(testData, total, total + length), output.toByteArray()),
					"HTTP chunked output stream did not correctly write data.");
			total += length;
		} while(total < testData.length);
		testInputStream.close();
	}

}
