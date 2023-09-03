/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static com.globalmentor.io.IOStreams.DEFAULT_BUFFER_SIZE;
import static java.lang.String.format;
import static java.util.Arrays.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.*;
import java.util.*;

import org.junit.jupiter.api.Test;

import com.globalmentor.java.Bytes;

/**
 * Various tests for the {@link IOStreams} utilities.
 * @author Garret Wilson
 */
public class IOStreamsTest {

	/** @see IOStreams#copy(InputStream, OutputStream) */
	@Test
	public void testCopy() throws IOException {
		final Random random = new Random(20201128);
		for(final int length : asList(0, 1, 100, DEFAULT_BUFFER_SIZE - 1, DEFAULT_BUFFER_SIZE + 1, DEFAULT_BUFFER_SIZE * 2 - 1, DEFAULT_BUFFER_SIZE * 2,
				DEFAULT_BUFFER_SIZE * 2 + 1, DEFAULT_BUFFER_SIZE * 3)) {
			final byte[] bytes = Bytes.generateRandom(length, random);
			try (final InputStream inputStream = new ByteArrayInputStream(bytes); final ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
				inputStream.transferTo(outputStream);
				assertThat(format("Copied %d bytes.", length), outputStream.toByteArray(), is(bytes));
			}
		}
	}

}
