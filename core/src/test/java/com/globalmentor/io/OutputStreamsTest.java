/*
 * Copyright © 2025 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static com.globalmentor.java.Bytes.*;
import static java.nio.charset.StandardCharsets.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import org.junit.jupiter.api.Test;

/**
 * Tests of {@link OutputStreams}.
 * @author Garret Wilson
 */
public class OutputStreamsTest {

	/** Tests for {@link OutputStreams#collectBytes(com.globalmentor.io.function.IOConsumer)}. */
	@Test
	void testCollectBytes() {
		final byte[] testBytes = "abcdefghijklmnopqrstuvwxyz0123456798".getBytes(US_ASCII);
		assertThat("collected bytes match", OutputStreams.collectBytes(os -> os.write(testBytes)), is(testBytes));
		assertThat("empty byte array", OutputStreams.collectBytes(os -> {}), is(NO_BYTES));
	}

	/** Tests for {@link OutputStreams#collectBytes(com.globalmentor.io.function.IOConsumer, int)}. */
	@Test
	void testCollectBytesWithInitialBufferSize() {
		final byte[] testBytes = "Custom buffer size test".getBytes(UTF_8);
		assertThat("with custom buffer size", OutputStreams.collectBytes(os -> os.write(testBytes), 64), is(testBytes));
		assertThat("with small buffer size", OutputStreams.collectBytes(os -> os.write(testBytes), 8), is(testBytes));
	}

	/** Tests for {@link OutputStreams#collectString(com.globalmentor.io.function.IOConsumer, java.nio.charset.Charset)}. */
	@Test
	void testCollectString() {
		final String testString = "Hello, World! 你好 🌍"; //你好 = "hello" in Chinese
		assertThat("UTF-8 encoding", OutputStreams.collectString(os -> os.write(testString.getBytes(UTF_8)), UTF_8), is(testString));
		assertThat("UTF-16 encoding", OutputStreams.collectString(os -> os.write(testString.getBytes(UTF_16)), UTF_16), is(testString));
		assertThat("US-ASCII encoding", OutputStreams.collectString(os -> os.write("ASCII only".getBytes(US_ASCII)), US_ASCII), is("ASCII only"));
		assertThat("empty string", OutputStreams.collectString(os -> {}, UTF_8), is(""));
	}

	/** Tests for {@link OutputStreams#collectString(com.globalmentor.io.function.IOConsumer, java.nio.charset.Charset, int)}. */
	@Test
	void testCollectStringWithInitialBufferSize() {
		final String testString = "Custom buffer size test";
		assertThat("with custom buffer size", OutputStreams.collectString(os -> os.write(testString.getBytes(UTF_8)), UTF_8, 64), is(testString));
		assertThat("with small buffer size", OutputStreams.collectString(os -> os.write(testString.getBytes(UTF_8)), UTF_8, 8), is(testString));
	}

}
