/*
 * Copyright © 2019 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.*;

import org.junit.jupiter.api.*;

import com.globalmentor.java.Characters;

/**
 * Tests of {@link ReaderParser}.
 * @author Garret Wilson
 */
public class ReaderParserTest {

	/** Tests that {@link ReaderParser#consumeUntil(Reader, Characters, char, boolean, StringBuilder, boolean)} detects the end of the stream when request. */
	@Test
	public void testConsumeUntilDetectsEnd() throws IOException {
		final Reader reader = new StringReader("abc");
		final StringBuilder stringBuilder = new StringBuilder();
		ReaderParser.consumeUntil(reader, null, 'x', false, stringBuilder, false);
	}

	/**
	 * Tests the main parsing method {@link ReaderParser#consumeWhile(Reader, Characters, StringBuilder)} to ensure that it works in the presence of an old JDK
	 * bug with {@link LineNumberReader} and a CRLF straddling a buffer boundary.
	 * @see ReaderParser#consumeWhile(Reader, Characters, StringBuilder)
	 * @see <a href="https://bugs.java.com/bugdatabase/view_bug.do?bug_id=JDK-8218280">JDK-8218280 : LineNumberReader throws "Mark invalid" exception if CRLF
	 *      straddles buffer.</a>
	 */
	@Test
	public void testConsumeWhileWithLineNumberReader() throws IOException {
		//Buffered readers will invalidate the mark when loading another buffer.
		//LineNumberReader will read a `\n` if it comes upon a `\r`, so if
		//the CRLF straddles the buffer end and the reader only marked one character,
		//reading the second character will invalidate the mark and result in:
		//    java.ioException: Mark invalid
		final String string = "foo\r\nbar";
		try (final Reader reader = new LineNumberReader(new StringReader(string), 5)) {
			// The following sequence shows how to manually force this problem for illustration.  
			//			reader.read();
			//			reader.read();
			//			reader.read();
			//			reader.read();
			//			reader.mark(1);
			//			reader.read();
			//			reader.reset(); //error
			//			System.out.print((char)reader.read());
			//			System.out.print((char)reader.read());
			//			System.out.println((char)reader.read());
			final int c = ReaderParser.consumeWhile(reader, Characters.of('f', 'o', '\r', '\n'), null);
			assertThat(c, is((int)'b'));
		}
	}

}
