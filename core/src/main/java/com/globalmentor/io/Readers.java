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

import static com.globalmentor.java.Conditions.*;
import static java.lang.Math.*;

import java.io.*;

import javax.annotation.*;

/**
 * Utility methods for working with {@link Reader} instances.
 * @author Garret Wilson
 */
public class Readers {

	/**
	 * The size of buffer in characters to use when reading from a reader.
	 * @implNote Compare with buffer size used in {@link BufferedInputStream}.
	 */
	private static final int BUFFER_SIZE = 8 * 1024;

	/**
	 * Reads all characters remaining in the reader into a string. This method will block until some input is available, an I/O error occurs, or the end of the
	 * stream is reached.
	 * @apiNote This method is analogous to {{java.nio.file.Files.readString(Path path)}}, introduced in Java 11.
	 * @implSpec This implementation delegates to {@link #readString(Reader, int)} with a maximum length of {@value Integer#MAX_VALUE}.
	 * @param reader The reader from which to read content.
	 * @return A string containing all the remaining characters read from the reader.
	 * @throws IOException if an I/O error occurs.
	 */
	public static String readString(@Nonnull final Reader reader) throws IOException {
		return readString(reader, Integer.MAX_VALUE);
	}

	/**
	 * Reads all characters remaining in the reader into a string up until the given maximum length. This method will block until some input is available, an I/O
	 * error occurs, or the end of the stream is reached.
	 * @param reader The reader from which to read content.
	 * @param maxLength The maximum number of characters to read; may be {@value Integer#MAX_VALUE}.
	 * @return A string containing all the remaining characters read from the reader but no longer that the given maximum length.
	 * @throws IllegalArgumentException if the given maximum length is negative.
	 * @throws IOException if an I/O error occurs.
	 */
	public static String readString(@Nonnull final Reader reader, @Nonnegative final int maxLength) throws IOException {
		if(maxLength == 0) {
			return ""; //if they don't want anything, no need to do anything
		}
		final int bufferSize = min(BUFFER_SIZE, checkArgumentNotNegative(maxLength)); //no need for a bigger buffer than necessary
		final StringBuilder stringBuilder = new StringBuilder();
		final char[] buffer = new char[bufferSize];
		int totalRemainingCount = maxLength;
		int readCount;
		while(totalRemainingCount > 0 && (readCount = reader.read(buffer, 0, min(bufferSize, totalRemainingCount))) != -1) {
			stringBuilder.append(buffer, 0, readCount);
			totalRemainingCount -= readCount;
		}
		return stringBuilder.toString();
	}

}
