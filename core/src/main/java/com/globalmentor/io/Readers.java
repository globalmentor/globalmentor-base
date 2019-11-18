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
	 * @param reader The reader from which to read content.
	 * @return A string containing all the remaining characters read from the reader.
	 * @throws IOException if an I/O error occurs.
	 */
	public static String readString(@Nonnull final Reader reader) throws IOException {
		final StringBuilder stringBuilder = new StringBuilder(BUFFER_SIZE);
		final char[] buffer = new char[BUFFER_SIZE];
		int charsRead;
		while((charsRead = reader.read(buffer)) != -1) {
			stringBuilder.append(buffer, 0, charsRead);
		}
		return stringBuilder.toString();
	}

}
