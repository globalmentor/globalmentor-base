/*
 * Copyright © 2019 GlobalMentor, Inc. <https://www.globalmentor.com/>
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
	 * Fills a buffer with characters from a reader, starting at its beginning, blocking until the buffer is full or the end of the reader is reached.
	 * @apiNote This method functions identically to {@link Reader#read(char[])} except that this method guarantees that the buffer will <em>always</em> be filled
	 *          unless the end of the reader is reached or an error is encountered.
	 * @implSpec This implementation delegates to {@link #read(Reader, char[], int)}.
	 * @implNote Technically the {@link Reader} read API does not guarantee that an {@link IndexOutOfBoundsException} will be thrown, but many implementations do
	 *           and the analogous {@link InputStream} methods explicitly indicate {@link IndexOutOfBoundsException}. Importantly this method will throw whatever
	 *           exception the underlying {@link Reader} throws.
	 * @param reader The reader from which to read.
	 * @param buffer The buffer to fill.
	 * @return The number of characters actually read; if less than the size of the buffer, the end of the reader has been reached.
	 * @throws IndexOutOfBoundsException If the length is negative or the length is greater than the size of the buffer.
	 * @throws IOException if there is an error reading from the reader.
	 * @see Reader#read(char[])
	 */
	public static int read(final Reader reader, final char[] buffer) throws IOException {
		return read(reader, buffer, buffer.length);
	}

	/**
	 * Fills a section of a buffer with characters from a reader, starting at its beginning, blocking until the buffer is full or the end of the reader is
	 * reached.
	 * @apiNote This method functions identically to {@link Reader#read(char[], int, int)} with an offset of <code>0</code> except that this method guarantees
	 *          that the requested number of characters will <em>always</em> be read unless the end of the reader is reached or an error is encountered.
	 * @implSpec This implementation delegates to {@link #read(Reader, char[], int, int)}.
	 * @implNote Technically the {@link Reader} read API does not guarantee that an {@link IndexOutOfBoundsException} will be thrown, but many implementations do
	 *           and the analogous {@link InputStream} methods explicitly indicate {@link IndexOutOfBoundsException}. Importantly this method will throw whatever
	 *           exception the underlying {@link Reader} throws.
	 * @param reader The reader from which to read.
	 * @param buffer The buffer to fill.
	 * @param length The maximum number of characters to read.
	 * @return The number of characters actually read; if less than the requested length, the end of the reader has been reached.
	 * @throws IndexOutOfBoundsException If the length is negative or the length is greater than the size of the buffer.
	 * @throws IOException if there is an error reading from the reader.
	 * @see Reader#read(char[], int, int)
	 */
	public static int read(final Reader reader, final char[] buffer, final int length) throws IOException {
		return read(reader, buffer, 0, length);
	}

	/**
	 * Fills a section of a buffer with characters from a reader, blocking until the buffer is full or the end of the reader is reached.
	 * @apiNote This method functions identically to {@link Reader#read(char[], int, int)} except that this method guarantees that the requested number of
	 *          characters will <em>always</em> be read unless the end of the reader is reached or an error is encountered.
	 * @implNote Technically the {@link Reader} read API does not guarantee that an {@link IndexOutOfBoundsException} will be thrown, but many implementations do
	 *           and the analogous {@link InputStream} methods explicitly indicate {@link IndexOutOfBoundsException}. Importantly this method will throw whatever
	 *           exception the underlying {@link Reader} throws.
	 * @param reader The reader from which to read.
	 * @param buffer The buffer to fill.
	 * @param offset The start offset in the buffer at which the data is written.
	 * @param length The maximum number of characters to read.
	 * @return The number of characters actually read; if less than the requested length, the end of the reader has been reached.
	 * @throws IndexOutOfBoundsException If the offset is negative, the length is negative, or the length is greater than the remaining characters in the buffer
	 *           starting at the given offset.
	 * @throws IOException if there is an error reading from the reader.
	 * @see Reader#read(char[], int, int)
	 */
	public static int read(final Reader reader, final char[] buffer, int offset, int length) throws IOException {
		int totalReadCount = 0;
		int eachReadCount;
		//use `length!=0` instead of `length>0` so that Reader.read() can check for us to make sure it is not negative
		while(length != 0 && (eachReadCount = reader.read(buffer, offset, length)) != -1) {
			totalReadCount += eachReadCount;
			offset += eachReadCount; //the offset goes forward
			length -= eachReadCount; //the remaining length diminishes
		}
		return totalReadCount;
	}

	/**
	 * Reads all characters remaining in the reader into a string. This method will block until some input is available, an I/O error occurs, or the end of the
	 * reader is reached.
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
	 * error occurs, or the end of the reader is reached.
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
