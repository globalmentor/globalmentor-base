/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import java.io.*;

/**
 * Class to manipulate output streams.
 * @author Garret Wilson
 */
public class OutputStreams {

	/**
	 * Writes the given byte to the output stream the specified number of times.
	 * @param outputStream The stream the bytes should be written to.
	 * @param b The byte to write; the 24 high-order bits are ignored.
	 * @param count The number of bytes to write.
	 * @throws IOException Thrown if there is an error writing to the output stream.
	 */
	public static void write(final OutputStream outputStream, final int b, int count) throws IOException {
		for(; count > 0; --count)
			//decrement the count each time we write a byte
			outputStream.write(b); //write the byte
	}

	/**
	 * Writes the given number of bytes of the given value, low-ordered bytes first.
	 * @param outputStream The stream the bytes should be written to.
	 * @param value The value to write.
	 * @param byteCount The number of bytes to write (&lt;=4).
	 * @throws IllegalArgumentException Thrown if the byte count is over four.
	 * @throws IOException Thrown if there is an error writing to the output stream.
	 */
	public static void writeLowOrderFirst(final OutputStream outputStream, long value, int byteCount) throws IllegalArgumentException, IOException {
		if(byteCount > 4) //if an invalid byte count was given
			throw new IllegalArgumentException("Invalid byte count: " + byteCount);
		for(; byteCount > 0; --byteCount) { //write each byte
			outputStream.write((int)value); //write the LSB of the value
			value = value >>> 8; //shift the value down a byte
		}
	}

}