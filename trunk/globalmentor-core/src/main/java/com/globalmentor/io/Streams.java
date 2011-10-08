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

import java.io.*;

import static java.lang.System.*;
import java.util.*;

import com.globalmentor.java.Bytes;
import com.globalmentor.text.CharacterEncoding;

/**
 * Utilities for working with streams generally.
 * @author Garret Wilson
 */
public class Streams
{

	/**
	 * Copies all information from an input stream to an output stream. Both streams are used as-is. If buffered reading and writing is desired, the streams
	 * should be wrapped in a {@link BufferedInputStream} and a {@link BufferedOutputStream} and those should be passed as parameters. After copying is finished,
	 * both streams are left open.
	 * @param inputStream The source of the data.
	 * @param outputStream The destination of the data.
	 * @return The total number of bytes copied.
	 * @exception IOException Thrown if there is an error reading from or writing to a stream.
	 */
	public static long copy(final InputStream inputStream, final OutputStream outputStream) throws IOException
	{
		return copy(inputStream, outputStream, -1); //we don't know how many bytes to expect
	}

	/**
	 * Copies all information from an input stream to an output stream. Both streams are used as-is. If buffered reading and writing is desired, the streams
	 * should be wrapped in a {@link BufferedInputStream} and a {@link BufferedOutputStream} and those should be passed as parameters. After copying is finished,
	 * both streams are left open.
	 * @param inputStream The source of the data.
	 * @param outputStream The destination of the data.
	 * @param expectedContentLength The length of content expected, or -1 if the length is unknown.
	 * @return The total number of bytes copied.
	 * @throws IOException Thrown if there is an error reading from or writing to a stream.
	 * @throws IOException if an expected content length was given and the number of bytes written to the output stream is not what was expected.
	 */
	public static long copy(final InputStream inputStream, final OutputStream outputStream, final long expectedContentLength) throws IOException
	{
		final int bufferSize = expectedContentLength >= 16 * 1024 * 1024 ? 10 * 1024 * 1024 : 64 * 1024; //create a buffer of the correct size, based upon the expected length if known; use a 10MB buffer for anything at least 10MB, or a 16KB buffer for everything else
		final byte[] buffer = new byte[bufferSize]; //create a buffer for copying data
		long totalBytesCopied = 0; //show that we have not copied any data
		int bytesRead; //this will store the number of bytes read each time
		while((bytesRead = inputStream.read(buffer)) >= 0) //read bytes until the end of the input stream is reached
		{
			outputStream.write(buffer, 0, bytesRead); //write the bytes to the output stream
			totalBytesCopied += bytesRead; //update the total bytes read
		}
		if(expectedContentLength >= 0 && totalBytesCopied != expectedContentLength) //if we didn't copy what was expected
		{
			throw new IOException("Error transferring information; expected to transfer " + expectedContentLength + " bytes and instead transferred "
					+ totalBytesCopied);
		}
		return totalBytesCopied; //return the total number of bytes copied
	}

}