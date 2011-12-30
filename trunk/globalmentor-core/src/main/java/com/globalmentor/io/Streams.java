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

import com.globalmentor.event.ProgressEvent;
import com.globalmentor.event.ProgressListener;

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
	 * @throws IOException Thrown if there is an error reading from or writing to a stream.
	 */
	public static long copy(final InputStream inputStream, final OutputStream outputStream) throws IOException
	{
		return copy(inputStream, outputStream, null);
	}

	/**
	 * Copies all information from an input stream to an output stream. Both streams are used as-is. If buffered reading and writing is desired, the streams
	 * should be wrapped in a {@link BufferedInputStream} and a {@link BufferedOutputStream} and those should be passed as parameters. After copying is finished,
	 * both streams are left open.
	 * @param inputStream The source of the data.
	 * @param outputStream The destination of the data.
	 * @param progressListener A listener to be notified of progress, or <code>null</code> if no progress notifications is requested.
	 * @return The total number of bytes copied.
	 * @throws IOException Thrown if there is an error reading from or writing to a stream.
	 * @throws IOException if an expected content length was given and the number of bytes written to the output stream is not what was expected.
	 */
	public static long copy(final InputStream inputStream, final OutputStream outputStream, final ProgressListener progressListener) throws IOException
	{
		return copy(inputStream, outputStream, -1, progressListener); //we don't know how many bytes to expect
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
		return copy(inputStream, outputStream, expectedContentLength, null); //we don't have a progress listener
	}

	/**
	 * Copies all information from an input stream to an output stream. Both streams are used as-is. If buffered reading and writing is desired, the streams
	 * should be wrapped in a {@link BufferedInputStream} and a {@link BufferedOutputStream} and those should be passed as parameters. After copying is finished,
	 * both streams are left open.
	 * @param inputStream The source of the data.
	 * @param outputStream The destination of the data.
	 * @param expectedContentLength The length of content expected, or -1 if the length is unknown.
	 * @param progressListener A listener to be notified of progress, or <code>null</code> if no progress notifications is requested.
	 * @return The total number of bytes copied.
	 * @throws IOException Thrown if there is an error reading from or writing to a stream.
	 * @throws IOException if an expected content length was given and the number of bytes written to the output stream is not what was expected.
	 */
	public static long copy(final InputStream inputStream, final OutputStream outputStream, final long expectedContentLength,
			final ProgressListener progressListener) throws IOException
	{
		final int bufferSize; //determine the optimal buffer size to use
		/*TODO check the amount of memory before trying to use such large buffers
		if(expectedContentLength >= 1 * 1024 * 1024 * 1024) //if the expected content length is over 1GB
		{
			bufferSize = 100 * 1024 * 1024; //use a 100MB buffer
		}
		else if(expectedContentLength >= 64 * 1024 * 1024) //if the expected content length is over 64MB
		{
			bufferSize = 50 * 1024 * 1024; //use a 50MB buffer
		}
		*/
		if(expectedContentLength >= 16 * 1024 * 1024) //if the expected content length is over 16MB
		{
			bufferSize = 10 * 1024 * 1024; //use a 10MB buffer
		}
		else if(expectedContentLength >= 1 * 1024 * 1024) //if the expected content length is over 1MB
		{
			bufferSize = 1 * 1024 * 1024; //use a 1MB buffer
		}
		else
		//for smaller (or unknown) sizes
		{
			bufferSize = 64 * 1024; //use a 16KB buffer for everything else
		}
		final byte[] buffer = new byte[bufferSize]; //create a buffer for copying data
		long totalBytesCopied = 0; //show that we have not copied any data
		int bytesRead; //this will store the number of bytes read each time
		if(progressListener != null) //if we have a progress listener
		{
			progressListener.progressed(new ProgressEvent(Streams.class, bufferSize, 0, expectedContentLength)); //indicate that we are starting but have made no progress
		}
		while((bytesRead = inputStream.read(buffer)) >= 0) //read bytes until the end of the input stream is reached
		{
			outputStream.write(buffer, 0, bytesRead); //write the bytes to the output stream
			totalBytesCopied += bytesRead; //update the total bytes read
			if(progressListener != null) //if we have a progress listener
			{
				progressListener.progressed(new ProgressEvent(Streams.class, bytesRead, totalBytesCopied, expectedContentLength)); //indicate our progress
			}
		}
		if(expectedContentLength >= 0 && totalBytesCopied != expectedContentLength) //if we didn't copy what was expected
		{
			throw new IOException("Error transferring information; expected to transfer " + expectedContentLength + " bytes and instead transferred "
					+ totalBytesCopied);
		}
		return totalBytesCopied; //return the total number of bytes copied
	}

}