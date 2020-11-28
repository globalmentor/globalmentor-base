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
public class IOStreams {

	/**
	 * The recommended default size of I/O buffers.
	 * @apiNote The general consensus is that many I/O devices use block sizes of 4096 or 8192 so the buffer size is optimal at a multiple of this. But the value
	 *          needs to be low enough to fit in the CPU cache. Many JDK buffers such as that in {@link BufferedInputStream} and the utilities in
	 *          {@link java.nio.file.Files} have been tuned to 8192.
	 * @implNote This value may change in the future if warranted by new technology.
	 * @see <a href="https://stackoverflow.com/q/236861">How do you determine the ideal buffer size when using FileInputStream?</a>
	 * @see <a href="https://stackoverflow.com/q/3033771">File I/O with streams - best memory buffer size</a>
	 */
	public static final int DEFAULT_BUFFER_SIZE = 1 << 13; //8182

	/**
	 * Copies all information from an input stream to an output stream. After copying is finished, both streams are left open.
	 * @apiNote This method will likely eventually be deprecated in favor of Java 9+ <code>InputStream.transferTo(OutputStream)</code>.
	 * @implSpec This implementation delegates to {@link #copy(InputStream, OutputStream, ProgressListener)}.
	 * @param inputStream The source of the data.
	 * @param outputStream The destination of the data.
	 * @return The total number of bytes copied.
	 * @throws IOException Thrown if there is an error reading from or writing to a stream.
	 */
	public static long copy(final InputStream inputStream, final OutputStream outputStream) throws IOException {
		return copy(inputStream, outputStream, null);
	}

	/**
	 * Copies all information from an input stream to an output stream. Both streams are used as-is. After copying is finished, both streams are left open.
	 * @implSpec This implementation delegates to {@link #copy(InputStream, OutputStream, long, ProgressListener)}.
	 * @param inputStream The source of the data.
	 * @param outputStream The destination of the data.
	 * @param progressListener A listener to be notified of progress, or <code>null</code> if no progress notifications is requested.
	 * @return The total number of bytes copied.
	 * @throws IOException Thrown if there is an error reading from or writing to a stream.
	 * @throws IOException if an expected content length was given and the number of bytes written to the output stream is not what was expected.
	 */
	public static long copy(final InputStream inputStream, final OutputStream outputStream, final ProgressListener progressListener) throws IOException {
		return copy(inputStream, outputStream, -1, progressListener); //we don't know how many bytes to expect
	}

	/**
	 * Copies all information from an input stream to an output stream. Both streams are used as-is. After copying is finished, both streams are left open.
	 * @implSpec This implementation delegates to {@link #copy(InputStream, OutputStream, long, ProgressListener)}.
	 * @param inputStream The source of the data.
	 * @param outputStream The destination of the data.
	 * @param expectedContentLength The length of content expected, or -1 if the length is unknown.
	 * @return The total number of bytes copied.
	 * @throws IOException Thrown if there is an error reading from or writing to a stream.
	 * @throws IOException if an expected content length was given and the number of bytes written to the output stream is not what was expected.
	 */
	public static long copy(final InputStream inputStream, final OutputStream outputStream, final long expectedContentLength) throws IOException {
		return copy(inputStream, outputStream, expectedContentLength, null); //we don't have a progress listener
	}

	/**
	 * Copies all information from an input stream to an output stream. Both streams are used as-is. After copying is finished, both streams are left open.
	 * @implSpec This implementation uses a single buffer of size {@value #DEFAULT_BUFFER_SIZE}.
	 * @implNote An earlier implementation attempted to optimize by using larger buffers for input streams known to contain a lot of data. Until there is
	 *           empirical or authoritative evidence of any benefit from larger buffer sizes, the current implementation simply uses uses the default buffer size
	 *           {@value #DEFAULT_BUFFER_SIZE} or the known total number of bytes, whichever is smaller. This approach saves memory and the transfer is usually
	 *           I/O bound, not CPU bound, anyway. Using a single, fixed-size buffer is also the approach of the Java 9+
	 *           <code>InputStream.transferTo(OutputStream)</code> method.
	 * @param inputStream The source of the data.
	 * @param outputStream The destination of the data.
	 * @param expectedContentLength The length of content expected, or -1 if the length is unknown.
	 * @param progressListener A listener to be notified of progress, or <code>null</code> if no progress notifications is requested.
	 * @return The total number of bytes copied.
	 * @throws IOException Thrown if there is an error reading from or writing to a stream.
	 * @throws IOException if an expected content length was given and the number of bytes written to the output stream is not what was expected.
	 * @see #DEFAULT_BUFFER_SIZE
	 */
	public static long copy(final InputStream inputStream, final OutputStream outputStream, final long expectedContentLength,
			final ProgressListener progressListener) throws IOException {
		final int bufferSize = expectedContentLength >= 0 && expectedContentLength < DEFAULT_BUFFER_SIZE ? (int)expectedContentLength : DEFAULT_BUFFER_SIZE; //use the default buffer size unless there is a known, smaller expected content length
		final byte[] buffer = new byte[bufferSize]; //create a buffer for copying data
		long totalBytesCopied = 0; //show that we have not copied any data
		int bytesRead; //this will store the number of bytes read each time
		if(progressListener != null) { //if we have a progress listener
			progressListener.progressed(new ProgressEvent(IOStreams.class, bufferSize, 0, expectedContentLength)); //indicate that we are starting but have made no progress
		}
		while((bytesRead = inputStream.read(buffer)) >= 0) { //read bytes until the end of the input stream is reached
			outputStream.write(buffer, 0, bytesRead); //write the bytes to the output stream
			totalBytesCopied += bytesRead; //update the total bytes read
			if(progressListener != null) { //if we have a progress listener
				progressListener.progressed(new ProgressEvent(IOStreams.class, bytesRead, totalBytesCopied, expectedContentLength)); //indicate our progress
			}
		}
		if(expectedContentLength >= 0 && totalBytesCopied != expectedContentLength) { //if we didn't copy what was expected
			throw new IOException(
					"Error transferring information; expected to transfer " + expectedContentLength + " bytes and instead transferred " + totalBytesCopied);
		}
		return totalBytesCopied; //return the total number of bytes copied
	}

}
