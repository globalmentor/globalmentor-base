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
import static java.lang.Math.*;

/**
 * An input stream that captures all transferred bytes of a decorated stream.
 * @author Garret Wilson
 */
public class CaptureInputStream extends InputStreamDecorator<InputStream> {

	/** The size of the local buffer used for skipping. */
	private final long SKIP_BUFFER_SIZE = 2048;

	/** The byte array output stream that captures transferred data. */
	private ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();

	/** @return The current captured data accumulated from transfers, leaving the data to be retrieved again at a future time. */
	public byte[] getCapturedBytes() {
		return getCapturedBytes(false); //return the captured bytes without clearing the data
	}

	/**
	 * Returns all captured bytes accumulated from transfers since the captured bytes were last cleared. This method allows the captured data to be atomically
	 * cleared at the same time that data is retrieved so that no data is lost.
	 * @param clearCapturedBytes Whether the captured data should be cleared after retrieving the data.
	 * @return The current captured data accumulated from transfers.
	 * @see #clearCapturedBytes()
	 */
	public byte[] getCapturedBytes(final boolean clearCapturedBytes) {
		final byte[] bytes; //we'll store here the bytes we retrieve
		synchronized(byteArrayOutputStream) { //synchronize on the captured bytes
			bytes = byteArrayOutputStream.toByteArray(); //get the accumulated bytes
			if(clearCapturedBytes) { //if we should clear the captured bytes
				byteArrayOutputStream.reset(); //reset the buffer
			}
		}
		return bytes; //return the bytes we retrieved
	}

	/** Clears all accumulated captured bytes. */
	public void clearCapturedBytes() {
		byteArrayOutputStream.reset(); //reset the captured data
	}

	/**
	 * Decorates the given input stream.
	 * @param inputStream The input stream to decorate.
	 * @throws NullPointerException if the given stream is <code>null</code>.
	 */
	public CaptureInputStream(final InputStream inputStream) {
		super(inputStream); //construct the parent class
	}

	@Override
	public int read() throws IOException {
		final int b = super.read(); //read data normally
		if(b >= 0) { //if valid data was read
			byteArrayOutputStream.write(b); //capture the data
		}
		return b; //return the data read
	}

	@Override
	public int read(byte b[]) throws IOException {
		final int count = super.read(b); //read data normally
		if(count > 0) { //if data was read
			byteArrayOutputStream.write(b, 0, count); //capture the data
		}
		return count; //return the amount of data read
	}

	@Override
	public int read(byte b[], int off, int len) throws IOException {
		final int count = super.read(b, off, len); //read data normally
		if(count > 0) { //if data was read
			byteArrayOutputStream.write(b, off, count); //capture the data
		}
		return count; //return the amount of data read
	}

	@Override
	public long skip(final long n) throws IOException {
		final byte[] buffer = new byte[(int)min(n, SKIP_BUFFER_SIZE)]; //make a buffer only as large as needed (we can cast to an int, because we know that at least one of the values is an int, and we're taking the minimum of the two)
		final int bufferSize = buffer.length; //get the length of the buffer
		long bytesLeft = n; //we'll start out needing to read all the bytes
		int bufferBytesRead = 0; //we'll keep track of how many bytes we read each time
		while(bytesLeft > 0 && bufferBytesRead >= 0) { //while there are bytes left and we haven't reached the end of the stream
			bufferBytesRead = read(buffer, 0, (int)min(bytesLeft, bufferSize)); //read as many bytes as we have left, or as many as our buffer can hold, whichever is less; this will also automatically capture our data
			if(bufferBytesRead > 0) { //if we read any bytes at all (this could be negative, so don't blindly subtract; but since we're checking anyway, we might as well throw out the zero case)
				byteArrayOutputStream.write(buffer, 0, bufferBytesRead); //capture the data
				bytesLeft -= bufferBytesRead; //decrease the bytes left by the number read
			}
		}
		return n - bytesLeft; //return the number of bytes we skipped (captured), which will be the total number minus however many we have left to read, if any (if we reached the end of the stream, that is)
	}

}
