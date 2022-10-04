/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

/**
 * An output stream that captures all transferred bytes of a decorated stream.
 * @author Garret Wilson
 */
public class CaptureOutputStream extends OutputStreamDecorator<OutputStream> {

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
	 * Decorates the given output stream.
	 * @param outputStream The output stream to decorate.
	 * @throws NullPointerException if the given stream is <code>null</code>.
	 */
	public CaptureOutputStream(final OutputStream outputStream) {
		super(outputStream); //construct the parent class
	}

	@Override
	public void write(int b) throws IOException {
		byteArrayOutputStream.write(b); //capture the byte
		super.write(b); //do the default writing
	}

	@Override
	public void write(byte b[]) throws IOException {
		byteArrayOutputStream.write(b); //capture the bytes
		super.write(b); //do the default writing
	}

	@Override
	public void write(byte b[], int off, int len) throws IOException {
		byteArrayOutputStream.write(b, off, len); //capture the bytes
		super.write(b, off, len); //do the default writing
	}

}
