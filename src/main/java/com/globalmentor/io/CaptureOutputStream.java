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

	/**
	 * Writes the specified byte to this output stream. The general contract for <code>write</code> is that one byte is written to the output stream. The byte to
	 * be written is the eight low-order bits of the argument <code>b</code>. The 24 high-order bits of <code>b</code> are ignored.
	 * <p>
	 * Subclasses of <code>OutputStream</code> must provide an implementation for this method.
	 *
	 * @param b the <code>byte</code>.
	 * @throws IOException if an I/O error occurs. In particular, an <code>IOException</code> may be thrown if the output stream has been closed.
	 */
	public void write(int b) throws IOException {
		byteArrayOutputStream.write(b); //capture the byte
		super.write(b); //do the default writing
	}

	/**
	 * Writes <code>b.length</code> bytes from the specified byte array to this output stream. The general contract for <code>write(b)</code> is that it should
	 * have exactly the same effect as the call <code>write(b, 0, b.length)</code>.
	 *
	 * @param b the data.
	 * @throws IOException if an I/O error occurs.
	 * @see java.io.OutputStream#write(byte[], int, int)
	 */
	public void write(byte b[]) throws IOException {
		byteArrayOutputStream.write(b); //capture the bytes
		super.write(b); //do the default writing
	}

	/**
	 * Writes <code>len</code> bytes from the specified byte array starting at offset <code>off</code> to this output stream. The general contract for
	 * <code>write(b, off, len)</code> is that some of the bytes in the array <code>b</code> are written to the output stream in order; element
	 * <code>b[off]</code> is the first byte written and <code>b[off+len-1]</code> is the last byte written by this operation.
	 * <p>
	 * The <code>write</code> method of <code>OutputStream</code> calls the write method of one argument on each of the bytes to be written out. Subclasses are
	 * encouraged to override this method and provide a more efficient implementation.
	 * <p>
	 * If <code>b</code> is <code>null</code>, a <code>NullPointerException</code> is thrown.
	 * <p>
	 * If <code>off</code> is negative, or <code>len</code> is negative, or <code>off+len</code> is greater than the length of the array <code>b</code>, then an
	 * <tt>IndexOutOfBoundsException</tt> is thrown.
	 *
	 * @param b the data.
	 * @param off the start offset in the data.
	 * @param len the number of bytes to write.
	 * @throws IOException if an I/O error occurs. In particular, an <code>IOException</code> is thrown if the output stream is closed.
	 */
	public void write(byte b[], int off, int len) throws IOException {
		byteArrayOutputStream.write(b, off, len); //capture the bytes
		super.write(b, off, len); //do the default writing
	}

}
