/*
 * Copyright © 1996-2012 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static java.util.Objects.*;

/**
 * Wraps an existing input stream.
 * @apiNote The decorated input stream is released when this stream is closed.
 * @implNote This decorator provides convenience methods {@link #beforeClose()} and {@link #afterClose()} called before and after the stream is closed,
 *           respectively.
 * @param <I> The type of input stream being decorated.
 * @author Garret Wilson
 */
public class InputStreamDecorator<I extends InputStream> extends InputStream {

	/** The input stream being decorated. */
	private I inputStream;

	/**
	 * Returns the input stream being decorated.
	 * @return The input stream being decorated, or <code>null</code> if it has been released after this stream was closed.
	 */
	protected I getInputStream() {
		return inputStream;
	}

	/**
	 * Decorates the given input stream.
	 * @param inputStream The input stream to decorate.
	 * @throws NullPointerException if the given stream is <code>null</code>.
	 */
	public InputStreamDecorator(final I inputStream) {
		this.inputStream = requireNonNull(inputStream, "Input stream cannot be null."); //save the decorated input stream
	}

	@Override
	public int read() throws IOException {
		final InputStream inputStream = getInputStream(); //get the decorated input stream
		return inputStream != null ? inputStream.read() : -1; //if there is no decorated input stream, indicate that the stream is closed by returning -1
	}

	@Override
	public int read(byte b[]) throws IOException {
		final InputStream inputStream = getInputStream(); //get the decorated input stream
		return inputStream != null ? inputStream.read(b) : -1; //if there is no decorated input stream, indicate that the stream is closed by returning -1
	}

	@Override
	public int read(byte b[], int off, int len) throws IOException {
		final InputStream inputStream = getInputStream(); //get the decorated input stream
		return inputStream != null ? inputStream.read(b, off, len) : -1; //if there is no decorated input stream, indicate that the stream is closed by returning -1
	}

	@Override
	public long skip(long n) throws IOException {
		final InputStream inputStream = getInputStream(); //get the decorated input stream
		return inputStream != null ? inputStream.skip(n) : 0;
	}

	@Override
	public int available() throws IOException {
		final InputStream inputStream = getInputStream(); //get the decorated input stream
		return inputStream != null ? inputStream.available() : 0;
	}

	@Override
	public synchronized void mark(int readlimit) {
		final InputStream inputStream = getInputStream(); //get the decorated input stream
		if(inputStream != null) { //if we still have an input stream to decorate
			inputStream.mark(readlimit);
		}
	}

	@Override
	public synchronized void reset() throws IOException {
		final InputStream inputStream = getInputStream(); //get the decorated input stream
		if(inputStream != null) { //if we still have an input stream to decorate
			inputStream.reset();
		}
	}

	@Override
	public boolean markSupported() {
		final InputStream inputStream = getInputStream(); //get the decorated input stream
		return inputStream != null ? inputStream.markSupported() : false;
	}

	/**
	 * Called before the stream is closed.
	 * @throws IOException if an I/O error occurs.
	 */
	protected void beforeClose() throws IOException {
	}

	/**
	 * Called after the stream is successfully closed.
	 * @throws IOException if an I/O error occurs.
	 */
	protected void afterClose() throws IOException {
	}

	/**
	 * Closes this input stream and releases any system resources associated with the stream. A closed stream cannot perform output operations and cannot be
	 * reopened.
	 * @implNote This method is synchronized so that the closing operation can complete without being bothered by other threads.
	 * @param closeDecoratedStream Whether the decorated stream should also be closed.
	 * @throws IOException if an I/O error occurs.
	 * @see #beforeClose()
	 * @see #afterClose()
	 */
	public synchronized void close(final boolean closeDecoratedStream) throws IOException {
		final InputStream inputStream = getInputStream(); //get the decorated input stream
		if(inputStream != null) { //if we still have an input stream to decorate
			beforeClose(); //perform actions before closing
			if(closeDecoratedStream) {
				inputStream.close(); //close the decorated input stream
			}
			this.inputStream = null; //release the decorated input stream if closing was successful
			afterClose(); //perform actions after closing
		}
	}

	@Override
	public void close() throws IOException {
		close(true); //close this stream and the underlying stream
	}

}
