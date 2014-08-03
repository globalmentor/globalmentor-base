/*
 * Copyright © 1996-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import com.globalmentor.java.Disposable;
import com.globalmentor.log.Log;

import static com.globalmentor.java.Objects.*;

/**
 * Wraps an existing input stream. The decorated input stream is released when this stream is closed. This decorator provides convenience methods
 * {@link #beforeClose()} and {@link #afterClose()} called before and after the stream is closed, respectively.
 * @param <I> The type of input stream being decorated.
 * @author Garret Wilson
 */
public class InputStreamDecorator<I extends InputStream> extends InputStream implements Disposable {

	/** Whether the stream should be automatically disposed when closed. */
	private final boolean autoDispose;

	/** The input stream being decorated. */
	private I inputStream;

	/** @return The input stream being decorated, or <code>null</code> if it has been released after this stream was closed. */
	protected I getInputStream() {
		return inputStream;
	}

	/**
	 * Decorates the given input stream, automatically calling {@link #dispose()} when closed.
	 * @param inputStream The input stream to decorate.
	 * @throws NullPointerException if the given stream is <code>null</code>.
	 */
	public InputStreamDecorator(final I inputStream) {
		this(inputStream, true);
	}

	/**
	 * Decorates the given input stream.
	 * @param inputStream The input stream to decorate.
	 * @param autoDispose Whether the stream should be automatically disposed when closed.
	 * @throws NullPointerException if the given stream is <code>null</code>.
	 */
	public InputStreamDecorator(final I inputStream, final boolean autoDispose) {
		this.inputStream = checkInstance(inputStream, "Input stream cannot be null."); //save the decorated input stream
		this.autoDispose = true;
	}

	/** {@inheritDoc} */
	@Override
	public int read() throws IOException {
		final InputStream inputStream = getInputStream(); //get the decorated input stream
		return inputStream != null ? inputStream.read() : -1; //if there is no decorated input stream, indicate that the stream is closed by returning -1
	}

	/** {@inheritDoc} */
	@Override
	public int read(byte b[]) throws IOException {
		final InputStream inputStream = getInputStream(); //get the decorated input stream
		return inputStream != null ? inputStream.read(b) : -1; //if there is no decorated input stream, indicate that the stream is closed by returning -1
	}

	/** {@inheritDoc} */
	@Override
	public int read(byte b[], int off, int len) throws IOException {
		final InputStream inputStream = getInputStream(); //get the decorated input stream
		return inputStream != null ? inputStream.read(b, off, len) : -1; //if there is no decorated input stream, indicate that the stream is closed by returning -1
	}

	/** {@inheritDoc} */
	@Override
	public long skip(long n) throws IOException {
		final InputStream inputStream = getInputStream(); //get the decorated input stream
		return inputStream != null ? inputStream.skip(n) : 0;
	}

	/** {@inheritDoc} */
	@Override
	public int available() throws IOException {
		final InputStream inputStream = getInputStream(); //get the decorated input stream
		return inputStream != null ? inputStream.available() : 0;
	}

	/** {@inheritDoc} */
	@Override
	public synchronized void mark(int readlimit) {
		final InputStream inputStream = getInputStream(); //get the decorated input stream
		if(inputStream != null) { //if we still have an input stream to decorate
			inputStream.mark(readlimit);
		}
	}

	/** {@inheritDoc} */
	@Override
	public synchronized void reset() throws IOException {
		final InputStream inputStream = getInputStream(); //get the decorated input stream
		if(inputStream != null) { //if we still have an input stream to decorate
			inputStream.reset();
		}
	}

	/** {@inheritDoc} */
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
	 * reopened. If auto-dispose is enabled, {@link #dispose()} will be called if closing is successful.
	 * @param closeDecoratedStream Whether the decorated stream should also be closed.
	 * @throws IOException if an I/O error occurs.
	 * @see #beforeClose()
	 * @see #afterClose()
	 * @see #dispose()
	 */
	public synchronized void close(final boolean closeDecoratedStream) throws IOException { //this method is synchronized so that the closing operation can complete without being bothered by other threads
		final InputStream inputStream = getInputStream(); //get the decorated input stream
		if(inputStream != null) { //if we still have an input stream to decorate
			beforeClose(); //perform actions before closing
			if(closeDecoratedStream) {
				inputStream.close(); //close the decorated input stream
			}
			this.inputStream = null; //release the decorated input stream if closing was successful
			afterClose(); //perform actions after closing
			if(autoDispose) {
				dispose(); //dispose of the object
			}
		}
	}

	/**
	 * Closes this input stream and releases any system resources associated with the stream. A closed stream cannot perform output operations and cannot be
	 * reopened.
	 * @throws IOException if an I/O error occurs.
	 * @see #beforeClose()
	 * @see #afterClose()
	 * @see #close(boolean)
	 * @see #dispose()
	 */
	public void close() throws IOException {
		close(true); //close this stream and the underlying stream
	}

	/** {@inheritDoc} This version closes the input stream and releases it, if still available. */
	@Override
	public synchronized void dispose() {
		if(inputStream != null) { //if we still have an input stream
			try {
				inputStream.close();
			} catch(final IOException ioException) {
				Log.error(ioException);
			}
			inputStream = null; //release the decorated output stream
		}
	}

	/** {@inheritDoc} This version calls {@link #dispose()}. */
	@Override
	protected void finalize() throws Throwable {
		try {
			dispose();
		} finally {
			super.finalize(); //always call the parent version
		}
	}
}
