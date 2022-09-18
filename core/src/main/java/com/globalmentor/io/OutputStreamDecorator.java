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

import static java.util.Objects.*;

import com.globalmentor.java.Disposable;

/**
 * Wraps an existing output stream.
 * @apiNote The decorated output stream is released when this stream is closed.
 * @implNote This decorator provides convenience methods {@link #beforeClose()} and {@link #afterClose()} called before and after the stream is closed,
 *           respectively.
 * @param <O> The type of output stream being decorated.
 * @author Garret Wilson
 */
public class OutputStreamDecorator<O extends OutputStream> extends OutputStream implements Disposable {

	/** Whether the stream should be automatically disposed when closed. */
	private final boolean autoDispose;

	/** The output stream being decorated. */
	private O outputStream;

	/** @return The output stream being decorated, or <code>null</code> if it has been released after this stream was closed. */
	protected O getOutputStream() {
		return outputStream;
	}

	/**
	 * Changes the decorated output stream.
	 * <p>
	 * This method can be used by child classes to change the decorated output stream, but cannot be used to remove the output stream---this can be done only by
	 * calling {@link #close()}.
	 * </p>
	 * @param outputStream The new output stream to decorate.
	 * @throws NullPointerException if the given output stream is <code>null</code>.
	 */
	protected void setOutputStream(final O outputStream) {
		this.outputStream = requireNonNull(outputStream);
	}

	/**
	 * Decorates the given output stream, automatically calling {@link #dispose()} when closed.
	 * @param outputStream The output stream to decorate.
	 * @throws NullPointerException if the given stream is <code>null</code>.
	 */
	public OutputStreamDecorator(final O outputStream) {
		this(outputStream, true);
	}

	/**
	 * Decorates the given output stream.
	 * @param outputStream The output stream to decorate.
	 * @param autoDispose Whether the stream should be automatically disposed when closed.
	 * @throws NullPointerException if the given stream is <code>null</code>.
	 */
	public OutputStreamDecorator(final O outputStream, final boolean autoDispose) {
		this.outputStream = requireNonNull(outputStream, "Output stream cannot be null."); //save the decorated output stream
		this.autoDispose = true;
	}

	@Override
	public void write(int b) throws IOException {
		checkOutputStream().write(b);
	}

	@Override
	public void write(byte b[]) throws IOException {
		checkOutputStream().write(b);
	}

	@Override
	public void write(byte b[], int off, int len) throws IOException {
		checkOutputStream().write(b, off, len);
	}

	@Override
	public void flush() throws IOException {
		checkOutputStream().flush();
	}

	/**
	 * Checks to make sure the decorated output stream is available.
	 * @return The decorated output stream.
	 * @throws IOException if there is no output stream, indicating that the stream is already closed.
	 */
	protected OutputStream checkOutputStream() throws IOException {
		final OutputStream outputStream = getOutputStream(); //get the decorated output stream
		if(outputStream == null) { //if this stream is closed
			throw new IOException("Stream already closed.");
		}
		return outputStream;
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
	 * Closes this output stream and releases any system resources associated with the stream. A closed stream cannot perform output operations and cannot be
	 * reopened. If auto-dispose is enabled, {@link #dispose()} will be called if closing is successful.
	 * @param closeDecoratedStream Whether the decorated stream should also be closed.
	 * @throws IOException if an I/O error occurs.
	 * @see #beforeClose()
	 * @see #afterClose()
	 * @see #dispose()
	 */
	public synchronized void close(final boolean closeDecoratedStream) throws IOException { //this method is synchronized so that the closing operation can complete without being bothered by other threads
		final OutputStream outputStream = getOutputStream(); //get the decorated output stream
		if(outputStream != null) { //if we still have an output stream to decorate
			beforeClose(); //perform actions before closing
			if(closeDecoratedStream) {
				outputStream.close(); //close the decorated output stream
			}
			this.outputStream = null; //release the decorated output stream if closing was successful---even if we didn't close it (because we weren't requested to)
			afterClose(); //perform actions after closing
			if(autoDispose) {
				dispose(); //dispose of the object
			}
		}
	}

	@Override
	public void close() throws IOException {
		close(true); //close this stream and the underlying stream
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This version closes the output stream and releases it, if still available.
	 */
	@Override
	public synchronized void dispose() {
		if(outputStream != null) { //if we still have an output stream
			try {
				outputStream.close();
			} catch(final IOException ioException) {
				//TODO fix log; fix/consolidate Disposable: Log.error(ioException);
			}
			outputStream = null; //release the decorated output stream
		}
	}

}
