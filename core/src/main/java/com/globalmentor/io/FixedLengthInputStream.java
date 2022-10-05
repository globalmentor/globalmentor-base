/*
 * Copyright © 2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static com.globalmentor.java.Conditions.*;

/**
 * Wraps an existing input stream and only returns a fixed number of bytes.
 * @apiNote This stream should always be closed when access is finished; otherwise the underlying stream could be corrupted.
 * @implNote This class is not thread safe.
 * @author Garret Wilson
 */
public class FixedLengthInputStream extends InputStreamDecorator<InputStream> {

	/** The number of bytes left to be read. */
	private long length;

	/** @return The number of bytes left to be read. */
	protected long getLength() {
		return length;
	}

	/** Whether the decorated stream should be closed when this stream is closed. */
	private final boolean closeDecoratedStream;

	/**
	 * Decorates the given input stream. The underlying stream will be closed when this stream is closed.
	 * @param inputStream The input stream to decorate.
	 * @param length The number of bytes to read.
	 * @throws NullPointerException if the given stream is <code>null</code>.
	 * @throws IllegalArgumentException if the given length is less than zero.
	 */
	public FixedLengthInputStream(final InputStream inputStream, final long length) {
		this(inputStream, length, true);
	}

	/**
	 * Decorates the given input stream.
	 * @param inputStream The input stream to decorate.
	 * @param length The number of bytes to read.
	 * @param closeDecoratedStream Whether the decorated stream should be closed when this stream is closed.
	 * @throws NullPointerException if the given stream is <code>null</code>.
	 * @throws IllegalArgumentException if the given length is less than zero.
	 */
	public FixedLengthInputStream(final InputStream inputStream, final long length, final boolean closeDecoratedStream) {
		super(inputStream);
		this.length = checkArgumentNotNegative(length);
		this.closeDecoratedStream = closeDecoratedStream;
	}

	@Override
	public int read() throws IOException {
		if(length == 0) { //if we've reached the end of our stream
			return -1;
		}
		final int b = super.read();
		if(b >= 0) { //if we haven't reached the end of the stream
			--length; //indicate that we've read another byte
		}
		return b;
	}

	@Override
	public final int read(byte b[]) throws IOException {
		return read(b, 0, b.length); //let the other method take care of the fixed length
	}

	@Override
	public final int read(byte b[], int off, int len) throws IOException {
		if(length == 0) { //if we've reached the end of our stream
			return -1;
		}
		if(len > length) { //if they want to read more than we have
			len = (int)length; //only read what we have
		}
		int count = super.read(b, off, len); //read the data
		if(count >= 0) { //if we haven't reached the end of the stream
			length -= count; //note that we've read however many bytes 
		}
		return count;
	}

	@Override
	public long skip(long n) throws IOException {
		if(length == 0) { //if we've reached the end of our stream
			return 0;
		}
		if(n > length) { //if they want to skip more than we have
			n = length; //only skip what we have
		}
		long count = super.skip(n); //skip the data
		if(count >= 0) { //if we haven't reached the end of the stream
			length -= count; //note that we've skipped however many bytes 
		}
		return count;
	}

	@Override
	public int available() throws IOException {
		return (int)Math.min(super.available(), length); //don't return that more is available that our length 
	}

	@Override
	public void close() throws IOException {
		if(getInputStream() != null && length > 0) { //if we aren't already closed and we have more bytes left
			skip(length); //skip the remaining bytes
		}
		close(closeDecoratedStream); //close this stream and optionally the underlying string, as configured
	}

}
