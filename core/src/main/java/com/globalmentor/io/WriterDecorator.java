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

import static java.util.Objects.*;

/**
 * Class that wraps an existing writer. All versions of {@link Writer} methods delegate to the decorated writer.
 * @author Garret Wilson
 */
public class WriterDecorator extends Writer {

	/** The wrapped writer. */
	private final Writer writer;

	/** @return The wrapped writer. */
	protected Writer getWriter() {
		return writer;
	}

	/**
	 * Writer constructor with critical sections synchronized on the writer itself.
	 * @param writer The writer being decorated.
	 * @throws NullPointerException if the given writer is <code>null</code>.
	 */
	public WriterDecorator(final Writer writer) {
		super(); //construct the parent class using this class as a lock
		this.writer = requireNonNull(writer, "Writer cannot be null.");
	}

	/**
	 * Writer and lock constructor.
	 * @param writer The writer being decorated.
	 * @param lock The object to synchronize on for operations such as {@link #flush()} and {@link #close()}.
	 * @throws NullPointerException if the given writer and/or lock is <code>null</code>.
	 */
	public WriterDecorator(final Writer writer, final Object lock) {
		super(lock); //construct the parent class with the lock
		this.writer = requireNonNull(writer, "Writer cannot be null.");
	}

	@Override
	public void write(final int c) throws IOException {
		getWriter().write(c);
	}

	@Override
	public void write(final char cbuf[]) throws IOException {
		getWriter().write(cbuf);
	}

	@Override
	public void write(final char cbuf[], final int off, final int len) throws IOException {
		getWriter().write(cbuf, off, len);
	}

	@Override
	public void write(final String str) throws IOException {
		getWriter().write(str);
	}

	@Override
	public void write(final String str, final int off, final int len) throws IOException {
		getWriter().write(str, off, len);
	}

	@Override
	public Writer append(final CharSequence csq) throws IOException {
		getWriter().append(csq);
		return this; //return this writer---not the underlying writer, which is what the decorated writer's version would return
	}

	@Override
	public Writer append(final CharSequence csq, final int start, final int end) throws IOException {
		getWriter().append(csq, start, end);
		return this; //return this writer---not the underlying writer, which is what the decorated writer's version would return
	}

	@Override
	public Writer append(final char c) throws IOException {
		getWriter().append(c);
		return this; //return this writer---not the underlying writer, which is what the decorated writer's version would return
	}

	@Override
	public void flush() throws IOException {
		getWriter().flush();
	}

	@Override
	public void close() throws IOException {
		getWriter().close();
	}

}
