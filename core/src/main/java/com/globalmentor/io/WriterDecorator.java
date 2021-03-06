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

	/**
	 * Write a single character. The character to be written is contained in the 16 low-order bits of the given integer value; the 16 high-order bits are ignored.
	 *
	 * <p>
	 * Subclasses that intend to support efficient single-character output should override this method.
	 *
	 * @param c int specifying a character to be written.
	 * @throws IOException If an I/O error occurs
	 */
	public void write(int c) throws IOException {
		getWriter().write(c);
	}

	/**
	 * Write an array of characters.
	 *
	 * @param cbuf Array of characters to be written
	 *
	 * @throws IOException If an I/O error occurs
	 */
	public void write(char cbuf[]) throws IOException {
		getWriter().write(cbuf);
	}

	/**
	 * Write a portion of an array of characters.
	 *
	 * @param cbuf Array of characters
	 * @param off Offset from which to start writing characters
	 * @param len Number of characters to write
	 *
	 * @throws IOException If an I/O error occurs
	 */
	public void write(char cbuf[], int off, int len) throws IOException {
		getWriter().write(cbuf, off, len);
	}

	/**
	 * Write a string.
	 *
	 * @param str String to be written
	 *
	 * @throws IOException If an I/O error occurs
	 */
	public void write(String str) throws IOException {
		getWriter().write(str);
	}

	/**
	 * Write a portion of a string.
	 *
	 * @param str A String
	 * @param off Offset from which to start writing characters
	 * @param len Number of characters to write
	 *
	 * @throws IOException If an I/O error occurs
	 */
	public void write(String str, int off, int len) throws IOException {
		getWriter().write(str, off, len);
	}

	/**
	 * Appends the specified character sequence to this writer.
	 *
	 * <p>
	 * An invocation of this method of the form <code>out.append(csq)</code> behaves in exactly the same way as the invocation
	 *
	 * <pre>
	 * out.write(csq.toString())
	 * </pre>
	 *
	 * <p>
	 * Depending on the specification of <code>toString</code> for the character sequence <code>csq</code>, the entire sequence may not be appended. For instance,
	 * invoking the <code>toString</code> method of a character buffer will return a subsequence whose content depends upon the buffer's position and limit.
	 *
	 * @param csq The character sequence to append. If <code>csq</code> is <code>null</code>, then the four characters <code>"null"</code> are appended to this writer.
	 *
	 * @return This writer
	 *
	 * @throws IOException If an I/O error occurs
	 *
	 * @since 1.5
	 */
	public Writer append(CharSequence csq) throws IOException {
		getWriter().append(csq);
		return this;
	} //return this writer---not the underlying writer, which is what the decorated writer's version would return

	/**
	 * Appends a subsequence of the specified character sequence to this writer. <code>Appendable</code>.
	 *
	 * <p>
	 * An invocation of this method of the form <code>out.append(csq, start,
	 * end)</code> when <code>csq</code> is not <code>null</code> behaves in exactly the same way as the invocation
	 *
	 * <pre>
	 * out.write(csq.subSequence(start, end).toString())
	 * </pre>
	 *
	 * @param csq The character sequence from which a subsequence will be appended. If <code>csq</code> is <code>null</code>, then characters will be appended as if
	 *          <code>csq</code> contained the four characters <code>"null"</code>.
	 *
	 * @param start The index of the first character in the subsequence
	 *
	 * @param end The index of the character following the last character in the subsequence
	 *
	 * @return This writer
	 *
	 * @throws IndexOutOfBoundsException If <code>start</code> or <code>end</code> are negative, <code>start</code> is greater than <code>end</code>, or <code>end</code> is greater
	 *           than <code>csq.length()</code>
	 *
	 * @throws IOException If an I/O error occurs
	 *
	 * @since 1.5
	 */
	public Writer append(CharSequence csq, int start, int end) throws IOException {
		getWriter().append(csq, start, end);
		return this;
	} //return this writer---not the underlying writer, which is what the decorated writer's version would return

	/**
	 * Appends the specified character to this writer.
	 *
	 * <p>
	 * An invocation of this method of the form <code>out.append(c)</code> behaves in exactly the same way as the invocation
	 *
	 * <pre>
	 * out.write(c)
	 * </pre>
	 *
	 * @param c The 16-bit character to append
	 *
	 * @return This writer
	 *
	 * @throws IOException If an I/O error occurs
	 *
	 * @since 1.5
	 */
	public Writer append(char c) throws IOException {
		getWriter().append(c);
		return this;
	} //return this writer---not the underlying writer, which is what the decorated writer's version would return

	/**
	 * Flush the stream. If the stream has saved any characters from the various write() methods in a buffer, write them immediately to their intended
	 * destination. Then, if that destination is another character or byte stream, flush it. Thus one flush() invocation will flush all the buffers in a chain of
	 * Writers and OutputStreams.
	 * <p>
	 * If the intended destination of this stream is an abstraction provided by the underlying operating system, for example a file, then flushing the stream
	 * guarantees only that bytes previously written to the stream are passed to the operating system for writing; it does not guarantee that they are actually
	 * written to a physical device such as a disk drive.
	 *
	 * @throws IOException If an I/O error occurs
	 */
	public void flush() throws IOException {
		getWriter().flush();
	}

	/**
	 * Close the stream, flushing it first. Once a stream has been closed, further write() or flush() invocations will cause an IOException to be thrown. Closing
	 * a previously-closed stream, however, has no effect.
	 *
	 * @throws IOException If an I/O error occurs
	 */
	public void close() throws IOException {
		getWriter().close();
	}

}
