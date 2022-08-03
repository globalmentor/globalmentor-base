/*
 * Copyright © 2022 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static java.util.Objects.*;

import java.io.*;

import javax.annotation.*;

/**
 * Converts a {@link PrintStream} to a {@link Writer}.
 * <p>
 * This class delegates to {@link PrintStream#print(String)} for writing strings, maintaining the charset of the underlying print stream. This is especially
 * important to maintain the charset of {@link System#out} and {@link System#err}, which virtually impossible to determine especially in earlier versions of
 * Java. Contrast this with {@link PrintWriter}, which does not maintain the underlying charset of the {@link PrintStream}.
 * </p>
 * @apiNote Note that if {@link System#out} or {@link System#err} is being redirected, the charset may continue to be set to the console charset, which may not
 *          necessarily be the system default charset and may not be appropriate for saving to a file. This is a Java limitation and is for the most part
 *          unavoidable. It may thus be better to avoid redirecting output and instead provide options for explicitly saving to an external file with some
 *          specified or default charset. See <a href="https://stackoverflow.com/q/72435634">How to find charset of System.err if stdout is redirected?</a> for
 *          more discussion.
 * @author Garret Wilson
 * @see System#out
 * @see System#err
 */
public class PrintStreamWriter extends Writer {

	private final PrintStream printStream;

	private final boolean closeStreamOnClose;

	/**
	 * Print stream constructor.
	 * @apiNote If writing to {@link System#out} or {@link System#err} it probably more appropriate to use the {@link #PrintStreamWriter(PrintStream, boolean)}
	 *          constructor with a close argument of <code>false</code>.
	 * @param printStream The print stream to convert.
	 * @see <a href="https://stackoverflow.com/q/7457723">Why don't we close `System.out` Stream after using it?</a>
	 */
	public PrintStreamWriter(@Nonnull final PrintStream printStream) {
		this(printStream, printStream != System.out && printStream != System.err);
	}

	/**
	 * Print stream and optional closing constructor.
	 * @apiNote Setting the close option to <code>false</code> is useful if wrapping {@link System#out} or {@link System#err}, as these streams are provided by
	 *          the JVM and should not be closed by the application.
	 * @param printStream The print stream to convert.
	 * @param closeStreamOnClose <code>true</code> if the given print stream should be closed when the writer is closed, or <code>false</code> if the print stream
	 *          should be left open.
	 * @see <a href="https://stackoverflow.com/q/7457723">Why don't we close `System.out` Stream after using it?</a>
	 */
	public PrintStreamWriter(@Nonnull final PrintStream printStream, boolean closeStreamOnClose) {
		this.printStream = requireNonNull(printStream);
		this.closeStreamOnClose = closeStreamOnClose;

	}

	@Override
	public void write(final int c) throws IOException {
		if(c <= Character.MAX_VALUE) {
			printStream.print((char)c);
		} else {
			printStream.print(Character.toChars(c));
		}
	}

	@Override
	public void write(final char cbuf[]) throws IOException {
		printStream.print(cbuf);
	}

	@Override
	public void write(final char cbuf[], final int off, final int len) throws IOException {
		printStream.print(new String(cbuf, off, len));
	}

	@Override
	public void write(final String str) throws IOException {
		printStream.print(str);
	}

	@Override
	public void write(final String str, final int off, final int len) throws IOException {
		printStream.print(str.substring(off, off + len));
	}

	@Override
	public void flush() throws IOException {
		printStream.flush();
	}

	@Override
	public void close() throws IOException {
		if(closeStreamOnClose) {
			printStream.close();
		} else {
			printStream.flush();
		}
	}

}
