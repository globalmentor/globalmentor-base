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
import java.util.*;

import static java.util.Objects.*;

import com.globalmentor.java.OperatingSystem;

import static com.globalmentor.java.OperatingSystem.*;

/**
 * A print stream that writes to a given writer. This print stream writer assumes that all bytes written to it are characters encoded in the platform default
 * character encoding.
 * @author Garret Wilson
 */
public class WriterPrintStream extends PrintStream {

	/** The system line separator characters in use when the class is created. */
	private static final String LINE_SEPARATOR = getLineSeparator();

	/** The writer to which print stream information will be written. */
	private final Writer writer;

	/** Whether flushing should occur automatically after each write. */
	private final boolean autoflush;

	/** The lazily-created, cached formatter. */
	private Formatter formatter = null;

	/** Whether an error has occurred. */
	private boolean isError = false;

	/**
	 * Writer constructor. The stream will not flush automatically when the end of a line is written.
	 * @param writer The writer to which values and objects will be printed.
	 * @throws NullPointerException if the given writer is <code>null</code>.
	 */
	public WriterPrintStream(final Writer writer) {
		this(writer, false); //construct the print stream with autoflush turned off
	}

	/**
	 * Writer and autoflush constructor.
	 * @param writer The writer to which values and objects will be printed.
	 * @param autoFlush <code>true</code> writer should be flushed whenever a byte array is written, one of the <code>println</code> methods is invoked, or a
	 *          newline character or byte (<code>'\n'</code>) is written.
	 * @throws NullPointerException if the given writer is <code>null</code>.
	 */
	public WriterPrintStream(final Writer writer, final boolean autoFlush) {
		super(new NullOutputStream()); //construct the parent class with an output stream that does nothing; this class is just an adapter to reroute the data to the writer, so the underlying output stream isn't needed
		this.writer = requireNonNull(writer, "Writer cannot be null.");
		this.autoflush = autoFlush;
	}

	/**
	 * Flushes the stream. This version delegates to the decorated writer.
	 */
	public void flush() {
		try {
			synchronized(this) {
				writer.flush();
			}
		} catch(final InterruptedIOException interruptedIOException) { //if we were interrupted
			Thread.currentThread().interrupt(); //interrupt the current thread
		} catch(final IOException ioException) { //if any other I/O exception occurred
			isError = true; //indicate that there is an error
		}
	}

	/**
	 * Closes the stream. This version delegates to the decorated writer.
	 */
	public void close() {
		try {
			synchronized(this) {
				writer.close();
			}
		} catch(final InterruptedIOException interruptedIOException) { //if we were interrupted
			Thread.currentThread().interrupt(); //interrupt the current thread
		} catch(final IOException ioException) { //if any other I/O exception occurred
			isError = true; //indicate that there is an error
		}
	}

	/**
	 * Flushes the stream and check its error state.
	 * @return <code>true</code> if and only if this stream has encountered an {@link IOException} other than {@link InterruptedIOException}, or the
	 *         {@link #setError()} method has been invoked.
	 */
	public boolean checkError() {
		flush(); //flush the output
		return isError; //return whether there has been an error
	}

	/** Set the error state of the stream to <code>true</code>. */
	protected void setError() {
		isError = true; //show that there is an error
	}

	/**
	 * Write the specified byte to this stream. If the byte is a newline and automatic flushing is enabled then the {@link #flush()} method will be invoked. This
	 * implementation assumes the given byte specifies a single character.
	 * @param b The byte to be written
	 * @see #print(char)
	 * @see #println(char)
	 */
	public void write(final int b) {
		try {
			synchronized(this) {
				writer.write(b);
				if(b == '\n' && autoflush) { //if this is a newline and we should automatically flush
					flush(); //delegate to the flush method
				}
			}
		} catch(final InterruptedIOException interruptedIOException) { //if we were interrupted
			Thread.currentThread().interrupt(); //interrupt the current thread
		} catch(final IOException ioException) { //if any other I/O exception occurred
			isError = true; //indicate that there is an error
		}
	}

	/**
	 * Write the specified number of bytes bytes from the specified byte array starting at the specified offset. If automatic flushing is enabled then the
	 * {@link #flush()} method will be invoked. Because a {@link PrintWriter} is expected to be used for writing characters rather than bytes, and uses the
	 * platform's default character encoding, this version converts the bytes back to characters using the platform's default character encoding.
	 * @param buffer A byte array.
	 * @param offset The offset from which to start taking bytes.
	 * @param length The number of bytes to write.
	 */
	public void write(final byte buffer[], final int offset, final int length) {
		write(new String(buffer, offset, length), autoflush); //write a string constructed from the bytes using the system default character encoding, because we assume that's how the bytes got there in the first place; flush if autoflush is turned on
	}

	/**
	 * Writes a string to the underlying writer, optionally flushing the writer.
	 * @param string The string to write.
	 * @param flush <code>true</code> if the writer should be flushed afterwards.
	 */
	protected void write(final String string, final boolean flush) {
		try {
			synchronized(this) {
				writer.write(string); //write the string
				if(flush) { //if we've been instructed to flush
					writer.flush(); //flush the writer
				}
			}
		} catch(final InterruptedIOException interruptedIOException) { //if we were interrupted
			Thread.currentThread().interrupt(); //interrupt the current thread
		} catch(final IOException ioException) { //if any other I/O exception occurred
			isError = true; //indicate that there is an error
		}
	}

	/**
	 * Writes a string to the underlying writer. If automatic flushing is enabled and the string contains the newline character '\\n' then the {@link #flush()}
	 * method will be invoked.
	 * @param string The string to write.
	 */
	protected void write(final String string) {
		write(string, autoflush && string.indexOf('\n') >= 0); //write the string; we should flush if autoflush is turned on and there is a newline in the string
	}

	/**
	 * Writes a newline character to the underlying writer. If automatic flushing is enabled then the {@link #flush()} method will be invoked.
	 */
	private void newLine() {
		write(LINE_SEPARATOR, autoflush); //write the line separator string, flushing if autoflushing is turned on
	}

	/* Methods that do not terminate lines */

	/**
	 * Print a boolean value.
	 * @param b The bolean value to be printed
	 */
	public void print(final boolean b) {
		write(Boolean.valueOf(b).toString()); //use the TRUE/FALSE Boolean object string values
	}

	/**
	 * Print a character.
	 * @param c The character to be printed.
	 */
	public void print(final char c) {
		write(String.valueOf(c)); //write the string value of the character, which take the correct action if the character is a newline
	}

	/**
	 * Print an integer. The string used is the output of {@link java.lang.String#valueOf(int)}.
	 * @param i The integer to be printed.
	 * @see java.lang.Integer#toString(int)
	 */
	public void print(final int i) {
		write(String.valueOf(i));
	}

	/**
	 * Print a long integer. The string used is the output of {@link java.lang.String#valueOf(long)}.
	 * @param l The long to be printed.
	 * @see java.lang.Long#toString(long)
	 */
	public void print(final long l) {
		write(String.valueOf(l));
	}

	/**
	 * Print a floating-point number. The string used is the output of {@link java.lang.String#valueOf(float)}.
	 * @param f The float to be printed.
	 * @see java.lang.Float#toString(float)
	 */
	public void print(final float f) {
		write(String.valueOf(f));
	}

	/**
	 * Print a double-precision floating-point number. The string used is the output of {@link java.lang.String#valueOf(double)}.
	 * @param d The double to be printed.
	 * @see java.lang.Double#toString(double)
	 */
	public void print(final double d) {
		write(String.valueOf(d));
	}

	/**
	 * Print an array of characters.
	 * @param charArray The array of characters to be printed.
	 * @throws NullPointerException if the array is <code>null</code>.
	 */
	public void print(final char[] charArray) {
		write(new String(charArray)); //create a string from the character array; although this is not as efficient as it could be, it reduces code duplication; print stream classes aren't very efficient in general anyway, and this adapter class is likely used in an inefficient situation to begin with
	}

	/**
	 * Print a string. If the argument is <code>null</code> then the string "null" is printed.
	 * @param string The string to be printed, or <code>null</code> if "null" should be printed.
	 */
	public void print(final String string) {
		write(string != null ? string : "null"); //write the string, or "null" if the string is null
	}

	/**
	 * Print an object. The string used is the output of {@link java.lang.String#valueOf(Object)}.
	 * @param object The object to be printed.
	 * @see java.lang.Object#toString()
	 */
	public void print(final Object object) {
		write(String.valueOf(object));
	}

	/* Methods that do terminate lines */

	/**
	 * Terminate the current line by writing the line separator string. The line separator string is defined by the system property
	 * {@value OperatingSystem#LINE_SEPARATOR_PROPERTY}, and is not necessarily a single newline character (<code>'\n'</code>). This implementation delegates to
	 * {@link #newLine()}.
	 */
	public void println() {
		newLine();
	}

	/**
	 * Print a boolean and then terminates the line. This method behaves as though it invokes {@link #print(boolean)} and then {@link #println()}.
	 * @param b The boolean to be printed
	 */
	public void println(final boolean b) {
		synchronized(this) {
			print(b);
			newLine();
		}
	}

	/**
	 * Print a character and then terminates the line. This method behaves as though it invokes {@link #print(char)} and then {@link #println()}.
	 * @param c The character to be printed.
	 */
	public void println(final char c) {
		synchronized(this) {
			print(c);
			newLine();
		}
	}

	/**
	 * Print an integer and then terminates the line. This method behaves as though it invokes {@link #print(int)} and then {@link #println()}.
	 * @param x The integer to be printed.
	 */
	public void println(final int x) {
		synchronized(this) {
			print(x);
			newLine();
		}
	}

	/**
	 * Print a long and then terminates the line. This method behaves as though it invokes {@link #print(long)} and then {@link #println()}.
	 * @param l The long to be printed.
	 */
	public void println(final long l) {
		synchronized(this) {
			print(l);
			newLine();
		}
	}

	/**
	 * Print a float and then terminates the line. This method behaves as though it invokes {@link #print(float)} and then {@link #println()}.
	 * @param x The float to be printed.
	 */
	public void println(final float x) {
		synchronized(this) {
			print(x);
			newLine();
		}
	}

	/**
	 * Print a double and then terminates the line. This method behaves as though it invokes {@link #print(double)} and then {@link #println()}.
	 * @param d The double to be printed.
	 */
	public void println(final double d) {
		synchronized(this) {
			print(d);
			newLine();
		}
	}

	/**
	 * Print an array of characters and then terminates the line. This method behaves as though it invokes {@link #print(char[])} and then
	 * <code>{@link #println()}</code>.
	 * @param charArray An array of characters to print.
	 */
	public void println(final char[] charArray) {
		synchronized(this) {
			print(charArray);
			newLine();
		}
	}

	/**
	 * Print a string and then terminates the line. This method behaves as though it invokes {@link #print(String)} and then {@link #println()}.
	 * @param string The string to be printed.
	 */
	public void println(final String string) {
		synchronized(this) {
			print(string);
			newLine();
		}
	}

	/**
	 * Print an object and then terminates the line. This method behaves as though it invokes {@link #print(Object)} and then {@link #println()}.
	 * @param object The object to be printed.
	 */
	public void println(final Object object) {
		synchronized(this) {
			print(object);
			newLine();
		}
	}

	/**
	 * Writes a formatted string to this output stream using the specified format string and arguments.
	 * <p>
	 * The locale always used is the one returned by {@link java.util.Locale#getDefault() Locale.getDefault()}, regardless of any previous invocations of other
	 * formatting methods on this object.
	 * </p>
	 * @param format A format string as described in <a href="../util/Formatter.html#syntax">Format string syntax</a>.
	 * @param args Arguments referenced by the format specifiers in the format string.
	 * @throws IllegalFormatException if a format string contains an illegal syntax, a format specifier that is incompatible with the given arguments,
	 *           insufficient arguments given the format string, or other illegal conditions.
	 * @throws NullPointerException If the format is <code>null</code>.
	 * @return This output stream.
	 */
	public PrintStream format(final String format, final Object... args) {
		synchronized(this) {
			if((formatter == null) || (formatter.locale() != Locale.getDefault())) { //if there's no formatter or the formatter is for a different locale
				formatter = new Formatter((Appendable)this); //create a new formatter
			}
			formatter.format(Locale.getDefault(), format, args); //format the information into this print stream.
		}
		return this; //return this print stream
	}

	/**
	 * Writes a formatted string to this output stream using the specified format string and arguments.
	 * @param locale The {@linkplain java.util.Locale locale} to apply during formatting, or <code>null</code> if no localization should be applied.
	 * @param format A format string as described in <a href="../util/Formatter.html#syntax">Format string syntax</a>
	 * @param args Arguments referenced by the format specifiers in the format string.
	 * @throws IllegalFormatException if a format string contains an illegal syntax, a format specifier that is incompatible with the given arguments,
	 *           insufficient arguments given the format string, or other illegal conditions.
	 * @throws NullPointerException If the format is <code>null</code>.
	 * @return This output stream.
	 */
	public WriterPrintStream format(final Locale locale, final String format, final Object... args) {
		synchronized(this) {
			if((formatter == null) || (formatter.locale() != locale)) { //if there's no formatter or the formatter is for a different locale
				formatter = new Formatter(this, locale); //create a new formatter
			}
			formatter.format(locale, format, args); //format the information into this print stream.
		}
		return this; //return this print stream
	}

}
