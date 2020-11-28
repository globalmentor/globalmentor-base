/*
 * Copyright © 1996-2013 GlobalMentor, Inc. <http://www.globalmentor.com/>
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
import java.nio.charset.Charset;

import static com.globalmentor.io.IOStreams.DEFAULT_BUFFER_SIZE;
import static com.globalmentor.java.Conditions.*;
import static java.lang.String.format;
import static java.lang.System.*;
import java.util.*;

import com.globalmentor.java.Bytes;

/**
 * Class to manipulate input streams.
 * @author Garret Wilson
 */
public class InputStreams {

	/** The shared instance of an input stream with no content. */
	public static final InputStream EMPTY_INPUT_STREAM = new EmptyInputStream();

	/** The initial size of buffers in the sequence when reading bytes. */
	static final int INITIAL_READ_BUFFER_SIZE = DEFAULT_BUFFER_SIZE;

	/** The maximum size of buffers in the sequence when reading bytes. */
	static final int MAX_READ_BUFFER_SIZE = INITIAL_READ_BUFFER_SIZE * (1 << 7); //8192 * 128 = 1048576 (~1MB)

	/**
	 * Loads the contents of an input stream into an array of bytes. The input stream will be left open after this operation.
	 * @implSpec This implementation creates a series of smaller buffers and, once the end of the stream has been reached, creates a new buffer and copies the
	 *           contents of each buffer into the new buffer. The buffer size starts at {@value #INITIAL_READ_BUFFER_SIZE} and doubles, up to a maximum of
	 *           {@value #MAX_READ_BUFFER_SIZE}, each time a new buffer is created.
	 * @implNote Using multiple small buffers is assumed to be faster (though it temporarily uses more memory) than using {@link ByteArrayOutputStream} because
	 *           that class copies all bytes that have been read each time the buffer needs to be expanded.
	 * @implNote There have been <a href="https://stackoverflow.com/a/8381338">claims</a> that some JVMs may not be able to allocate arrays of
	 *           {@link Integer#MAX_VALUE}; This method may fail reading slightly less than {@link Integer#MAX_VALUE} bytes if there are JVM-dependent limitations
	 *           on byte array length.
	 * @param inputStream The input stream from which to read.
	 * @return An array of bytes from the input stream.
	 * @throws IOException Thrown if there is an error loading the bytes.
	 * @throws OutOfMemoryError if an array of the required size cannot be allocated, either because not enough memory is available or the JVM cannot otherwise
	 *           create an array with the required number of bytes.
	 */
	public static byte[] readBytes(final InputStream inputStream) throws IOException {
		final List<byte[]> bufferList = new ArrayList<byte[]>(); //create a list for the buffers
		int lastBufferSize = 0; //we haven't used any buffers yet
		int lastBufferBytesRead; //we'll use this to store the number of bytes read into the most recent buffer
		int totalBytesRead = 0; //show that we haven't read anything at all yet
		do {
			if(lastBufferSize < MAX_READ_BUFFER_SIZE) { //double the buffer size each time until we reach the max size
				lastBufferSize = lastBufferSize == 0 ? INITIAL_READ_BUFFER_SIZE : lastBufferSize * 2; //use the initial buffer size for the first buffer
			}
			final byte[] buffer = new byte[lastBufferSize]; //create a new buffer (the "last buffer" for the moment is the "current buffer")
			lastBufferBytesRead = read(inputStream, buffer); //read bytes into the buffer
			if(Integer.MAX_VALUE - totalBytesRead < lastBufferBytesRead) { //if the additional bytes just read is greater than those remaining before we hit the total aggregate buffer size limit
				throw new OutOfMemoryError(format("Read %d + %d bytes; total greater than maximum array length %d.", totalBytesRead, lastBufferBytesRead));
			}
			totalBytesRead += lastBufferBytesRead; //update our total bytes read
			bufferList.add(buffer); //add this buffer to our list of buffers
		} while(lastBufferBytesRead == lastBufferSize); //keep adding new buffers until we run out of bytes (leaving one buffer not fully filled)
		final byte[] finalBuffer = new byte[totalBytesRead]; //create a buffer of the correct length of all the bytes we've read
		int bytesCopied = 0; //show that we haven't copied any bytes yet
		final int bufferCount = bufferList.size();
		for(int i = 0; i < bufferCount; ++i) { //look at each of our buffers
			final byte[] buffer = bufferList.get(i);
			//all the buffers should be full except for the last one, which will only hold the number of bytes last read
			final int bytesToCopy = (i < bufferCount - 1) ? buffer.length : lastBufferBytesRead;
			arraycopy(buffer, 0, finalBuffer, bytesCopied, bytesToCopy); //copy bytes from this buffer to our final buffer
			bytesCopied += bytesToCopy; //show that we copied this many bytes
		}
		return finalBuffer; //return the final buffer
	}

	/**
	 * Reads a specified number of bytes from the stream and returns them in a byte array.
	 * @param inputStream The input stream from which to read.
	 * @param length The number of bytes to read
	 * @return An array of bytes containing the read characters, which may be shorter than the length specified if the number of bytes actually available were
	 *         less than requested.
	 * @throws IllegalArgumentException if the offset or the length is negative.
	 * @throws IOException if there is an error reading from the input stream.
	 */
	public static byte[] readBytes(final InputStream inputStream, final int length) throws IOException {
		return readBytes(inputStream, 0, length); //get bytes from the stream starting at the beginning
	}

	/**
	 * Reads a specified number of bytes from the stream and returns them in a byte array, starting at a given offset.
	 * @param inputStream The input stream from which to read.
	 * @param offset The number of bytes to skip at the beginning of the input stream.
	 * @param length The number of bytes to read
	 * @return An array of bytes containing the read characters, which may be shorter than the length specified if the number of bytes actually available were
	 *         less than requested.
	 * @throws IllegalArgumentException if the offset or the length is negative.
	 * @throws IOException if there is an error reading from the input stream.
	 */
	public static byte[] readBytes(final InputStream inputStream, final long offset, final int length) throws IOException {
		if(offset < 0) { //if a negative offset is requested
			throw new IllegalArgumentException("Offset cannot be negative.");
		}
		if(length < 0) { //if a negative length is requested
			throw new IllegalArgumentException("Length cannot be negative.");
		}
		final long bytesSkipped = offset > 0 ? inputStream.skip(offset) : 0; //skip the requested amount, if any
		if(bytesSkipped == offset) { //if all the requested bytes were skipped
			final byte[] buffer = new byte[length]; //create a new array of bytes
			final int bytesRead = read(inputStream, buffer); //read bytes into the buffer
			if(bytesRead == length) { //if all the bytes were read
				return buffer; //return the buffer normally
			} else { //if less bytes were read then requested
				final byte[] shortBuffer = new byte[bytesRead]; //create a shorter buffer
				arraycopy(buffer, 0, shortBuffer, 0, bytesRead); //copy the bytes read from the expected buffer to the shorter buffer
				return shortBuffer; //return the shorter buffer of bytes that were actually read
			}
		} else { //if not all the skipped bytes requested were actually skipped
			return Bytes.NO_BYTES; //return an empty byte array
		}
	}

	/**
	 * Checks whether the given input stream is empty (at its end). The input stream must support mark/reset.
	 * @param inputStream The input stream to check.
	 * @return <code>true</code> if reading from the given input stream would return <code>-1</code>.
	 * @throws NullPointerException if the given input stream is <code>null</code>.
	 * @throws IllegalArgumentException if the given input stream does not support mark/reset.
	 * @throws IOException if there was an error accessing the input stream.
	 * @see InputStream#markSupported()
	 */
	public static boolean isEmpty(final InputStream inputStream) throws IOException {
		checkMarkSupported(inputStream);
		inputStream.mark(1); //we want to only check one byte
		final int b = inputStream.read(); //read a single byte
		inputStream.reset(); //reset the stream back to where it was
		return b < 0; //if -1 was returned, the stream is empty
	}

	/**
	 * Checks whether the given input stream supports mark/reset and throws an exception if not.
	 * @param inputStream The input stream to check.
	 * @return The given input stream.
	 * @throws NullPointerException if the given input stream is <code>null</code>.
	 * @throws IllegalArgumentException if the given input stream does not support mark/reset.
	 * @see InputStream#markSupported()
	 */
	public static InputStream checkMarkSupported(final InputStream inputStream) {
		checkArgument(inputStream.markSupported(), "Input stream mark not supported.");
		return inputStream;
	}

	/**
	 * Retrieves an input stream that support mark/reset. If the given input stream supports mark/reset, it is returned; otherwise, a buffered input stream is
	 * constructed around the input stream and returned.
	 * @param inputStream The input stream ultimately supplying the information.
	 * @return An input stream that supports mark/reset, but that will return the same data as the given input stream.
	 * @throws NullPointerException if the given input stream is <code>null</code>.
	 * @see InputStream#markSupported()
	 */
	public static InputStream toMarkSupportedInputStream(final InputStream inputStream) {
		if(inputStream.markSupported()) { //if the input stream already supports mark/reset
			return inputStream; //return what we have already
		}
		return new BufferedInputStream(inputStream); //return buffered access to the input stream, which we know will support mark/reset 
	}

	/**
	 * Fills a buffer with bytes from an input stream, starting at its beginning, blocking until the buffer is full or the end of the stream is reached.
	 * @apiNote This method functions identically to {@link InputStream#read(byte[])} except that this method guarantees that the buffer will <em>always</em> be
	 *          filled unless the end of the stream is reached or an error is encountered.
	 * @implSpec This implementation delegates to {@link #read(InputStream, byte[], int)}.
	 * @param inputStream The input stream from which to read.
	 * @param buffer The buffer to fill.
	 * @return The number of bytes actually read; if less than the size of the buffer, the end of the stream has been reached.
	 * @throws IndexOutOfBoundsException If the length is negative or the length is greater than the size of the buffer.
	 * @throws IOException if there is an error reading from the input stream.
	 * @see InputStream#read(byte[])
	 */
	public static int read(final InputStream inputStream, final byte[] buffer) throws IOException {
		return read(inputStream, buffer, buffer.length);
	}

	/**
	 * Fills a section of a buffer with bytes from an input stream, starting at its beginning, blocking until the buffer is full or the end of the stream is
	 * reached.
	 * @apiNote This method functions identically to {@link InputStream#read(byte[], int, int)} with an offset of <code>0</code> except that this method
	 *          guarantees that the requested number of bytes will <em>always</em> be read unless the end of the stream is reached or an error is encountered.
	 * @implSpec This implementation delegates to {@link #read(InputStream, byte[], int, int)}.
	 * @param inputStream The input stream from which to read.
	 * @param buffer The buffer to fill.
	 * @param length The maximum number of bytes to read.
	 * @return The number of bytes actually read; if less than the requested length, the end of the stream has been reached.
	 * @throws IndexOutOfBoundsException If the length is negative or the length is greater than the size of the buffer.
	 * @throws IOException if there is an error reading from the input stream.
	 * @see InputStream#read(byte[], int, int)
	 */
	public static int read(final InputStream inputStream, final byte[] buffer, final int length) throws IOException {
		return read(inputStream, buffer, 0, length);
	}

	/**
	 * Fills a section of a buffer with bytes from an input stream, blocking until the buffer is full or the end of the stream is reached.
	 * @apiNote This method functions identically to {@link InputStream#read(byte[], int, int)} except that this method guarantees that the requested number of
	 *          bytes will <em>always</em> be read unless the end of the stream is reached or an error is encountered.
	 * @param inputStream The input stream from which to read.
	 * @param buffer The buffer to fill.
	 * @param offset The start offset in the buffer at which the data is written.
	 * @param length The maximum number of bytes to read.
	 * @return The number of bytes actually read; if less than the requested length, the end of the stream has been reached.
	 * @throws IndexOutOfBoundsException If the offset is negative, the length is negative, or the length is greater than the remaining bytes in the buffer
	 *           starting at the given offset.
	 * @throws IOException if there is an error reading from the input stream.
	 * @see InputStream#read(byte[], int, int)
	 */
	public static int read(final InputStream inputStream, final byte[] buffer, int offset, int length) throws IOException {
		int totalReadCount = 0;
		int eachReadCount;
		//use `length!=0` instead of `length>0` so that InputStream.read() can check for us to make sure it is not negative
		while(length != 0 && (eachReadCount = inputStream.read(buffer, offset, length)) != -1) {
			totalReadCount += eachReadCount;
			offset += eachReadCount; //the offset goes forward
			length -= eachReadCount; //the remaining length diminishes
		}
		return totalReadCount;
	}

	/**
	 * Attempts to automatically detect the charset of a particular input stream based upon its byte order marker (BOM).
	 * <p>
	 * The input stream must be at its beginning and must support marking and resetting.
	 * </p>
	 * @param inputStream The stream the charset of which will be detected.
	 * @return The charset detected, or <code>null</code> if no byte order mark could be detected.
	 * @throws IOException if a mixed byte order mark is encountered.
	 * @throws IOException if an I/O error occurred.
	 * @see ByteOrderMark
	 * @see ByteOrderMark#isMixed()
	 */
	public static Charset detectCharset(final InputStream inputStream) throws IOException {
		return detectCharset(inputStream, null); //detect the charset, returning null if the BOM can't be determined
	}

	/**
	 * Attempts to automatically detect the charset from the byte order mark (BOM) of a particular input stream.
	 * <p>
	 * The input stream must be at its beginning and must support marking and resetting.
	 * </p>
	 * @param inputStream The stream the charset of which will be detected.
	 * @param defaultCharset The charset to return if the encoding can't be determined by the byte order mark.
	 * @return The charset detected, or the given default charset if no byte order mark could be detected.
	 * @throws IOException if a mixed byte order mark is encountered.
	 * @throws IOException if an I/O error occurred.
	 * @see ByteOrderMark
	 * @see ByteOrderMark#isMixed()
	 */
	public static Charset detectCharset(final InputStream inputStream, final Charset defaultCharset) throws IOException {
		checkArgument(inputStream.markSupported(), "Inputstream must support mark/reset.");
		final int BYTE_ORDER_MARK_LENGTH = 4; //the number of bytes in the largest byte order mark
		inputStream.mark(BYTE_ORDER_MARK_LENGTH); //we won't read more than the byte order mark
		final byte[] bytes = new byte[BYTE_ORDER_MARK_LENGTH]; //create an array to hold the byte order mark; Java initializes the array with zeros
		final int byteOrderMarkCount = read(inputStream, bytes); //read as many characters of the byte order mark as we can
		if(byteOrderMarkCount > 0) { //if we read any characters as all
			final Optional<ByteOrderMark> optionalBOM = ByteOrderMark.detect(bytes); //try to detect the byte order mark
			if(optionalBOM.isPresent()) { //if we discovered a BOM
				final ByteOrderMark bom = optionalBOM.get();
				bom.checkUsualIO(); //make sure this is a usual 
				//throw away the BOM
				inputStream.reset(); //reset the stream back to where we found it
				for(int i = bom.getLength() - 1; i >= 0; --i) { //throw away the correct number of bytes
					inputStream.read(); //throw away a byte
				}
				return bom.toCharset(); //return the charset for this BOM
			}
		}
		inputStream.reset(); //reset the stream back to where we found it
		return defaultCharset; //we couldn't find a BOM; return the default charset
	}

}
