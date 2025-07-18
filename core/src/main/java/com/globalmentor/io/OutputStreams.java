/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import com.globalmentor.io.function.IOConsumer;

/**
 * Class to manipulate output streams.
 * @author Garret Wilson
 */
public final class OutputStreams {

	private OutputStreams() {
	}

	/**
	 * Writes the given byte to the output stream the specified number of times.
	 * @param outputStream The stream the bytes should be written to.
	 * @param b The byte to write; the 24 high-order bits are ignored.
	 * @param count The number of bytes to write.
	 * @throws IOException Thrown if there is an error writing to the output stream.
	 */
	public static void write(final OutputStream outputStream, final int b, int count) throws IOException {
		for(; count > 0; --count)
			//decrement the count each time we write a byte
			outputStream.write(b); //write the byte
	}

	/**
	 * Writes the given number of bytes of the given value, low-ordered bytes first.
	 * @param outputStream The stream the bytes should be written to.
	 * @param value The value to write.
	 * @param byteCount The number of bytes to write (&lt;=4).
	 * @throws IllegalArgumentException Thrown if the byte count is over four.
	 * @throws IOException Thrown if there is an error writing to the output stream.
	 */
	public static void writeLowOrderFirst(final OutputStream outputStream, long value, int byteCount) throws IllegalArgumentException, IOException {
		if(byteCount > 4) //if an invalid byte count was given
			throw new IllegalArgumentException("Invalid byte count: " + byteCount);
		for(; byteCount > 0; --byteCount) { //write each byte
			outputStream.write((int)value); //write the LSB of the value
			value = value >>> 8; //shift the value down a byte
		}
	}

	/**
	 * Collects bytes written to an output stream provided to a byte producer.
	 * @apiNote Regarding terminology, the byte <em>producer</em> is technically also a <em>consumer</em> because it consumes the {@link OutputStream} to which it
	 *          will produce bytes.
	 * @implSpec This implementation internally uses a {@link ByteArrayOutputStream}.
	 * @implNote An initial internal buffer size of <code>32</code> is used, which is the same as that {@link ByteArrayOutputStream#ByteArrayOutputStream()} uses.
	 * @implNote This API was designed so that in normal circumstances the caller can collect bytes without worrying about exception handling. The
	 *           {@link OutputStream} used by the implementation cannot throw an {@link IOException}. Nevertheless, a consumer that allows for throwing an
	 *           {@link IOException} was used because many operations that work with {@link OutputStream} report throwing an {@link IOException}, which would have
	 *           required tedious wrapping of the logic. (Calling {@link Writer#write(String)} on a wrapped {@link Writer} is one example.) The byte producer must
	 *           not actually throw an {@link IOException} in its processing of data unless the caller is prepared to handle an {@link UncheckedIOException}.
	 * @param byteProducer The logic to produce bytes by writing to a provided output stream. The producer is expected not to throw an {@link IOException}.
	 * @return The bytes collected after the byte producer is finished writing to the provided output stream.
	 * @throws UncheckedIOException if the byte producer throws an {@link IOException} during its processing of data (not in its writing to the
	 *           {@link OutputStream}, which cannot throw an {@link IOException}). If the byte producer does not throw an {@link IOException} in its processing of
	 *           data, no {@link UncheckedIOException} will be thrown.
	 */
	public static byte[] collectBytes(final IOConsumer<OutputStream> byteProducer) {
		return collectBytes(byteProducer, 32); //use the same initial buffer size that an `OutputStream` would use 
	}

	/**
	 * Collects bytes written to an output stream provided to a byte producer, using a buffer with the specified initial size.
	 * @apiNote Regarding terminology, the byte <em>producer</em> is technically also a <em>consumer</em> because it consumes the {@link OutputStream} to which it
	 *          will produce bytes.
	 * @implSpec This implementation internally uses a {@link ByteArrayOutputStream} using an initial buffer of the requested size.
	 * @implNote This API was designed so that in normal circumstances the caller can collect bytes without worrying about exception handling. The
	 *           {@link OutputStream} used by the implementation cannot throw an {@link IOException}. Nevertheless, a consumer that allows for throwing an
	 *           {@link IOException} was used because many operations that work with {@link OutputStream} report throwing an {@link IOException}, which would have
	 *           required tedious wrapping of the logic. (Calling {@link Writer#write(String)} on a wrapped {@link Writer} is one example.) The byte producer must
	 *           not actually throw an {@link IOException} in its processing of data unless the caller is prepared to handle an {@link UncheckedIOException}.
	 * @param byteProducer The logic to produce bytes by writing to a provided output stream. The producer is expected not to throw an {@link IOException}.
	 * @param initialBufferSize The initial size to use for the buffer.
	 * @return The bytes collected after the byte producer is finished writing to the provided output stream.
	 * @throws UncheckedIOException if the byte producer throws an {@link IOException} during its processing of data (not in its writing to the
	 *           {@link OutputStream}, which cannot throw an {@link IOException}). If the byte producer does not throw an {@link IOException} in its processing of
	 *           data, no {@link UncheckedIOException} will be thrown.
	 */
	public static byte[] collectBytes(final IOConsumer<OutputStream> byteProducer, final int initialBufferSize) {
		try (final ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream(initialBufferSize)) {
			byteProducer.accept(byteArrayOutputStream);
			return byteArrayOutputStream.toByteArray(); //TODO make improvement, with extensive tests, exposing the underlying buffer and returning it without copying if the count is the same size as the buffer  
		} catch(final IOException ioException) {
			throw new UncheckedIOException(ioException);
		}
	}

}
