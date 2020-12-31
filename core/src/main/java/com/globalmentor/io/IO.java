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

import static java.util.Objects.*;

import java.io.*;
import java.net.URI;

import javax.annotation.*;

/**
 * Support for reading or writing a particular type.
 * @param <T> The type to read and write.
 * @author Garret Wilson
 */
public interface IO<T> {

	/**
	 * Converts an object to a {@link Closeable} instance so that it can be used with try-with-resources.
	 * @apiNote A more general auto-closeable interface would be {@link AutoCloseable}, although that interface throws more generalized exceptions; a similar
	 *          method for {@link AutoCloseable} is available at {@link com.globalmentor.java.Objects#toAutoCloseable(Object)}.
	 * @implSpec If the given object is an instance of {@link Closeable}, the object itself is returned; otherwise, a no-operation {@link Closeable} instance is
	 *           returned.
	 * @param object The object to convert to a {@link Closeable}.
	 * @return A {@link Closeable} instance that will ensure the object is closed if it implements {@link Closeable}.
	 * @see com.globalmentor.java.Objects#toAutoCloseable(Object)
	 */
	public static Closeable toCloseable(@Nonnull final Object object) {
		if(object instanceof Closeable) {
			return (Closeable)object;
		}
		requireNonNull(object); //if the object was auto-closeable above, we didn't need the null check
		return () -> { //no-op
		};
	}

	/**
	 * Reads an object from an input stream.
	 * @param inputStream The input stream from which to read the data.
	 * @param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	 * @return The object read from the input stream.
	 * @throws NullPointerException if the given input stream is <code>null</code>.
	 * @throws IOException Thrown if there is an error reading the data.
	 * @deprecated to be moved to an I/O strategy class, or removed altogether.
	 */
	@Deprecated
	public T read(final InputStream inputStream, final URI baseURI) throws IOException;

	/**
	 * Writes an object to an output stream.
	 * @param outputStream The output stream to which to write the data.
	 * @param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	 * @param object The object to write to the given output stream.
	 * @throws NullPointerException if the given output stream and/or object is <code>null</code>.
	 * @throws IOException Thrown if there is an error writing the data.
	 * @deprecated to be moved to an I/O strategy class, or removed altogether.
	 */
	@Deprecated
	public void write(final OutputStream outputStream, final URI baseURI, final T object) throws IOException;

}
