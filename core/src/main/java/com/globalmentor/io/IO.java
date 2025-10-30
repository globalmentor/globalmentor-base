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

import static java.util.Objects.*;

import java.io.*;

import org.jspecify.annotations.*;

/**
 * I/O utilities.
 * @author Garret Wilson
 */
public final class IO {

	private IO() {
	}

	/**
	 * Checks the result of an expression and throws an {@link IOException} if the value is <code>false</code>.
	 * @param test The result of the test.
	 * @throws IOException if the given value is <code>false</code>.
	 */
	public static void check(final boolean test) throws IOException {
		check(test, null); //check the test with no description
	}

	/**
	 * Checks the result of an expression and throws an {@link IOException} if the value is <code>false</code>.
	 * @param test The result of the test.
	 * @param description A description of the test to be used when generating an exception, optionally formatted with arguments, or <code>null</code> for no
	 *          description.
	 * @param arguments The arguments to be applied when formatting, or an empty array if the message should not be formatted.
	 * @throws NullPointerException if the given arguments is <code>null</code>.
	 * @throws IOException if the given value is <code>false</code>.
	 * @throws IllegalArgumentException if the description is an invalid pattern, or if an argument in the arguments array is not of the type expected by the
	 *           format element(s) that use it.
	 * @see String#format(String, Object...)
	 */
	public static void check(final boolean test, String description, final Object... arguments) throws IOException {
		if(!test) { //format the message if appropriate
			if(description != null && arguments.length > 0) {
				description = String.format(description, arguments);
			}
			throw new IOException(description);
		}
	}

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
	public static Closeable toCloseable(@NonNull final Object object) {
		if(object instanceof Closeable) {
			return (Closeable)object;
		}
		requireNonNull(object); //if the object was auto-closeable above, we didn't need the null check
		return () -> { //no-op
		};
	}

}
