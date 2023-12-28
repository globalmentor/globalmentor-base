/*
 * Copyright © 2017 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import javax.annotation.*;

/**
 * Utility factory class for creating {@link Closeable} instances.
 * 
 * @author Garret Wilson
 */
public final class Close {

	private Close() {
	}

	/**
	 * Produces a {@link Closeable} object that will close by invoking the given consumer.
	 * @param closeStragy The strategy for closing the returned {@link Closeable}.
	 * @return A {@link Closeable} instance that will invoke the given close strategy when closed.
	 * @throws NullPointerException if the optional is <code>null</code>; or if the value is present but the consumer is <code>null</code>.
	 */
	public static Closeable by(@Nonnull final Strategy closeStragy) {
		requireNonNull(closeStragy);
		return new AbstractCloseable() {
			@Override
			protected void closeImpl() throws IOException {
				closeStragy.close();
			}
		};
	}

	/**
	 * A strategy for closing.
	 * <p>
	 * This interface differs from {@link Closeable} in that there is no requirement that the {@link #close()} method be idempotent. It is not marked as
	 * {@link AutoCloseable} because it is not meant to be used in auto-closing situations.
	 * </p>
	 * @author Garret Wilson
	 */
	@FunctionalInterface
	public interface Strategy {
		/**
		 * Performs some closing operation.
		 * <p>
		 * The implementation of this method may not be idempotent.
		 * </p>
		 * @throws IOException if an I/O error occurs.
		 */
		public void close() throws IOException;

	}
}
