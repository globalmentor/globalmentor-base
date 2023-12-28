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

import java.io.*;

/**
 * Abstract base implementation to facilitate implementation of {@link Closeable}.
 * <p>
 * This implementation ensures that {@link Closeable#close()} is idempotent by preventing the underlying close implementation from being called more than once.
 * </p>
 * <p>
 * Implementations must override {@link #closeImpl()}.
 * </p>
 * 
 * @author Garret Wilson
 */
public abstract class AbstractCloseable implements Closeable {

	private boolean closed = false;

	/** Constructor. */
	protected AbstractCloseable() {
	}

	/**
	 * {@inheritDoc} Implementations must override {@link #closeImpl()}.
	 * @see #closeImpl()
	 */
	@Override
	public void close() throws IOException {
		if(!closed) {
			closed = true;
			closeImpl();
		}
	}

	/**
	 * Implements the closing functionality.
	 *
	 * @throws IOException if an I/O error occurs.
	 * @see #close()
	 */
	protected abstract void closeImpl() throws IOException;

}
