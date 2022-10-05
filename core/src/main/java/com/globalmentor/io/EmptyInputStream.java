/*
 * Copyright © 2012 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import java.io.IOException;
import java.io.InputStream;

/**
 * An input stream that has no content.
 * 
 * @author Garret Wilson
 */
public class EmptyInputStream extends InputStream {

	/** {@inheritDoc} This version always returns -1. */
	@Override
	public int read() throws IOException {
		return -1;
	}

	/** {@inheritDoc} This version always returns -1. */
	@Override
	public int read(byte[] b) throws IOException {
		return -1;
	}

	/** {@inheritDoc} This version always returns -1. */
	@Override
	public int read(byte[] b, int off, int len) throws IOException {
		return -1;
	}

	/** {@inheritDoc} This version always returns 0. */
	@Override
	public long skip(long n) throws IOException {
		return 0;
	}

}
