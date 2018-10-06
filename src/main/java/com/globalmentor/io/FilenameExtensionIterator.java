/*
 * Copyright © 2018 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.io.Files.FILENAME_EXTENSION_SEPARATOR;
import static com.globalmentor.java.CharSequences.*;
import static java.util.Objects.*;

import java.util.*;

import javax.annotation.*;

/**
 * Iterates through the possible extensions of a filename, from the most specific to the most general.
 * <p>
 * For example for the filename <code>example.foo.bar</code>, this iterator would return the following in order:
 * </p>
 * <ol>
 * <li><code>foo.bar</code></li>
 * <li><code>bar</code></li>
 * </ol>
 * @author Garret Wilson
 * @see Files#FILENAME_EXTENSION_SEPARATOR
 */
public class FilenameExtensionIterator implements Iterator<String> {

	private final CharSequence filename;

	private final char extensionDelimiter;

	private int delimiterIndex;

	/**
	 * Filename constructor using the normal filename extension delimiter.
	 * @param filename The filename over which extensions should be iterated.
	 * @see Files#FILENAME_EXTENSION_SEPARATOR
	 */
	public FilenameExtensionIterator(@Nonnull final CharSequence filename) {
		this(filename, FILENAME_EXTENSION_SEPARATOR);
	}

	/**
	 * Filename and delimiter constructor.
	 * @param filename The filename over which extensions should be iterated.
	 * @param delimiter The filename extension delimiter.
	 */
	public FilenameExtensionIterator(@Nonnull final CharSequence filename, final char extensionDelimiter) {
		this.filename = requireNonNull(filename);
		this.extensionDelimiter = extensionDelimiter;
		delimiterIndex = -1; //prime the current delimiters index
		delimiterIndex = findNextDelimiterIndex();
	}

	@Override
	public boolean hasNext() {
		return delimiterIndex >= 0;
	}

	@Override
	public String next() {
		if(delimiterIndex < 0) {
			throw new NoSuchElementException("No more filename extensions found.");
		}
		final String next = filename.subSequence(delimiterIndex + 1, filename.length()).toString();
		delimiterIndex = findNextDelimiterIndex(); //prime the next delimiter index
		return next;
	}

	protected int findNextDelimiterIndex() {
		return indexOf(filename, extensionDelimiter, delimiterIndex + 1);
	}

}
