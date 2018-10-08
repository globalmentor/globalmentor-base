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

	private int nextDelimiterIndex;

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
		nextDelimiterIndex = -1; //prepare the next delimiter index to be primed
		nextDelimiterIndex = findNextDelimiterIndex(); //prime the next delimiter index
	}

	@Override
	public boolean hasNext() {
		return nextDelimiterIndex >= 0;
	}

	@Override
	public String next() {
		if(nextDelimiterIndex < 0) {
			throw new NoSuchElementException("No more filename extensions found.");
		}
		final String next = filename.subSequence(nextDelimiterIndex + 1, filename.length()).toString();
		nextDelimiterIndex = findNextDelimiterIndex(); //advance to the next delimiter
		return next;
	}

	/**
	 * Determines the next delimiter index based on the current "next" delimiter index.
	 * @return The index of the new "next" delimiter index, based on the current "next" delimiter index.
	 */
	protected int findNextDelimiterIndex() {
		return indexOf(filename, extensionDelimiter, nextDelimiterIndex + 1);
	}

}
