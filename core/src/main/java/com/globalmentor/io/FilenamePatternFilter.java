/*
 * Copyright © 2011 GlobalMentor, Inc. <https://www.globalmentor.com/>
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
import java.util.regex.*;

import static java.util.Objects.*;

/**
 * A class that can filter files based upon a given filename pattern.
 * @author Garret Wilson
 */
public class FilenamePatternFilter extends AbstractFileFilter {

	/** The pattern for checking the filename. */
	private final Pattern filenamePattern;

	/** @return The pattern for checking the filename. */
	public Pattern getFilenamePattern() {
		return filenamePattern;
	}

	/**
	 * Constructor specifying a filename matching pattern string.
	 * @param filenamePatternString The string for creating a pattern for checking the filename.
	 * @throws PatternSyntaxException If the expression's syntax is invalid
	 */
	public FilenamePatternFilter(final String filenamePatternString) {
		this(Pattern.compile(filenamePatternString));
	}

	/**
	 * Constructor specifying a filename matching pattern.
	 * @param filenamePattern The pattern for checking the filename.
	 */
	public FilenamePatternFilter(final Pattern filenamePattern) {
		this.filenamePattern = requireNonNull(filenamePattern);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This version matches the filename against {@link #getFilenamePattern()}.
	 * </p>
	 * @see Matcher#matches()
	 */
	public boolean accept(final File file) {
		return getFilenamePattern().matcher(file.getName()).matches();
	}
}
