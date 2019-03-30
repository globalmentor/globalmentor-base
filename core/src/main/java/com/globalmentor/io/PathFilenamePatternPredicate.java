/*
 * Copyright © 2019 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.nio.file.Path;
import java.util.function.Predicate;
import java.util.regex.Pattern;

import javax.annotation.*;

/**
 * A predicate that matches paths based on whether the path filename matches a given pattern.
 * @implSpec This predicate may be used by multiple threads.
 * @author Garret Wilson
 * @see Pattern
 */
public class PathFilenamePatternPredicate implements Predicate<Path> {

	private final Pattern filenamePattern;

	/**
	 * Creates a predicate for matching a given filename pattern.
	 * @param filenamePattern The pattern for matching a filename.
	 */
	private PathFilenamePatternPredicate(@Nonnull final Pattern filenamePattern) {
		this.filenamePattern = requireNonNull(filenamePattern);
	}

	/**
	 * Returns a predicate for matching a given filename pattern. Only paths that have a filename and which filename matches the given filename pattern will pass
	 * the predicate.
	 * @param filenamePattern The pattern for matching a filename.
	 * @return A predicate for matching path filenames against the given pattern.
	 */
	public static PathFilenamePatternPredicate forPattern(@Nonnull final Pattern filenamePattern) {
		return new PathFilenamePatternPredicate(filenamePattern);
	}

	@Override
	public boolean test(final Path path) {
		final Path filename = path.getFileName();
		//we can't reset and reuse the matcher because this predicate may be used by multiple threads
		return filename != null && filenamePattern.matcher(filename.toString()).matches();
	}

}
