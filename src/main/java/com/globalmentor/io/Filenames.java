/*
 * Copyright © 1996-2018 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.util.stream.*;

/**
 * Utilities for working with filenames, whether from files, paths, or URIs for example.
 * @author Garret Wilson
 */
public class Filenames { //TODO move relevant constants and utilities from Files, URIs, and potentially other classes

	/**
	 * Returns all the possible extensions of a filename, from the most specific to the most general.
	 * <p>
	 * For example for the filename <code>example.foo.bar</code> the following would be returned in order:
	 * </p>
	 * <ol>
	 * <li><code>foo.bar</code></li>
	 * <li><code>bar</code></li>
	 * </ol>
	 */
	public static Stream<String> extensions(final CharSequence filename) {
		return StreamSupport.stream(getExtensions(filename).spliterator(), false); //TODO create direct filename extension spliterator
	}

	/**
	 * Returns all the possible extensions of a filename, from the most specific to the most general.
	 * <p>
	 * For example for the filename <code>example.foo.bar</code> the following would be returned in order:
	 * </p>
	 * <ol>
	 * <li><code>foo.bar</code></li>
	 * <li><code>bar</code></li>
	 * </ol>
	 */
	public static Iterable<String> getExtensions(final CharSequence filename) {
		return () -> new FilenameExtensionIterator(filename);
	}

}
