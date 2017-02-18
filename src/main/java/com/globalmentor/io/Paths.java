/*
 * Copyright © 2017 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.nio.file.Path;

/**
 * Utility methods to manipulate Paths.
 * 
 * @author Magno Nascimento
 */
public class Paths {

	/** This class cannot be publicly instantiated. */
	private Paths() {
	}

	/**
	 * Adds the given extension to a path and returns the new path with the new extension. The filename is not checked to see if it currently has an extension.
	 * This method delegates to {@link Files#addExtension(String, String)}.
	 * @param path The path to which to add an extension.
	 * @param extension The extension to add.
	 * @return The path with the new extension.
	 */
	public static Path addExtension(final Path path, final String extension) {
		return java.nio.file.Paths.get(Files.addExtension(path.toString(), extension)); //add an extension to the path and create and return a new file with that 
	}

}
