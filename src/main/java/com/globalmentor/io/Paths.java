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

import static com.globalmentor.io.Filenames.*;
import static java.util.Objects.*;

import java.nio.file.Path;

import javax.annotation.*;

/**
 * Utility methods to manipulate Paths.
 * 
 * @author Magno Nascimento
 */
public class Paths {

	/** This class cannot be publicly instantiated. */
	private Paths() {
	}

	//#filenames

	//##dotfiles

	/**
	 * Determines whether the path is for a so-called "dotfile", the filename of which begins with a dot but is neither <code>"."</code> or <code>".."</code>.
	 * This method does not make a distinction between files and directories.
	 * @param path The path to check.
	 * @return <code>true</code> if the path contains a filename that is considered a dotfile.
	 * @see <a href="https://superuser.com/q/757635/954883">Why do some file/folder names on Windows have a dot in front of them?</a>
	 * @see <a href="https://wiki.archlinux.org/index.php/Dotfiles">Dotfiles</a>
	 * @see Path#getFileName()
	 * @see Filenames#isDotfileFilename(CharSequence)
	 */
	public static boolean isDotfile(@Nonnull final Path path) {
		final Path filename = path.getFileName();
		return filename != null && isDotfileFilename(filename.toString());
	}

	//#extensions

	/**
	 * Adds the given extension to a path and returns the new path with the new extension. The filename is not checked to see if it currently has an extension.
	 * @implSpec This method delegates to {@link Filenames#addExtension(String, String)}.
	 * 
	 * @param path The path to which to add an extension.
	 * @param extension The extension to add.
	 * 
	 * @return The path with the new extension.
	 */
	public static Path addExtension(final Path path, final String extension) {
		requireNonNull(path, "the <path> cannot be null.");
		requireNonNull(extension, "the <extension> to be added cannot be null.");

		final String fileName = path.getFileName().toString();

		return path.resolveSibling(Filenames.addExtension(fileName, extension));
	}

}
