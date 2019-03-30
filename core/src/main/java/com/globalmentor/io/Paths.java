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
import static com.globalmentor.java.Conditions.*;
import static java.util.Objects.*;

import java.nio.file.Path;
import java.util.function.Predicate;
import java.util.regex.Pattern;

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

	/**
	 * Changes a path from one base to another. For example, <code>/example/base1/test.txt</code> changed from base <code>/example/base1/</code> to base
	 * <code>/example/base2/level2/</code> yields <code>/example/base2/level2/test.txt</code>. If the old and new base paths are the same, a path equal to the
	 * given path is returned.
	 * <p>
	 * The paths are normalized before the base is changed.
	 * </p>
	 * @param path The path the base of which to change.
	 * @param oldBasePath The current base path.
	 * @param newBasePath The base path of the new path to return .
	 * @return A new path constructed by relativizing the path to the old base path and resolving the resulting path against the new base path.
	 * @see Path#relativize(Path)
	 * @see Path#resolveSibling(Path)
	 * @throws IllegalArgumentException if the old base path is not a base path (or the same path) of the given path.
	 */
	public static Path changeBase(@Nonnull final Path path, Path oldBasePath, Path newBasePath) {
		oldBasePath = oldBasePath.normalize();
		newBasePath = newBasePath.normalize();
		if(oldBasePath.equals(newBasePath)) {
			return path; //the URI will not change
		}
		checkArgumentSubPath(oldBasePath, path);
		final Path relativePath = oldBasePath.relativize(path);
		assert !relativePath.isAbsolute() : "A path relativized against a base path is not expected to be absolute.";
		return newBasePath.resolve(relativePath);
	}

	/**
	 * Ensures that a path is absolute.
	 * @param path The path to check to be absolute.
	 * @return The given path.
	 * @throws IllegalArgumentException if the given path is not absolute.
	 * @see Path#isAbsolute()
	 */
	public static Path checkArgumentAbsolute(@Nonnull final Path path) {
		checkArgument(path.isAbsolute(), "The path %s is not absolute.", path);
		return path;
	}

	/**
	 * Ensures one path is a subpath of another; that is, they both share a base path with no backtracking. This method allows the paths to be identical.
	 * @param basePath The base path against which the other path will be compared.
	 * @param subPath The potential subpath.
	 * @return The given subpath.
	 * @throws IllegalArgumentException if the given subpath is not a subpath of (or the same path as) the base path.
	 * @see #isSubPath(Path, Path)
	 */
	public static Path checkArgumentSubPath(@Nonnull final Path basePath, @Nonnull final Path subPath) {
		checkArgument(isSubPath(basePath, subPath), "The path %s is not a subpath of the path %s.", subPath, basePath);
		return subPath;
	}

	/**
	 * Ensures two paths are <dfn>disjoint</dfn>, that is, neither one is a subpath of (or equal to) the other.
	 * @param path1 The first path to compare.
	 * @param path2 The second path.
	 * @throws IllegalArgumentException if the two path trees overlap.
	 * @see #isDisjoint(Path, Path)
	 */
	public static void checkArgumentDisjoint(@Nonnull final Path path1, @Nonnull final Path path2) {
		checkArgument(isDisjoint(path1, path2), "The paths %s and %s are not allowed to overlap.", path1, path2);
	}

	/**
	 * Determines whether two paths are <dfn>disjoint</dfn>, that is, neither one is a subpath of (or equal to) the other.
	 * @param path1 The first path to compare.
	 * @param path2 The second path.
	 * @return <code>true</code> if the two path trees do not overlap.
	 * @see #isSubPath(Path, Path)
	 */
	public static boolean isDisjoint(@Nonnull final Path path1, @Nonnull final Path path2) {
		return !isSubPath(path1, path2) && !isSubPath(path2, path1);
	}

	/**
	 * Determines whether one path is a subpath of another; that is, they both share a base path with no backtracking. This method allows the paths to be
	 * identical.
	 * @param basePath The base path against which the other path will be compared.
	 * @param subPath The potential subpath.
	 * @return <code>true</code> if the given subpath is truly a subpath of (or the same path as) the base path.
	 */
	public static boolean isSubPath(@Nonnull final Path basePath, @Nonnull final Path subPath) {
		return subPath.normalize().startsWith(basePath.normalize()); //normalize files to compare apples to apples
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
	 * @param path The path to which to add an extension.
	 * @param extension The extension to add.
	 * @return The path with the new extension.
	 * @throws IllegalArgumentException If a filename is not present.
	 * @see Filenames#addExtension(String, String)
	 */
	public static Path addExtension(@Nonnull final Path path, @Nonnull final String extension) {
		requireNonNull(path, "the <path> cannot be null.");
		requireNonNull(extension, "the <extension> to be added cannot be null.");
		final Path filename = path.getFileName();
		checkArgument(filename != null, "Path %s has no filename to which an extension can be added.", path);
		return path.resolveSibling(Filenames.addExtension(filename.toString(), extension));
	}

	/**
	 * Changes the last extension of a path's filename and returns a new path with the filename with the new extension. If the filename does not currently have an
	 * extension, one will be added.
	 * @implSpec This method delegates to {@link Filenames#changeExtension(String, String)}.
	 * @param path The path for which an extension will be changed.
	 * @param extension The extension to set, or <code>null</code> if the extension should be removed.
	 * @return The path with the filename with the new extension.
	 * @throws IllegalArgumentException If a filename is not present, or if the name is just a "/".
	 * @see Filenames#changeExtension(String, String)
	 */
	public static Path changeExtension(@Nonnull final Path path, @Nullable final String extension) {
		final Path filename = path.getFileName();
		checkArgument(filename != null, "Path %s has no filename for changing its extension.");
		return path.resolveSibling(Filenames.changeExtension(filename.toString(), extension));
	}

	/**
	 * Removes the last extension, if any, of a path's filename and returns a new filename with no extension.
	 * @implSpec This method delegates to {@link Filenames#removeExtension(String)}.
	 * @param path The path for which an extension will be removed.
	 * @return The path with the filename with no extension.
	 * @see Filenames#removeExtension(String)
	 */
	public static Path removeExtension(@Nonnull final Path path) {
		final Path filename = path.getFileName();
		checkArgument(filename != null, "Path %s has no filename for changing its extension.");
		return path.resolveSibling(Filenames.removeExtension(filename.toString()));
	}

	//#predicates

	/**
	 * Returns a predicate for matching paths by a base filename. Only paths that have a filename and which filename has the given base name (the given base name
	 * followed by one or more filename extensions) will pass the predicate.
	 * @param baseFilename The filename base name to match.
	 * @return A predicate for matching path filenames against the given base name.
	 * @see Filenames#getBaseFilenamePattern(String)
	 */
	public static Predicate<Path> byBaseFilename(@Nonnull final String baseFilename) {
		return PathFilenamePatternPredicate.forPattern(getBaseFilenamePattern(baseFilename)); //TODO test
	}

	/**
	 * Returns a predicate for a given filename pattern. Only paths that have a filename and which filename matches the given filename pattern will pass the
	 * predicate.
	 * @param filenamePattern The pattern for matching a filename.
	 * @return A predicate for matching path filenames against the given pattern.
	 */
	public static Predicate<Path> byFilenamePattern(@Nonnull final Pattern filenamePattern) {
		return PathFilenamePatternPredicate.forPattern(filenamePattern);
	}

}
