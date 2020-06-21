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

import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.java.OperatingSystem.*;
import static java.util.Objects.*;

import java.nio.file.*;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.*;

import javax.annotation.*;

import com.globalmentor.java.*;
import com.globalmentor.text.Case;

/**
 * Utilities for working with filenames, whether from files, paths, or URIs for example.
 * @author Garret Wilson
 */
public class Filenames {

	/**
	 * The prefix used by Unix to designate a <dfn>dotfile</dfn>, which is usually hidden.
	 * @see <a href="https://superuser.com/q/757635/954883">Why do some file/folder names on Windows have a dot in front of them?</a>
	 * @see <a href="https://wiki.archlinux.org/index.php/Dotfiles">Dotfiles</a>
	 */
	public static final char DOTFILE_PREFIX = '.';

	/** The character used to separate an extension from the rest of a filename. */
	public static final char EXTENSION_SEPARATOR = '.';

	/**
	 * The special name {@value #CURRENT_DIRECTORY_NAME} indicating the current directory.
	 * @apiNote This is not necessarily a special name on <em>every</em> implementation of a {@link FileSystem}. See further
	 *          <a href="https://stackoverflow.com/q/60834114/421049">discussion on Stack Overflow</a>.
	 * @see Path#normalize()
	 */
	public static final String CURRENT_DIRECTORY_NAME = ".";

	/**
	 * The special name {@value #PARENT_DIRECTORY_NAME} indicating the parent directory.
	 * @apiNote This is not necessarily a special name on <em>every</em> implementation of a {@link FileSystem}. See further
	 *          <a href="https://stackoverflow.com/q/60834114/421049">discussion on Stack Overflow</a>.
	 * @see Path#normalize()
	 */
	public static final String PARENT_DIRECTORY_NAME = "..";

	/**
	 * Determines if a filename is considered "special" (such as a parent directory designation) on file systems in general and cannot therefore be used as a
	 * normal filename.
	 * @apiNote If this method returns <code>true</code> for a name, it does not necessarily mean that the name is considered special on <em>every</em>
	 *          implementation of a {@link FileSystem}. See further <a href="https://stackoverflow.com/q/60834114/421049">discussion on Stack Overflow</a>.
	 * @param name The filename to test.
	 * @return <code>true</code> if the given name is special and should not be used as a filename.
	 * @see #CURRENT_DIRECTORY_NAME
	 * @see #PARENT_DIRECTORY_NAME
	 */
	public static boolean isSpecialName(@Nonnull final String name) {
		return name.equals(CURRENT_DIRECTORY_NAME) || name.equals(PARENT_DIRECTORY_NAME);
	}

	/**
	 * The character to use for escaping reserved characters.
	 * <p>
	 * This is a somewhat arbitrary, proprietary escape character. It does not reflect any existing convention.
	 * </p>
	 * <p>
	 * Java automatically converts <code>'%'</code> in URIs and does not correctly access file URIs containing <code>'#'</code>, so neither of these characters
	 * can be used as an escape character.
	 * </p>
	 * <p>
	 * Note that, as <code>'^'</code> is not a valid URI character, it will be escaped again using <code>'%'</code> if such a filename is included in a URI.
	 * </p>
	 */
	public static final char ESCAPE_CHAR = '^';

	/**
	 * The characters that may not be used in POSIX filenames.
	 * @see <a href="http://hypermail.idiosynkrasia.net/linux-kernel/archived/2001/week50/1017.html">Linux Kernal Mailing List 2001:50:1017</a>
	 * @see #encodeFilename(String)
	 */
	public static final Characters POSIX_RESERVED_CHARACTERS = Characters.of('\u0000', '/');
	/**
	 * The characters that may not be used in Windows filenames.
	 * @see <a href="http://msdn.microsoft.com/en-us/library/aa365247.aspx">MSDN: Naming a File or Directory</a>
	 * @see <a href="http://hypermail.idiosynkrasia.net/linux-kernel/archived/2001/week50/1017.html">Linux Kernal Mailing List 2001:50:1017</a>
	 * @see #encodeFilename(String)
	 */
	public static final Characters WINDOWS_RESERVED_CHARACTERS = Characters.of('\u0000', '<', '>', ':', '"', '/', '\\', '|', '?', '*');
	/**
	 * The characters that may not be used as the last character of Windows filenames.
	 * @see <a href="http://msdn.microsoft.com/en-us/library/aa365247.aspx">MSDN: Naming a File or Directory</a>
	 * @see <a href="http://hypermail.idiosynkrasia.net/linux-kernel/archived/2001/week50/1017.html">Linux Kernal Mailing List 2001:50:1017</a>
	 * @see #encodeFilename(String)
	 */
	public static final Characters WINDOWS_RESERVED_FINAL_CHARACTERS = Characters.of('.', ' ');
	/**
	 * The characters that may not be used in various file system filenames.
	 * @see <a href="http://msdn.microsoft.com/en-us/library/aa365247.aspx">MSDN: Naming a File or Directory</a>
	 * @see <a href="http://hypermail.idiosynkrasia.net/linux-kernel/archived/2001/week50/1017.html">Linux Kernal Mailing List 2001:50:1017</a>
	 * @see #encodeFilename(String)
	 */
	public static final Characters CROSS_PLATFORM_RESERVED_CHARACTERS = Characters.of('\u0000', '<', '>', ':', '"', '/', '\\', '|', '?', '*');
	/**
	 * The characters that may not be used as the last character of various file system filenames.
	 * @see <a href="http://msdn.microsoft.com/en-us/library/aa365247.aspx">MSDN: Naming a File or Directory</a>
	 * @see <a href="http://hypermail.idiosynkrasia.net/linux-kernel/archived/2001/week50/1017.html">Linux Kernal Mailing List 2001:50:1017</a>
	 * @see #encodeFilename(String)
	 */
	public static final Characters CROSS_PLATFORM_RESERVED_FINAL_CHARACTERS = Characters.of('.', ' ');

	/** This class cannot be publicly instantiated. */
	private Filenames() {
	}

	//# base filenames

	//TODO for all base filename and extension methods implement a way to ignore invalid extensions, e.g. with spaces or that are empty, such as "Hello World. Nice to see you..txt"

	/**
	 * Creates a pattern for matching a base filename (the given base name followed by one or more filename extensions).
	 * @param baseFilename The filename base name to match.
	 * @return A pattern for for matching filenames against the given base name.
	 */
	public static Pattern getBaseFilenamePattern(@Nonnull final String baseFilename) {
		return Pattern.compile(Pattern.quote(baseFilename) + "\\..+"); //TODO test
	}

	/**
	 * Appends a given string to the end of a filename before the extension, if any. This is useful for forming a locale-aware filename, such as
	 * <code>test_fr.txt</code> from <code>test.txt</code>.
	 * @apiNote Here "base filename" refers to the filename with <em>all</em> extensions removed. That is both <code>example.bar</code> and
	 *          <code>example.foo.bar</code> would result in a base filename of <code>example</code>.
	 * @param filename The filename that may contain an extension.
	 * @param charSequence The characters to append to the filename.
	 * @return A filename with the given string appended before the filename extension, if any.
	 */
	public static String appendBase(@Nonnull final String filename, @Nonnull final CharSequence charSequence) {
		final int separatorIndex = filename.indexOf(EXTENSION_SEPARATOR); //see if we can find the extension separator
		final int insertionIndex = separatorIndex >= 0 ? separatorIndex : filename.length(); //insert the characters before the extension or, if there is no extension, at the end of the string
		return StringBuilders.insert(new StringBuilder(filename), insertionIndex, charSequence).toString(); //create a new string builder, insert the characters, and return the new string
	}

	/**
	 * Appends a given string to the end of a filename before the extension, if any. This is useful for forming a locale-aware filename, such as
	 * <code>test_fr.txt</code> from <code>test.txt</code>.
	 * @apiNote Here "base filename" refers to the filename with <em>all</em> extensions removed. That is both <code>example.bar</code> and
	 *          <code>example.foo.bar</code> would result in a base filename of <code>example</code>.
	 * @param filename The filename that may contain an extension.
	 * @param charSequence The characters to append to the filename.
	 * @return A filename with the given string appended before the filename extension, if any.
	 * @deprecated to be removed in favor of {@link #appendBase(String, CharSequence)}.
	 */
	@Deprecated
	public static String appendBaseFilename(final String filename, final CharSequence charSequence) {
		return appendBase(filename, charSequence);
	}

	/**
	 * Retrieves a base filename with no extensions
	 * @apiNote Here "base filename" refers to the filename with <em>all</em> extensions removed. That is both <code>example.bar</code> and
	 *          <code>example.foo.bar</code> would result in a base filename of <code>example</code>.
	 * @param filename The filename that may contain an extension.
	 * @return A filename with all extensions, if any, removed.
	 */
	public static String getBase(@Nonnull final String filename) {
		final int separatorIndex = filename.indexOf(EXTENSION_SEPARATOR); //see if we can find the extension separator
		return separatorIndex >= 0 ? filename.substring(0, separatorIndex) : filename; //insert the characters before the extension or, if there is no extension, at the end of the string
	}

	/**
	 * Retrieves a base filename with no extensions
	 * @apiNote Here "base filename" refers to the filename with <em>all</em> extensions removed. That is both <code>example.bar</code> and
	 *          <code>example.foo.bar</code> would result in a base filename of <code>example</code>.
	 * @param filename The filename that may contain an extension.
	 * @return A filename with all extensions, if any, removed.
	 * @deprecated to be removed in favor of {@link #getBase(String)}.
	 */
	@Deprecated
	public static String getBaseFilename(final String filename) {
		return getBase(filename);
	}

	//# filenames

	/**
	 * Checks to ensure that a particular string is a valid filename across operating systems.
	 * @param string The string of characters which may represent a filename.
	 * @return <code>true</code> if the string contains no illegal filename characters.
	 */
	public static boolean isCrossPlatformFilename(final String string) {
		return isValidFilename(string, CROSS_PLATFORM_RESERVED_CHARACTERS, CROSS_PLATFORM_RESERVED_FINAL_CHARACTERS); //check the filename using cross-platform reserved characters
	}

	//## dotfiles

	/**
	 * Determines whether the filename is for a so-called "dotfile", beginning with a dot but including at least one other character. This method does not
	 * consider <code>"."</code> and <code>".."</code> to be dotfile filenames.
	 * @implSpec The current implementation currently does not consider whether the filename contains slashes of any sort, assuming that the string is actually a
	 *           filename if it is non-empty.
	 * @param filename The filename to check.
	 * @return <code>true</code> if the filename is considered a dotfile.
	 * @throws IllegalArgumentException if the filename is the empty string, which is not a valid filename.
	 * @see <a href="https://superuser.com/q/757635/954883">Why do some file/folder names on Windows have a dot in front of them?</a>
	 * @see <a href="https://wiki.archlinux.org/index.php/Dotfiles">Dotfiles</a>
	 * @see #DOTFILE_PREFIX
	 */
	public static boolean isDotfileFilename(@Nonnull final CharSequence filename) {
		final int length = filename.length();
		checkArgument(length > 0, "Empty filenames are not valid.");
		return (length > 1) //A dotfile has at least two characters (e.g. "." is not a dotfile), …
				&& filename.charAt(0) == DOTFILE_PREFIX //… starts with '.', …
				&& (filename.charAt(1) != DOTFILE_PREFIX || length > 2); //… but is not "..".
	}

	/**
	 * Checks to ensure that a particular string is a valid filename for the operating system.
	 * <p>
	 * The reserved characters of the current operating system will be used.
	 * </p>
	 * @param string The string of characters which may represent a filename.
	 * @return <code>true</code> if the string contains no illegal filename characters.
	 */
	public static boolean isValidFilename(final String string) {
		if(isWindowsOS()) {
			return isValidFilename(string, WINDOWS_RESERVED_CHARACTERS, WINDOWS_RESERVED_FINAL_CHARACTERS); //check the filename using Windows reserved characters
		} else { //for all other operating systems TODO fix for Macintosh
			return isValidFilename(string, POSIX_RESERVED_CHARACTERS, null); //check the filename for POSIX
		}
	}

	/**
	 * Checks to ensure that a particular string is a valid filename.
	 * @param string The string of characters which may represent a filename.
	 * @param reservedCharacters The reserved characters which should be encoded.
	 * @param reservedFinalCharacters The characters that should be encoded if they appear in the final position of the filename, or <code>null</code> if the
	 *          final character doesn't have to meet special rules.
	 * @return <code>true</code> if the string contains no reserved filename characters.
	 */
	public static boolean isValidFilename(final String string, final Characters reservedCharacters, final Characters reservedFinalCharacters) {
		//the string is a filename if the string isn't null and there are no illegal characters in the string
		final boolean isFilename = string != null && !contains(string, reservedCharacters);
		if(isFilename && reservedFinalCharacters != null && !reservedFinalCharacters.isEmpty()) { //if we should check the final character
			if(string.length() > 0) { //if we have any characters at all
				final char lastChar = string.charAt(string.length() - 1); //see what the last character is
				if(reservedFinalCharacters.contains(lastChar)) { //if the last character is reserved
					return false; //this is not a valid filename
				}
			}
		}
		return isFilename; //return what we thought to begin with
	}

	//## encode/decode

	/**
	 * Escape all reserved filename characters to a two-digit <em>uppercase</em> hex representation using <code>'^'</code> as an escape character so that the
	 * filename can be used across operating systems.
	 * <p>
	 * Note that this encodes path separators, and therefore this method should only be called on filenames, not paths.
	 * </p>
	 * @param filename The filename string to be encoded.
	 * @return The string modified to be a filename.
	 * @see #CROSS_PLATFORM_RESERVED_CHARACTERS
	 * @see #CROSS_PLATFORM_RESERVED_FINAL_CHARACTERS
	 * @see #ESCAPE_CHAR
	 * @see CharSequences#escapeHex(CharSequence, Characters, Characters, int, char, int, Case)
	 * @see #isValidFilename(String, Characters, Characters)
	 */
	public static String encodeCrossPlatformFilename(final String filename) {
		return encodeFilename(filename, CROSS_PLATFORM_RESERVED_CHARACTERS, CROSS_PLATFORM_RESERVED_FINAL_CHARACTERS); //encode the filename using cross-platform reserved characters
	}

	/**
	 * Escape all reserved filename characters to a two-digit <em>uppercase</em> hex representation using <code>'^'</code> as an escape character.
	 * <p>
	 * Note that this encodes path separators, and therefore this method should only be called on filenames, not paths.
	 * </p>
	 * <p>
	 * The filename is encoded using the reserved characters of the current operating system.
	 * </p>
	 * @param filename The filename string to be encoded.
	 * @return The string modified to be a filename.
	 * @see #WINDOWS_RESERVED_FINAL_CHARACTERS
	 * @see #POSIX_RESERVED_CHARACTERS
	 * @see #ESCAPE_CHAR
	 * @see CharSequences#escapeHex(CharSequence, Characters, Characters, int, char, int, Case)
	 * @see #isValidFilename(String, Characters, Characters)
	 */
	public static String encodeFilename(final String filename) {
		if(isWindowsOS()) {
			return encodeFilename(filename, WINDOWS_RESERVED_CHARACTERS, WINDOWS_RESERVED_FINAL_CHARACTERS); //encode the filename using Windows reserved characters
		} else { //for all other operating systems TODO fix for Macintosh
			return encodeFilename(filename, POSIX_RESERVED_CHARACTERS, null); //encode the filename for POSIX
		}
	}

	/**
	 * Escape all reserved filename characters to a two-digit <em>uppercase</em> hex representation using <code>'^'</code> as an escape character.
	 * <p>
	 * Note that this encodes path separators, and therefore this method should only be called on filenames, not paths.
	 * </p>
	 * @param filename The filename string to be encoded.
	 * @param reservedCharacters The reserved characters which should be encoded.
	 * @param reservedFinalCharacters The characters that should be encoded if they appear in the final position of the filename, or <code>null</code> if the
	 *          final character doesn't have to meet special rules.
	 * @return The string modified to be a filename.
	 * @see #ESCAPE_CHAR
	 * @see CharSequences#escapeHex(CharSequence, Characters, Characters, int, char, int, Case)
	 * @see #isValidFilename(String, Characters, Characters)
	 */
	public static String encodeFilename(final String filename, final Characters reservedCharacters, final Characters reservedFinalCharacters) {
		//check to see if this is already a valid filename; if so (it usually is), this will give us a performance increase
		//even if this is a valid filename, make sure it doesn't have the escape character in it---we would have to escape that, too, even though it isn't reserved
		if(isValidFilename(filename, reservedCharacters, reservedFinalCharacters) //if this is a valid filename already	
				&& filename.indexOf(ESCAPE_CHAR) < 0) { //if the filename doesn't contain the escape character	
			return filename; //return the string as is---it already is a valid filename
		} else { //if something about the filename isn't correct
			final String encodedFilename = escapeHex(filename, null, reservedCharacters, Integer.MAX_VALUE, ESCAPE_CHAR, 2, Case.UPPERCASE);
			if(reservedFinalCharacters != null && !reservedFinalCharacters.isEmpty()) { //if we should check the final character (e.g. on Windows)
				if(encodedFilename.length() > 0) { //if we have a filename
					final char lastChar = encodedFilename.charAt(encodedFilename.length() - 1); //see what the last character is
					if(reservedFinalCharacters.contains(lastChar)) { //if the last character is a reserved character
						final String replacementString = escapeHex(String.valueOf(lastChar), null, Characters.of(lastChar), Integer.MAX_VALUE, ESCAPE_CHAR, 2,
								Case.UPPERCASE); //escape the last character						
						return encodedFilename.substring(0, encodedFilename.length() - 1) + replacementString; //replace the last character with its escaped form
					}
				}
			}
			return encodedFilename; //return the encoded filename since we didn't need to modify it further
		}
	}

	/**
	 * Unescapes all characters in a string that are encoded using <code>'^'</code> as an escape character followed by two hex digits.
	 * @param filename The filename string to be decoded.
	 * @return The filename string decoded back to a normal string.
	 * @see #ESCAPE_CHAR
	 * @see CharSequences#unescapeHex(CharSequence, char, int)
	 */
	public static String decodeFilename(final String filename) {
		return unescapeHex(filename, ESCAPE_CHAR, 2).toString(); //decode the filename
	}

	//# extensions

	/**
	 * Returns all the possible extensions of a filename, from the most specific to the most general.
	 * <p>
	 * For example for the filename <code>example.foo.bar</code> the following would be returned in order:
	 * </p>
	 * <ol>
	 * <li><code>foo.bar</code></li>
	 * <li><code>bar</code></li>
	 * </ol>
	 * @param filename The filename for which extensions should be returned.
	 * @return A stream of extensions of the given filename.
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
	 * @param filename The filename for which extensions should be returned.
	 * @return An iterable to iterate over the extensions of the given filename.
	 * @see FilenameExtensionIterator
	 */
	public static Iterable<String> getExtensions(final CharSequence filename) {
		return () -> new FilenameExtensionIterator(filename);
	}

	/**
	 * Adds the given extension to a filename and returns the new filename with the new extension. The name is not checked to see if it currently has an
	 * extension.
	 * <p>
	 * This method currently allows an extension with the <code>.</code> delimiter, but it may be prohibited in the future.
	 * </p>
	 * @param filename The filename name to which to add an extension.
	 * @param extension The extension to add.
	 * @return The name with the new extension.
	 * @throws NullPointerException if the given extension is <code>null</code>.
	 */
	public static String addExtension(final String filename, final String extension) {
		return new StringBuilder(filename).append(EXTENSION_SEPARATOR).append(requireNonNull(extension, "Extension cannot be null")).toString(); //add the requested extension and return the new filename
	}

	/**
	 * Changes the last extension of a filename and returns a new filename with the new extension. If the filename does not currently have an extension, one will
	 * be added.
	 * @param filename The filename to examine.
	 * @param extension The extension to set, or <code>null</code> if the extension should be removed.
	 * @return The filename with the new extension.
	 * @throws IllegalArgumentException If the name is empty, or if the name is just a "/".
	 */
	public static String changeExtension(String filename, final String extension) {
		final int separatorIndex = filename.lastIndexOf(EXTENSION_SEPARATOR); //see if we can find the extension separator
		if(separatorIndex >= 0) { //if we found a separator
			filename = filename.substring(0, separatorIndex); //remove the extension
		}
		if(extension != null) { //if an extension was given
			filename = addExtension(filename, extension); //add the requested extension
		}
		return filename; //return the new filename
	}

	/**
	 * Extracts the extension from a filename.
	 * @param filename The filename to examine.
	 * @return The extension of the name (not including '.'), which may not be present.
	 */
	public static Optional<String> findExtension(@Nonnull final String filename) {
		final int separatorIndex = filename.lastIndexOf(EXTENSION_SEPARATOR); //see if we can find the extension separator, which will be the last such character in the string
		return separatorIndex >= 0 ? Optional.of(filename.substring(separatorIndex + 1)) : Optional.empty(); //if we found a separator, return everything after it 
	}

	/**
	 * Extracts the extension from a filename.
	 * @param filename The filename to examine.
	 * @return The extension of the name (not including '.'), which may not be present.
	 * @deprecated to be removed in favor of {@link #findExtension(String)}.
	 */
	@Deprecated
	public static String getExtension(@Nonnull final String filename) {
		return findExtension(filename).orElse(null);
	}

	/**
	 * Removes the last extension, if any, of a filename and returns a new filename with no extension. This is a convenience method that delegates to
	 * {@link #changeExtension(String, String)}.
	 * @param filename The name to examine.
	 * @return The name with no extension.
	 */
	public static String removeExtension(final String filename) {
		return changeExtension(filename, null); //replace the extension with nothing
	}

	/**
	 * Adds the extension, if any, to a filename and returns the new filename. This is a convenience method that delegates to
	 * {@link #addExtension(String, String)} if a non-<code>null</code> extension is given.
	 * @param filename The filename to examine.
	 * @param extension The extension to add, or <code>null</code> if no extension should be added.
	 * @return The name with the new extension, if any.
	 */
	public static String setExtension(final String filename, final String extension) {
		return extension != null ? addExtension(filename, extension) : filename; //if an extension was given, add it; otherwise, return the name unmodified
	}

}
