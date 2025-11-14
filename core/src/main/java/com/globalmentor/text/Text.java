/*
 * Copyright © 1996-2012 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.text;

import java.io.UnsupportedEncodingException;
import java.text.Collator;
import java.util.regex.Pattern;

import org.jspecify.annotations.*;

import static java.util.Objects.*;

import com.globalmentor.collections.comparators.SortOrder;
import com.globalmentor.java.Characters;
import com.globalmentor.net.MediaType;

import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Characters.*;

/**
 * Constants and utilities for text.
 * @author Garret Wilson
 */
public final class Text {

	private Text() {
	}

	/** The MIME subtype of <code>text/plain</code>. */
	public static final String PLAIN_SUBTYPE = "plain";

	/** The media type for plain text: <code>text/plain</code>. */
	public static final MediaType PLAIN_MEDIA_TYPE = MediaType.of(MediaType.TEXT_PRIMARY_TYPE, PLAIN_SUBTYPE);

	/** The name extension for log files. */
	public static final String LOG_FILENAME_EXTENSION = "log";
	/**
	 * The name extension for text list files.
	 * @see <a href="https://www.file-extensions.org/lst-file-extension">LST file extension - List text</a>
	 */
	public static final String LST_FILENAME_EXTENSION = "lst";
	/** The name extension for text files. */
	public static final String TXT_FILENAME_EXTENSION = "txt";

	/**
	 * The string representing the CR EOL character sequence.
	 * @see Characters#CARRIAGE_RETURN_CHAR
	 */
	public static final String CARRIAGE_RETURN_STRING = String.valueOf(CARRIAGE_RETURN_CHAR);

	/**
	 * The string representing the LF EOL character sequence.
	 * @see Characters#LINE_FEED_CHAR
	 */
	public static final String LINE_FEED_STRING = String.valueOf(LINE_FEED_CHAR);

	/**
	 * The pattern that can split a line based upon linefeeds.
	 * @see Characters#LINE_FEED_CHAR
	 */
	public static final Pattern LINE_FEED_PATTERN = Pattern.compile(LINE_FEED_STRING);

	/**
	 * The string representing the CRLF EOL sequence.
	 * @see Characters#CARRIAGE_RETURN_CHAR
	 * @see Characters#LINE_FEED_CHAR
	 */
	public static final String CRLF_STRING = CARRIAGE_RETURN_STRING + LINE_FEED_STRING;

	/**
	 * Compares two strings for order in ascending order using the specified collator. Returns a negative integer, zero, or a positive integer as the first
	 * argument is less than, equal to, or greater than the second. Identical strings are always considered equal. This method functions exactly as if the two
	 * strings were compared using {@link Collator#compare(String, String)}, except:
	 * <ul>
	 * <li>Identical strings are recognized as such without delegating to the actual {@link Collator#compare(String, String)} method.</li>
	 * <li>This method allows <code>null</code> arguments, considering a <code>null</code> string to be lower than a non-<code>null</code> string.</li>
	 * </ul>
	 * This method matches the semantics of {@link Collator#compare(String, String)}, except that this method allows <code>null</code> arguments.
	 * @param string1 The first string to be compared, or <code>null</code> if the string is not available.
	 * @param string2 The second string to be compared, or <code>null</code> if the string is not available.
	 * @param collator The collator used to perform comparisons.
	 * @return A negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
	 * @throws NullPointerException if the given collator is <code>null</code>.
	 * @throws ClassCastException if the arguments' types prevent them from being compared.
	 * @see Collator#compare(String, String)
	 */
	public static int compare(final String string1, final String string2, final Collator collator) {
		return compare(string1, string2, collator, SortOrder.ASCENDING); //compare in ascending order
	}

	/**
	 * Compares two strings for order using the specified collator with the specified sort order. Returns a negative integer, zero, or a positive integer as the
	 * first argument is less than, equal to, or greater than the second. Identical strings are always considered equal. This method functions exactly as if the
	 * two strings were compared using {@link Collator#compare(String, String)}, except:
	 * <ul>
	 * <li>Identical strings are recognized as such without delegating to the actual {@link Collator#compare(String, String)} method.</li>
	 * <li>This method allows <code>null</code> arguments, considering a <code>null</code> string to be lower than a non-<code>null</code> string.</li>
	 * </ul>
	 * @param string1 The first string to be compared, or <code>null</code> if the string is not available.
	 * @param string2 The second string to be compared, or <code>null</code> if the string is not available.
	 * @param collator The collator used to perform comparisons.
	 * @param sortOrder The order in which to perform comparisons.
	 * @return A negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
	 * @throws NullPointerException if the given collator and/or sort order is <code>null</code>.
	 * @throws ClassCastException if the arguments' types prevent them from being compared.
	 * @see Collator#compare(String, String)
	 */
	public static int compare(final String string1, final String string2, final Collator collator, final SortOrder sortOrder) {
		if(string1 == string2) { //if the strings are identical
			return 0; //identical strings are always equal
		}
		if(string1 != null) { //if the first string is not null
			if(string2 != null) { //if the second string is not null
				return sortOrder == SortOrder.ASCENDING ? collator.compare(string1, string2) : collator.compare(string2, string1); //compare in the requested order
			} else { //if only the first string is not null
				return sortOrder == SortOrder.ASCENDING ? 1 : -1; //null strings should be sorted lower
			}
		} else { //if the first string is null
			assert string2 != null : "Both strings cannot be null, because we already checked for identity.";
			return sortOrder == SortOrder.ASCENDING ? -1 : 1; //null strings should be sorted lower
		}
	}

	/**
	 * Creates a control string according to ECMA-48, "Control Functions for Coded Character Sets", Section 5.6, "Control strings". A control string begins with
	 * the Start of String control character (U+0098) and ends with a String Terminator control character (U+009C). ECMA-48 publication is also approved as
	 * ISO/IEC 6429.
	 * @param string The string from which a control string will be created.
	 * @return An ECMA-48 control string with the given string as its content.
	 * @throws NullPointerException if the given string is <code>null</code>.
	 * @see <a href="http://www.ecma-international.org/publications/standards/Ecma-048.htm">ECMA-48: Control Functions for Coded Character Sets</a>
	 */
	public static final String createControlString(final String string) {
		return START_OF_STRING_CHAR + requireNonNull(string, "String cannot be null.") + STRING_TERMINATOR_CHAR; //wrap the string with a SOS/ST pair
	}

	/**
	 * Determines if the given media type is one representing text in some form.
	 * <p>
	 * Text media types include:
	 * </p>
	 * <ul>
	 * <li><code>text/*</code></li>
	 * <li><code>application/xml</code></li>
	 * <li><code>application/*+xml</code></li>
	 * <li><code>application/xml-external-parsed-entity</code></li>
	 * <li><code>application/*+xml-external-parsed-entity</code> (not formally defined)</li>
	 * </ul>
	 * @param mediaType The media type of a resource, or <code>null</code> for no media type.
	 * @return <code>true</code> if the given media type is one of several text media types.
	 */
	public static boolean isText(final MediaType mediaType) {
		if(mediaType != null) { //if a content type is given
			if(MediaType.TEXT_PRIMARY_TYPE.equals(mediaType.getPrimaryType())) { //if this is "text/*"
				return true; //text/* is a text content type
			}
			//TODO improve; see if removing this causes problems in Guise; application/xml could be considered non-text xml; see www.grauw.nl/blog/entry/489
			if(MediaType.APPLICATION_PRIMARY_TYPE.equals(mediaType.getPrimaryType())) { //if this is "application/*"
				final String subType = mediaType.getSubType(); //get the subtype
				if("xml".equals(subType) //see if the subtype is "xml"
						|| mediaType.hasSubTypeSuffix("xml")) { //see if the subtype has an XML suffix
					return true; //application/*+xml is considered text
				}
				if("xml-external-parsed-entity".equals(subType) //if the subtype is /xml-external-parsed-entity
						|| mediaType.hasSubTypeSuffix("xml-external-parsed-entity")) { //or if the subtype has an XML external parsed entity suffix
					return true; //application/*+xml-external-parsed-entity is considered text
				}
			}
		}
		return false; //this is not a media type we recognize as being HTML
	}

	/**
	 * Re-encodes the given string to the new encoding (such as UTF-8), assuming the string was encoded from an array of bytes using the old encoding (e.g.
	 * ISO-8859-1).
	 * @param string The string to recode.
	 * @param oldEncoding The encoding used to create the string originally.
	 * @param newEncoding The new encoding to use when creating the string.
	 * @return The a string created from encoding the characters in the specified new encoding.
	 * @throws UnsupportedEncodingException Thrown if either the old encoding or the new encoding is not supported.
	 */
	public static String recode(final String string, final String oldEncoding, final String newEncoding) throws UnsupportedEncodingException {
		final byte[] bytes = string.getBytes(oldEncoding); //get the bytes of the string as they were before they were encoded
		return new String(bytes, newEncoding); //create a string from the bytes using the new encoding
	}

	/**
	 * Checks the result of an expression to see if an argument syntax is correct, and throws an {@link ArgumentSyntaxException} if the value is
	 * <code>false</code>.
	 * @apiNote This is a precondition check.
	 * @apiNote This method is typically used in a static factory {@code parse(CharSequence)} method such as {@link java.time.LocalDate#parse(CharSequence)}.
	 * @param test The result of the test.
	 * @throws ArgumentSyntaxException if the given value is <code>false</code>.
	 */
	public static void checkArgumentSyntax(final boolean test) {
		checkArgumentSyntax(test, null);
	}

	/**
	 * Checks the result of an expression to see if an argument syntax is correct, and throws an {@link ArgumentSyntaxException} if the value is
	 * <code>false</code>.
	 * @apiNote This is a precondition check.
	 * @apiNote This method is typically used in a static factory {@code parse(CharSequence)} method such as {@link java.time.LocalDate#parse(CharSequence)}.
	 * @param test The result of the test.
	 * @param description A description of the test to be used when generating an exception, optionally formatted with arguments, or {@code null} for no
	 *          description.
	 * @param arguments The arguments to be applied when formatting, or an empty array if the message should not be formatted.
	 * @throws NullPointerException if the given arguments is {@code null}.
	 * @throws ArgumentSyntaxException if the given value is <code>false</code>.
	 * @throws IllegalArgumentException if the description is an invalid pattern, or if an argument in the arguments array is not of the type expected by the
	 *           format element(s) that use it.
	 * @see String#format(String, Object...)
	 */
	public static void checkArgumentSyntax(final boolean test, @Nullable String description, @NonNull final Object... arguments) {
		if(!test) {
			if(description != null && arguments.length > 0) {
				description = String.format(description, arguments);
			}
			throw new ArgumentSyntaxException(description);
		}
	}

	/**
	 * Normalizes end-of-line sequences in the character sequence to <code>LF</code>. The following sequences are normalized:
	 * <ul>
	 * <li><code>CR</code></li>
	 * <li><code>LF</code></li>
	 * <li><code>CRLF</code></li>
	 * </ul>
	 * @implSpec This implementation delegates to {@link #normalizeEol(CharSequence, CharSequence)}
	 * @implSpec This implementation recognizes an input sequence that require no change, and simply returns that character sequence unchanged.
	 * @param charSequence The character sequence to normalize.
	 * @return A character sequence, which may be mutable, with the ends of lines normalized to <code>LF</code>.
	 * @throws NullPointerException if the given character sequence and/or EOL characters is <code>null</code>.
	 * @see #LINE_FEED_STRING
	 */
	public static CharSequence normalizeEol(@NonNull final CharSequence charSequence) {
		return normalizeEol(charSequence, LINE_FEED_STRING);
	}

	/**
	 * Normalizes end-of-line sequences in the character sequence to a given sequence. The following sequences are normalized to the provided EOL:
	 * <ul>
	 * <li><code>CR</code></li>
	 * <li><code>LF</code></li>
	 * <li><code>CRLF</code></li>
	 * </ul>
	 * @implSpec This implementation recognizes an input sequence that require no change if the normalization EOL sequence is a single character that is not
	 *           <code>CR</code>, and simply returns that character sequence unchanged.
	 * @param charSequence The character sequence to normalize.
	 * @param normalEol The end of line characters to which to normalize the ends of lines.
	 * @return A character sequence, which may be mutable, with the ends of lines normalized to the given end-of-line character sequence.
	 * @throws NullPointerException if the given character sequence and/or EOL characters is <code>null</code>.
	 */
	public static CharSequence normalizeEol(@NonNull final CharSequence charSequence, @NonNull final CharSequence normalEol) {
		final int length = charSequence.length();
		final int normalEolLength = normalEol.length();
		final char normalEolChar = normalEolLength == 1 ? normalEol.charAt(0) : 0; //the single normalization EOL character, or -1 if there are more than one
		final boolean detectNoChange = normalEolLength == 1 && normalEolChar != CARRIAGE_RETURN_CHAR; //CR could be part of CRLF
		int currentIndex = 0; //start searching from the beginning
		int resultIndex;
		StringBuilder stringBuilder = null; //don't create a string builder unless we need to
		while(currentIndex < length) { //keep searching until we finish the string
			resultIndex = indexOfLength(charSequence, EOL_CHARACTERS, currentIndex); //perform the next search
			if(stringBuilder == null) { //if we don't yet have a string builder, see if we really need to create one
				if(resultIndex < length) { //if we found an EOL character
					if(detectNoChange) { //this implementation can only detect no change for a single normalized EOL character that isn't CR
						if(charSequence.charAt(resultIndex) == normalEolChar) {
							currentIndex = resultIndex + 1; //skip the existing character---it's what we expected
							continue;
						}
					}
				} else { //if there are no EOL characters in the entire character sequence
					break; //there's no need to modify the character sequence
				}
				stringBuilder = new StringBuilder(normalEolLength > 1 ? length + 16 : length); //allow for extra room if the EOLs might expand
				stringBuilder.append(charSequence, 0, resultIndex); //add all the characters that aren't EOL characters, even if this is not our first detection
			} else {
				stringBuilder.append(charSequence, currentIndex, resultIndex); //add only the new characters that aren't EOL characters
			}
			int skipEolCount = 1; //assume we'll just skip one character
			if(resultIndex < length) { //if we aren't out of characters, yet
				stringBuilder.append(normalEol); //append the EOL sequence
				final char eolChar = charSequence.charAt(resultIndex); //get the EOL character we found
				if(eolChar == CARRIAGE_RETURN_CHAR) { //if this is a CR, see if it is a CRLF
					final int nextIndex = resultIndex + 1; //get the index of the next character
					if(nextIndex < length && charSequence.charAt(nextIndex) == LINE_FEED_CHAR) { //if the next character is an LF
						++skipEolCount; //skip the next character
					}
				}
			}
			currentIndex = resultIndex + skipEolCount; //skip the EOL characters
		}
		return stringBuilder != null ? stringBuilder : charSequence; //return the string we constructed, or the character sequence if there were no EOL character
	}

}
