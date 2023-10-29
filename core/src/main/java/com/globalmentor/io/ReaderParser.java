/*
 * Copyright © 1996-2017 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static java.util.Objects.*;

import java.io.*;

import javax.annotation.*;

import com.globalmentor.java.Characters;

import static com.globalmentor.io.Readers.*;
import static com.globalmentor.java.Conditions.*;

/**
 * Parsing methods that work on a {@link Reader}. The reader must support marking.
 * <p>
 * This parser supports several general operations:
 * </p>
 * <dl>
 * <dt><dfn>check</dfn></dt>
 * <dd>Checks the parsed content, throwing an exception if the content does not match that expected. The content is consumed. Implies that an exception will be
 * thrown if the end of the content is reached. Semantically a "check" operation is equivalent to a "confirm" operation that throws an exception if the result
 * is <code>false</code>.</dd>
 * <dt><dfn>confirm</dfn></dt>
 * <dd>Compares the content and returns whether it matches that expected. If the content matches, it is consumed. If the content does not match, it is not
 * consumed. No exception is thrown based upon the match.</dd>
 * <dt><dfn>pass</dfn></dt>
 * <dd>Skips content until some delimiter is encountered. The reached content is discarded. By default no exception is thrown if the end of the content is
 * reached, unless the method indicates that reaching the delimiter is required. The semantics of "reach" is the same as "read past", discarding content.</dd>
 * <dt><dfn>reach</dfn></dt>
 * <dd>Skips content until some delimiter is encountered. The reached content is not consumed. By default no exception is thrown if the end of the content is
 * reached, unless the method indicates that reaching the delimiter is required. The semantics of "reach" is the same as "read until", discarding content.</dd>
 * <dt><dfn>read</dfn></dt>
 * <dd>Reads content; either "while" some content is encountered, or "until" some content is encountered (which will not be read), or "past" some content (which
 * will be read). By default no exception is thrown if the end of the content is reached, unless the method indicates that reaching the delimiter is
 * required.</dd>
 * <dt><dfn>skip</dfn></dt>
 * <dd>Skips and discards content. No exception is thrown if the end of the content is reached. The semantics of "skip" is the same as "read while", discarding
 * content.</dd>
 * </dl>
 * @author Garret Wilson
 * @see Reader#markSupported()
 */
public class ReaderParser {

	/**
	 * The minimum number of characters to mark in a reader. This value is guaranteed to be at least <code>1</code> for marking a single character. If fewer than
	 * the minimum characters are marked, an ancient bug in the {@link LineNumberReader} could results in an {@link IOException} for "mark invalid" if a
	 * <code>CRLF</code> happens to straddle the end the buffer.
	 * @see <a href="https://bugs.java.com/bugdatabase/view_bug.do?bug_id=JDK-8218280">JDK-8218280 : LineNumberReader throws "Mark invalid" exception if CRLF
	 *      straddles buffer.</a>
	 */
	public static final int MINIMUM_MARK = 2;

	/**
	 * Checks for a parsing condition and, if the test did not pass, throws a {@link ParseIOException}.
	 * @param reader The reader the contents of which to be parsed.
	 * @param test The result of the test.
	 * @param description A description of the test to be used when generating an exception, optionally formatted with arguments, or <code>null</code> for no
	 *          description.
	 * @param arguments The arguments to be applied when formatting, or an empty array if the message should not be formatted.
	 * @throws NullPointerException if the given arguments is <code>null</code>.
	 * @throws ParseIOException if the given value is <code>false</code>.
	 * @throws ParseIOException if the description is an invalid pattern, or if an argument in the arguments array is not of the type expected by the format
	 *           element(s) that use it.
	 * @see String#format(String, Object...)
	 */
	public static void checkParseIO(@Nonnull final Reader reader, final boolean test, @Nullable String description, final Object... arguments)
			throws ParseIOException {
		if(!test) { //format the message if appropriate
			if(description != null && arguments.length > 0) {
				description = String.format(description, arguments);
			}
			throw new ParseIOException(reader, description);
		}
	}

	/**
	 * Checks that reader has no more data.
	 * @param reader The reader the contents of which to be parsed.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseUnexpectedDataException if the reader is not at the end of data.
	 */
	public static void checkReaderEnd(@Nonnull final Reader reader) throws IOException, ParseUnexpectedDataException {
		final int c = reader.read(); //get the current character
		if(c >= 0) { //if this character is valid (the reader is not out of data)
			throw new ParseUnexpectedDataException(reader, "Expected end of data; found " + Characters.getLabel(c) + ".", (char)c);
		}
	}

	/**
	 * Checks that a read character does not represent the end of the reader's data.
	 * @param reader The reader the contents of which to be parsed.
	 * @param c The character returned from a reader's {@link Reader#read()} operation.
	 * @return The given character.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @throws ParseEndException if the given character represents the end of the reader's data.
	 */
	public static int checkReaderNotEnd(@Nonnull final Reader reader, final int c) throws ParseEndException {
		if(c < 0) { //if this returned character represents the end of the reader's data
			throw new ParseEndException(reader);
		}
		return c;
	}

	/**
	 * Checks that the current character matches a specific character and advances to the next character.
	 * @param reader The reader the contents of which to be parsed.
	 * @param character The character against which which the current character should be checked.
	 * @return The character returned the reader's {@link Reader#read()} operation.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseUnexpectedDataException if the current character in the reader does not match the specified character.
	 * @throws ParseEndException if the reader has no more characters.
	 * @see #confirm(Reader, char)
	 */
	public static char check(@Nonnull final Reader reader, final char character) throws IOException, ParseUnexpectedDataException {
		final char c = readRequired(reader); //read the next character
		if(c != character) { //if this character does not match what we expected
			throw new ParseUnexpectedDataException(reader, character, c);
		}
		return c; //return the character read
	}

	/**
	 * Checks that the current character matches a given set of characters and advances to the next character.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters to accept.
	 * @return The character returned the reader's {@link Reader#read()} operation.
	 * @throws NullPointerException if the given reader and/or the given characters is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseUnexpectedDataException if the current character in the reader does not match one of the specified characters.
	 * @throws ParseEndException if the reader has no more characters.
	 */
	public static char check(@Nonnull final Reader reader, @Nonnull final Characters characters) throws IOException, ParseUnexpectedDataException {
		final char c = readRequired(reader); //read the next character
		if(!characters.contains(c)) { //if this character does not match one of the expected characters
			throw new ParseUnexpectedDataException(reader, characters, c);
		}
		return c; //return the character read
	}

	/**
	 * Checks that the a certain number of characters matches a given set of characters and advances to the next character.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters to accept.
	 * @param count The number of characters to read.
	 * @return A string containing the characters returned the reader's {@link Reader#read()} operation.
	 * @throws NullPointerException if the given reader and/or the given characters is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseUnexpectedDataException if one of the characters in the reader does not match one of the specified characters.
	 * @throws ParseEndException if the reader has no more characters.
	 */
	public static String checkCount(@Nonnull final Reader reader, @Nonnull final Characters characters, final int count)
			throws IOException, ParseUnexpectedDataException {
		return checkCount(reader, characters, count, new StringBuilder()).toString();
	}

	/**
	 * Checks that the a certain number of characters matches a given set of characters and advances to the next character.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters to accept.
	 * @param count The number of characters to read.
	 * @param stringBuilder The string builder to receive the characters read.
	 * @return The given string builder with the added characters.
	 * @throws NullPointerException if the given reader, characters, and/or string builder is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseUnexpectedDataException if one of the characters in the reader does not match one of the specified characters.
	 * @throws ParseEndException if the reader has no more characters.
	 */
	public static StringBuilder checkCount(@Nonnull final Reader reader, @Nonnull final Characters characters, int count,
			@Nonnull final StringBuilder stringBuilder) throws IOException, ParseUnexpectedDataException {
		for(; count > 0; count--) {
			stringBuilder.append(check(reader, characters));
		}
		return stringBuilder;
	}

	/**
	 * Checks that the current and subsequent characters matches a specified character sequence.
	 * @param reader The reader the contents of which to be parsed.
	 * @param match The character sequence with which the current characters should be checked.
	 * @return The character sequence that was checked.
	 * @throws NullPointerException if the given reader and/or match character sequence is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseUnexpectedDataException if the current character in the reader does not match the specified character sequence.
	 * @throws ParseEndException if the reader has no more characters.
	 */
	public static CharSequence check(@Nonnull final Reader reader, @Nonnull final CharSequence match) throws IOException, ParseUnexpectedDataException {
		final int matchLength = match.length(); //get the length to match
		for(int i = 0; i < matchLength; ++i) { //for each match index
			check(reader, match.charAt(i)); //compare the current character with the match character
		}
		return match; //return the matched character sequence
	}

	/**
	 * Reads a character and, if a character does not match the given character, resets the reader as if the character were not read.
	 * @param reader The reader the contents of which to be parsed.
	 * @param character The expected character.
	 * @return <code>true</code> if the given character was read, or <code>false</code> if the next character is not the expected character and was therefore
	 *         replaced if the end of the reader was not reached.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 */
	public static boolean confirm(@Nonnull final Reader reader, final char character) throws IOException {
		reader.mark(MINIMUM_MARK); //mark our current position
		final int c = reader.read(); //get the current character
		if(c >= 0) { //if the end of the reader was not reached
			if(c == character) { //if the expected character was read
				return true; //indicate that the character was the one expected
			} else { //if the character was not the one expected
				reader.reset(); //reset to the last mark, which was set right before the character we found
			}
		}
		return false; //indicate that another character was encountered or the end of the reader was reached
	}

	/**
	 * Reads a character and, if a character does not match the given set of characters, resets the reader as if the character were not read.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters to accept.
	 * @return <code>true</code> if the given character was read, or <code>false</code> if the next character is not one of the expected characters and was
	 *         therefore replaced if the end of the reader was not reached.
	 * @throws NullPointerException if the given reader and/or characters is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 */
	public static boolean confirm(@Nonnull final Reader reader, @Nonnull final Characters characters) throws IOException {
		reader.mark(MINIMUM_MARK); //mark our current position
		final int c = reader.read(); //get the current character
		if(c >= 0) { //if the end of the reader was not reached
			if(characters.contains((char)c)) { //if one of the expected characters was read
				return true; //indicate that the character was the one expected
			} else { //if the character was not the one expected
				reader.reset(); //reset to the last mark, which was set right before the character we found
			}
		}
		return false; //indicate that another character was encountered or the end of the reader was reached
	}

	/**
	 * Reads a string and, if the string does not match the given character sequence, resets the reader as if the string was not read.
	 * @param reader The reader the contents of which to be parsed.
	 * @param charSequence The character sequence to accept.
	 * @return <code>true</code> if a string matching the character sequence was read, or <code>false</code> if the next characters do not match the given
	 *         character sequence and were therefore replaced.
	 * @throws NullPointerException if the given reader and/or character sequence is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 */
	public static boolean confirm(@Nonnull final Reader reader, @Nonnull final CharSequence charSequence) throws IOException {
		final int length = charSequence.length();
		final char[] buffer = new char[length]; //create a buffer for reading all the characters more efficiently than one at a time
		reader.mark(length); //mark our current position
		if(read(reader, buffer) < length) { //read the characters; if there weren't enough characters left before the end
			reader.reset(); //reset to the last mark, right before we read the string
			return false;
		}
		for(int i = 0; i < length; ++i) { //check all the characters
			if(buffer[i] != charSequence.charAt(i)) { //if a character doesn't match
				reader.reset(); //reset to the last mark, right before we read the string
				return false;
			}
		}
		return true;
	}

	/**
	 * Consumes characters in a reader that appear within a given set of characters and optionally collects those characters in a string builder. The new position
	 * will either be the first character not in the characters or the end of the reader.
	 * <p>
	 * This is a generalized reading method used by the higher-level semantic methods.
	 * </p>
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters to consume.
	 * @param stringBuilder The string builder to collect the characters consumed, or <code>null</code> if the consumed characters should be discarded.
	 * @return The next character that will be returned the reader's {@link Reader#read()} operation, or <code>-1</code> if the end of the reader has been
	 *         reached.
	 * @throws NullPointerException if the given reader and/or the given characters is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 */
	static int consumeWhile(@Nonnull final Reader reader, @Nonnull final Characters characters, @Nullable final StringBuilder stringBuilder) throws IOException {
		int c; //the character read
		boolean consume; //we'll note when we should consume
		do {
			consume = false; //start out assuming we shouldn't skip this character
			reader.mark(MINIMUM_MARK); //mark our current position TODO testing; line number reader may read several lines; check other locations
			c = reader.read(); //read another character
			if(c < 0) { //if we're at the end of the reader
				return c; //stop skipping and return without resetting the reader to the mark
			}
			if(characters.contains((char)c)) {
				if(stringBuilder != null) { //if a string builder was given
					stringBuilder.append((char)c); //save the character to be sent back
				}
				consume = true; //indicate that we should consume this character
			}
		} while(consume); //keep reading characters until we find one we shouldn't consume
		reader.reset(); //reset to the last mark, which was set right before the character we found
		return c; //return the next character to be read
	}

	/**
	 * Reads all characters in a reader until one of the given characters is reached. The new position will be that of the given character.
	 * @implSpec This implementation delegates to {@link #consumeUntil(Reader, Characters, char, boolean, StringBuilder, boolean)}.
	 * @param reader The reader the contents of which to be parsed.
	 * @param untilCharacter The character to check if the given characters is <code>null</code>.
	 * @param isEndError Whether reaching the end of the reader is an error condition.
	 * @param stringBuilder The string builder to collect the characters consumed, or <code>null</code> if the consumed characters should be discarded.
	 * @param includeReached Whether the reached character should be included, effectively making this method read <em>past</em> the character.
	 * @return The string builder given, or <code>null</code> if no string builder was provided.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseEndException if the reader has no more characters and the end-is-error flag is <code>true</code>.
	 */
	protected static StringBuilder consumeUntil(@Nonnull final Reader reader, final char untilCharacter, final boolean isEndError,
			@Nullable final StringBuilder stringBuilder, final boolean includeReached) throws IOException, ParseEndException {
		return consumeUntil(reader, null, untilCharacter, isEndError, stringBuilder, includeReached);
	}

	/**
	 * Reads all characters in a reader until one of the given characters is reached. The new position will be that of the given character.
	 * @implSpec This implementation delegates to {@link #consumeUntil(Reader, Characters, char, boolean, StringBuilder, boolean)}.
	 * @param reader The reader the contents of which to be parsed.
	 * @param untilCharacters The characters one of which to reach.
	 * @param isEndError Whether reaching the end of the reader is an error condition.
	 * @param stringBuilder The string builder to collect the characters consumed, or <code>null</code> if the consumed characters should be discarded.
	 * @param includeReached Whether the reached character should be included, effectively making this method read <em>past</em> the character.
	 * @return The string builder given, or <code>null</code> if no string builder was provided.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseEndException if the reader has no more characters and the end-is-error flag is <code>true</code>.
	 */
	protected static StringBuilder consumeUntil(@Nonnull final Reader reader, @Nullable final Characters untilCharacters, final boolean isEndError,
			@Nullable final StringBuilder stringBuilder, final boolean includeReached) throws IOException, ParseEndException {
		return consumeUntil(reader, requireNonNull(untilCharacters), (char)0, isEndError, stringBuilder, includeReached);
	}

	/**
	 * Reads all characters in a reader until one of the given characters is reached. The new position will be that of the given character.
	 * @param reader The reader the contents of which to be parsed.
	 * @param untilCharacters The characters one of which to reach, or <code>null</code> if the given character should be checked instead.
	 * @param untilCharacter The character to check if the given characters is <code>null</code>.
	 * @param isEndError Whether reaching the end of the reader is an error condition.
	 * @param stringBuilder The string builder to collect the characters consumed, or <code>null</code> if the consumed characters should be discarded.
	 * @param includeReached Whether the reached character should be included, effectively making this method read <em>past</em> the character.
	 * @return The string builder given, or <code>null</code> if no string builder was provided.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseEndException if the reader has no more characters and the end-is-error flag is <code>true</code>.
	 */
	protected static StringBuilder consumeUntil(@Nonnull final Reader reader, @Nullable final Characters untilCharacters, final char untilCharacter,
			final boolean isEndError, @Nullable final StringBuilder stringBuilder, final boolean includeReached) throws IOException, ParseEndException {
		int c; //the character read
		boolean reached;
		do {
			reader.mark(MINIMUM_MARK); //mark our current position
			c = reader.read(); //read another character
			if(c < 0) { //if this returned character represents the end of the reader's data
				if(isEndError) { //if requested make sure we're not at the end of the reader
					throw new ParseEndException(reader);
				} else {
					return stringBuilder; //there's no content to include, and there was no character to reset; in short, we're finished
				}
			}
			final char character = (char)c;
			reached = untilCharacters != null ? untilCharacters.contains(character) : c == untilCharacter; //see if we've reached the character
			if(!reached || includeReached) { //include everything up to the character---and maybe even the character
				if(stringBuilder != null) {
					stringBuilder.append(character); //add the read character to the string builder
				}
			}
		} while(!reached); //stop searching when we reach the character
		if(!includeReached) { //if we shouldn't included the reached character
			reader.reset(); //reset to the last mark, which was set right before the character we found
		}
		return stringBuilder;
	}

	/**
	 * Skips all characters in a reader until the given delimiter is passed or the end is reached. The new position will be immediately <em>after</em> that of the
	 * given character.
	 * @implSpec This implementation delegates to {@link #consumeUntil(Reader, char, boolean, StringBuilder, boolean)}.
	 * @param reader The reader the contents of which to be parsed.
	 * @param character The character to pass.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseEndException if the reader has no more characters.
	 */
	public static void pass(@Nonnull final Reader reader, final char character) throws IOException {
		consumeUntil(reader, character, false, null, true);
	}

	/**
	 * Skips all characters in a reader until one of the given characters or the end is reached. The new position will be immediately <em>after</em> that of the
	 * given character.
	 * @implSpec This implementation delegates to {@link #consumeUntil(Reader, Characters, boolean, StringBuilder, boolean)}.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters one of which to reach.
	 * @throws NullPointerException if the given reader and/or characters is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 */
	public static void pass(@Nonnull final Reader reader, final Characters characters) throws IOException {
		consumeUntil(reader, characters, false, null, true);
	}

	/**
	 * Skips all characters in a reader until the given delimiter is passed, throwing an exception if the end of the reader has been reached. The new position
	 * will be immediately <em>after</em> that of the given character.
	 * @implSpec This implementation delegates to {@link #consumeUntil(Reader, char, boolean, StringBuilder, boolean)}.
	 * @param reader The reader the contents of which to be parsed.
	 * @param character The character to pass.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseEndException if the reader has no more characters.
	 */
	public static void passRequired(@Nonnull final Reader reader, final char character) throws IOException, ParseEndException {
		consumeUntil(reader, character, true, null, true);
	}

	/**
	 * Skips all characters in a reader until one of the given characters is reached, throwing an exception if the end of the reader has been reached. The new
	 * position will be immediately <em>after</em> that of the given character.
	 * @implSpec This implementation delegates to {@link #consumeUntil(Reader, Characters, boolean, StringBuilder, boolean)}.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters one of which to reach.
	 * @throws NullPointerException if the given reader is and/or characters <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseEndException if the reader has no more characters.
	 */
	public static void passRequired(@Nonnull final Reader reader, @Nonnull final Characters characters) throws IOException, ParseEndException {
		consumeUntil(reader, characters, true, null, true);
	}

	/**
	 * Reads a character and, if a character was read (i.e. the reader is not out of data), resets the reader as if the character were not read.
	 * @param reader The reader the contents of which to be parsed.
	 * @return The next character that will be returned the reader's {@link Reader#read()} operation, or <code>-1</code> if the end of the reader has been
	 *         reached.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 */
	public static int peek(@Nonnull final Reader reader) throws IOException {
		reader.mark(MINIMUM_MARK); //mark our current position
		final int c = reader.read(); //get the current character
		if(c >= 0) { //if the reader is not out of data
			reader.reset(); //reset to the last mark, which was set right before the character we found
		}
		return c; //return the character read
	}

	/**
	 * Reads a character and and resets the reader as if the character were not read, throwing an exception if the end of the reader has been reached.
	 * @param reader The reader the contents of which to be parsed.
	 * @return The next character that will be returned the reader's {@link Reader#read()} operation.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseEndException if the reader has no more characters.
	 */
	public static char peekRequired(@Nonnull final Reader reader) throws IOException, ParseEndException {
		final int c = peek(reader); //peek a character
		checkReaderNotEnd(reader, c); //make sure we're not at the end of the reader
		return (char)c; //return the character peeked
	}

	/**
	 * Skips all characters in a reader until the given delimiter is passed or the end is reached. The new position will be that of the given character.
	 * @implSpec This implementation delegates to {@link #consumeUntil(Reader, char, boolean, StringBuilder, boolean)}.
	 * @param reader The reader the contents of which to be parsed.
	 * @param character The character to pass.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseEndException if the reader has no more characters.
	 */
	public static void reach(@Nonnull final Reader reader, final char character) throws IOException {
		consumeUntil(reader, character, false, null, false);
	}

	/**
	 * Skips all characters in a reader until one of the given characters or the end is reached. The new position will be that of the given character.
	 * @implSpec This implementation delegates to {@link #consumeUntil(Reader, Characters, boolean, StringBuilder, boolean)}.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters one of which to reach.
	 * @throws NullPointerException if the given reader and/or characters is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 */
	public static void reach(@Nonnull final Reader reader, @Nonnull final Characters characters) throws IOException {
		consumeUntil(reader, characters, false, null, false);
	}

	/**
	 * Skips all characters in a reader until the given delimiter is passed, throwing an exception if the end of the reader has been reached. The new position
	 * will be that of the given character.
	 * @implSpec This implementation delegates to {@link #consumeUntil(Reader, char, boolean, StringBuilder, boolean)}.
	 * @param reader The reader the contents of which to be parsed.
	 * @param character The character to pass.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseEndException if the reader has no more characters.
	 */
	public static void reachRequired(@Nonnull final Reader reader, final char character) throws IOException, ParseEndException {
		consumeUntil(reader, character, true, null, false);
	}

	/**
	 * Skips all characters in a reader until one of the given characters is reached, throwing an exception if the end of the reader has been reached. The new
	 * position will be that of the given character.
	 * @implSpec This implementation delegates to {@link #consumeUntil(Reader, Characters, boolean, StringBuilder, boolean)}.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters one of which to reach.
	 * @throws NullPointerException if the given reader and/or characters is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseEndException if the reader has no more characters.
	 */
	public static void reachRequired(@Nonnull final Reader reader, @Nonnull final Characters characters) throws IOException, ParseEndException {
		consumeUntil(reader, characters, true, null, false);
	}

	/**
	 * Reads text from a reader that has certain required beginning and ending delimiter characters. The delimiters will be discarded.
	 * @param reader The reader the contents of which to be parsed.
	 * @param startDelimiter The character to expect at the first of the characters.
	 * @param endDelimiter The character to expect at the end of the characters.
	 * @throws IOException Thrown when an I/O error occurs.
	 * @throws ParseUnexpectedDataException Thrown when an unexpected character is found.
	 * @throws ParseEndException Thrown when the end of the reader is reached unexpectedly.
	 * @return The characters between the delimiters.
	 */
	public static String readEnclosedRequired(final Reader reader, final char startDelimiter, final char endDelimiter)
			throws IOException, ParseUnexpectedDataException, ParseEndException {
		check(reader, startDelimiter); //read the first delimiter
		final String string = readUntilRequired(reader, endDelimiter); //read until the end delimiter is reached
		check(reader, endDelimiter); //read the ending delimiter
		return string; //return the string in-between
	}

	/**
	 * Reads a character, throwing an error if the end of the reader was reached.
	 * @apiNote This method is semantically equivalent to calling {@link #readRequiredCount(Reader, int)} with a value of <code>1</code>.
	 * @param reader The reader the contents of which to be parsed.
	 * @return The character returned from the reader's {@link Reader#read()} operation.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseEndException if the reader has no more characters.
	 */
	public static char readRequired(@Nonnull final Reader reader) throws IOException, ParseEndException {
		final int c = reader.read(); //read the next character
		checkReaderNotEnd(reader, c); //make sure we're not at the end of the reader
		return (char)c; //return the character read
	}

	/**
	 * Reads a given number of characters, throwing an error if the end of the reader was reached.
	 * @param reader The reader the contents of which to be parsed.
	 * @param count The number of characters to read.
	 * @return The string representing the characters returned from the reader's {@link Reader#read()} operation.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @throws IllegalArgumentException if the given count is less than zero.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseEndException if the reader has no more characters.
	 */
	public static String readRequiredCount(@Nonnull final Reader reader, final int count) throws IOException, ParseEndException {
		checkArgumentNotNegative(count); //make sure the count isn't negative
		final char[] characters = new char[count]; //create a new buffer
		if(reader.read(characters) != count) { //read the characters; if all the character weren't read
			throw new ParseEndException(reader);
		}
		return new String(characters); //return a new string from the characters read 
	}

	/**
	 * Reads a given number of characters, throwing an error if the end of the reader was reached or if a character in the string does not match one of the given
	 * characters.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters to accept.
	 * @param count The number of characters to read.
	 * @return The string representing the characters returned from the reader's {@link Reader#read()} operation.
	 * @throws NullPointerException if the given reader and/or characters is <code>null</code>.
	 * @throws IllegalArgumentException if the given count is less than zero.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseUnexpectedDataException if one of the characters read is not included in the given characters.
	 * @throws ParseEndException if the reader has no more characters.
	 */
	public static String readRequiredCount(@Nonnull final Reader reader, @Nonnull final Characters characters, final int count)
			throws IOException, ParseUnexpectedDataException {
		checkArgumentNotNegative(count); //make sure the count isn't negative
		final char[] buffer = new char[count]; //create a new buffer
		if(read(reader, buffer) != count) { //read the characters; if all the character weren't read
			throw new ParseEndException(reader);
		}
		for(int i = 0; i < count; ++i) { //look at each character
			final char c = buffer[i]; //look at this character
			if(!characters.contains(c)) { //if this character does not match one of the expected characters
				throw new ParseUnexpectedDataException(reader, characters, c);
			}
		}
		return new String(buffer); //return a new string from the characters read 
	}

	/**
	 * Reads at least a minimum number of characters in a reader that appear in a set of characters. The new position will either be the first character not in
	 * the range or the end of the reader.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters to accept.
	 * @param minimumCount The minimum number of characters to read.
	 * @return The characters that were read.
	 * @throws NullPointerException if the given reader and/or characters is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 */
	public static String readRequiredMinimumCount(@Nonnull final Reader reader, @Nonnull final Characters characters, int minimumCount) throws IOException {
		return readRequiredMinimumCount(reader, characters, minimumCount, new StringBuilder()).toString();
	}

	/**
	 * Reads at least a minimum number of characters in a reader that appear in a set of characters. The new position will either be the first character not in
	 * the range or the end of the reader.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters to accept.
	 * @param stringBuilder The string builder to which the characters will be appended.
	 * @param minimumCount The minimum number of characters to read.
	 * @return The given string builder with the characters appended.
	 * @throws NullPointerException if the given reader, characters, and/or string builder is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 */
	public static StringBuilder readRequiredMinimumCount(@Nonnull final Reader reader, @Nonnull final Characters characters, int minimumCount,
			@Nonnull final StringBuilder stringBuilder) throws IOException {
		final int originalLength = stringBuilder.length();
		readWhile(reader, characters, stringBuilder); //read all the characters we can
		if(stringBuilder.length() - originalLength < minimumCount) { //if we didn't read enough characters
			check(reader, characters); //attempt to read another character, which will throw the appropriate exception
		}
		return stringBuilder;
	}

	/**
	 * Reads all characters in a reader until one of the given characters or the end is reached. The reached character will be included and the new position will
	 * be immediately <em>after</em> that of the given character.
	 * @implSpec This implementation delegates to {@link #consumeUntil(Reader, Characters, boolean, StringBuilder, boolean)}.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters one of which to reach.
	 * @return The string read until the given character or the empty string if there is no such a character.
	 * @throws NullPointerException if the given reader and/or characters is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 */
	public static String readPast(@Nonnull final Reader reader, @Nonnull final Characters characters) throws IOException {
		return consumeUntil(reader, characters, false, new StringBuilder(), true).toString();
	}

	/**
	 * Reads all characters in a reader until one of the given characters. The reached character will be included and the new position will be immediately
	 * <em>after</em> that of the given character.
	 * @implSpec This implementation delegates to {@link #consumeUntil(Reader, Characters, boolean, StringBuilder, boolean)}.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters one of which to reach.
	 * @return The string read until the given character or the empty string if there is no such a character.
	 * @throws NullPointerException if the given reader and/or characters is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseEndException if the reader has no more characters.
	 */
	public static String readPastRequired(@Nonnull final Reader reader, @Nonnull final Characters characters) throws IOException {
		return consumeUntil(reader, characters, true, new StringBuilder(), true).toString();
	}

	/**
	 * Reads all characters in a reader until one of the given characters or the end is reached. The new position will be that of the given character.
	 * @implSpec This implementation delegates to {@link #consumeUntil(Reader, Characters, boolean, StringBuilder, boolean)}.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters one of which to reach.
	 * @return The string read until the given character or the empty string if there is no such a character.
	 * @throws NullPointerException if the given reader and/or characters is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 */
	public static String readUntil(@Nonnull final Reader reader, @Nonnull final Characters characters) throws IOException {
		return consumeUntil(reader, characters, false, new StringBuilder(), false).toString();
	}

	/**
	 * Reads all characters in a reader until the given delimiter is reached, throwing an exception if the end of the reader has been reached. The new position
	 * will be that of the given character.
	 * @implSpec This implementation delegates to {@link #consumeUntil(Reader, char, boolean, StringBuilder, boolean)}.
	 * @param reader The reader the contents of which to be parsed.
	 * @param character The character to pass.
	 * @return The string read until the given character.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseEndException if the reader has no more characters.
	 */
	public static String readUntilRequired(@Nonnull final Reader reader, final char character) throws IOException, ParseEndException {
		return consumeUntil(reader, character, true, new StringBuilder(), false).toString();
	}

	/**
	 * Reads all characters in a reader until one of the given characters is reached, throwing an exception if the end of the reader has been reached. The new
	 * position will be that of the given character.
	 * @implSpec This implementation delegates to {@link #consumeUntil(Reader, Characters, boolean, StringBuilder, boolean)}.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters one of which to reach.
	 * @return The string read until the given character.
	 * @throws NullPointerException if the given reader and/or characters is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseEndException if the reader has no more characters.
	 */
	public static String readUntilRequired(@Nonnull final Reader reader, @Nonnull final Characters characters) throws IOException, ParseEndException {
		return consumeUntil(reader, characters, true, new StringBuilder(), false).toString();
	}

	/**
	 * Reads all characters in a reader that appear within a given set of characters. The new position will either be the first character not in the characters or
	 * the end of the reader.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters to read.
	 * @return The characters that were read.
	 * @throws NullPointerException if the given reader and/or characters is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 */
	public static String readWhile(@Nonnull final Reader reader, @Nonnull final Characters characters) throws IOException {
		return readWhile(reader, characters, new StringBuilder()).toString();
	}

	/**
	 * Reads all characters in a reader that appear within a given set of characters and collects them in a given string builder. The new position will either be
	 * the first character not in the characters or the end of the reader.
	 * @implSpec This implementation delegates to {@link #consumeWhile(Reader, Characters, StringBuilder)}.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters to read.
	 * @param stringBuilder The string builder to collect the read characters.
	 * @return The given string builder with the characters that were read added.
	 * @throws NullPointerException if the given reader, characters, and/or string builder is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 */
	public static StringBuilder readWhile(@Nonnull final Reader reader, @Nonnull final Characters characters, @Nonnull final StringBuilder stringBuilder)
			throws IOException {
		consumeWhile(reader, characters, requireNonNull(stringBuilder)); //read the characters
		return stringBuilder;
	}

	/**
	 * Skips over characters in a reader that appear within a given set of characters. The new position will either be the first character not in the characters
	 * or the end of the reader.
	 * @implSpec This implementation delegates to {@link #consumeWhile(Reader, Characters, StringBuilder)}.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters to skip.
	 * @return The next character that will be returned the reader's {@link Reader#read()} operation, or <code>-1</code> if the end of the reader has been
	 *         reached.
	 * @throws NullPointerException if the given reader and/or characters is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 */
	public static int skip(@Nonnull final Reader reader, @Nonnull final Characters characters) throws IOException {
		return consumeWhile(reader, characters, null); //skip the characters without saving them
	}

}
