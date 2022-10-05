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

import static com.globalmentor.collections.Lists.*;
import static java.util.Objects.*;

import java.io.Reader;
import java.util.*;

import javax.annotation.*;

import com.globalmentor.java.Characters;

/**
 * Class for unexpected characters encountered when parsing an input stream. Used by {@link ParseReader}.
 * <p>
 * This implementation assumes that if a list of strings were expected, a string will have been found. That is, if {@link #getExpectedCharacters()} returns a
 * value {@link #getFoundCharacter()} will hold a valid value; otherwise, {@link #getFoundString()} will hold the appropriate value.
 * </p>
 * @see ParseIOException
 * @see ParseReader
 */
public class ParseUnexpectedDataException extends ParseIOException {

	private static final long serialVersionUID = 2L;

	/** The expected characters, if characters were expected, else <code>null</code>. */
	private Characters expectedCharacters = null;

	/** @return The expected characters, if characters were expected. */
	public Optional<Characters> getExpectedCharacters() {
		return Optional.ofNullable(expectedCharacters);
	}

	/**
	 * Sets the expected characters.
	 * @param expectedCharacters The expected characters.
	 */
	protected void setExpectedCharacters(@Nonnull final Characters expectedCharacters) {
		this.expectedCharacters = requireNonNull(expectedCharacters);
	}

	/** The character found, if characters were expected, else <code>0</code>. */
	private char foundCharacter = (char)0;

	/** @return The character found, if characters were expected, else <code>0</code>. */
	public char getFoundCharacter() {
		return foundCharacter;
	}

	/**
	 * Sets the character found.
	 * @param foundCharacter The character found.
	 */
	protected void setFoundCharacter(final char foundCharacter) {
		this.foundCharacter = foundCharacter;
	}

	/** The expected strings, if strings were expected, else <code>null</code>. */
	private List<String> expectedStrings = null;

	/** @return The expected strings, if strings were expected. */
	public Optional<List<String>> getExpectedStrings() {
		return Optional.ofNullable(expectedStrings);
	}

	/**
	 * Sets the expected strings.
	 * @param expectedStrings The expected strings.
	 */
	protected void setExpectedStrings(@Nonnull final List<String> expectedStrings) {
		this.expectedStrings = immutableListOf(expectedStrings);
	}

	/** The string found, if strings were expected, else <code>null</code>. */
	private String foundString = null;

	/** @return The string found, if strings were expected. */
	public Optional<String> getFoundString() {
		return Optional.ofNullable(foundString);
	}

	/**
	 * Sets the string found.
	 * @param foundString The string found.
	 */
	protected void setFoundString(@Nonnull final String foundString) {
		this.foundString = requireNonNull(foundString);
	}

	/**
	 * Constructor for an unexpected character error from a parse reader.
	 * @param reader The reader the data of which is the source of the error.
	 * @param foundChar The character found at this location.
	 */
	public ParseUnexpectedDataException(final Reader reader, final char foundChar) {
		this(reader, (String)null, foundChar);
	}

	/**
	 * Constructor for an unexpected character error from a parse reader.
	 * @param reader The reader the data of which is the source of the error.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 * @param foundChar The character found at this location.
	 */
	public ParseUnexpectedDataException(final Reader reader, final String message, final char foundChar) {
		this(message, foundChar, getLineIndex(reader), getCharacterIndex(reader)); //construct the class with values from the parse reader
	}

	/**
	 * Constructor for an unexpected character error.
	 * @param foundChar The character found at this location.
	 * @param lineIndex The index of the line in which the error occurred.
	 * @param charIndex The index of the character at which the error occurred on the current line.
	 */
	public ParseUnexpectedDataException(final char foundChar, final long lineIndex, final long charIndex) {
		this((String)null, foundChar, lineIndex, charIndex);
	}

	/**
	 * Constructor for an unexpected character error.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 * @param foundChar The character found at this location.
	 * @param lineIndex The index of the line in which the error occurred.
	 * @param charIndex The index of the character at which the error occurred on the current line.
	 */
	public ParseUnexpectedDataException(final String message, final char foundChar, final long lineIndex, final long charIndex) {
		super(message != null ? message : "Unexpected character: found " + Characters.getLabel(foundChar) + ".", (String)null, lineIndex, charIndex);
		setFoundCharacter(foundChar); //save the character found
	}

	/**
	 * Constructor for an unexpected character error from a parse reader, when one character was expected.
	 * @param reader The reader the data of which is the source of the error.
	 * @param expectedChar The character expected at this location.
	 * @param foundChar The character found at this location.
	 */
	public ParseUnexpectedDataException(final Reader reader, final char expectedChar, final char foundChar) {
		this(null, expectedChar, foundChar, getLineIndex(reader), getCharacterIndex(reader)); //construct the class with values from the parse reader
	}

	/**
	 * Constructor for an unexpected character error from a parse reader, when one character was expected.
	 * @param reader The reader the data of which is the source of the error.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 * @param expectedChar The character expected at this location.
	 * @param foundChar The character found at this location.
	 */
	public ParseUnexpectedDataException(final Reader reader, final String message, final char expectedChar, final char foundChar) {
		this(message, expectedChar, foundChar, getLineIndex(reader), getCharacterIndex(reader)); //construct the class with values from the parse reader
	}

	/**
	 * Constructor for an unexpected character error, when one character was expected.
	 * @param expectedChar The character expected at this location.
	 * @param foundChar The character found at this location.
	 * @param lineIndex The index of the line in which the error occurred.
	 * @param charIndex The index of the character at which the error occurred on the current line.
	 */
	public ParseUnexpectedDataException(final char expectedChar, final char foundChar, final long lineIndex, final long charIndex) {
		this(null, expectedChar, foundChar, lineIndex, charIndex);
	}

	/**
	 * Constructor for an unexpected character error, when one character was expected.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 * @param expectedChar The character expected at this location.
	 * @param foundChar The character found at this location.
	 * @param lineIndex The index of the line in which the error occurred.
	 * @param charIndex The index of the character at which the error occurred on the current line.
	 */
	public ParseUnexpectedDataException(final String message, final char expectedChar, final char foundChar, final long lineIndex, final long charIndex) {
		super(message != null ? message : "Unexpected character: expected " + Characters.getLabel(expectedChar) + " found " + Characters.getLabel(foundChar) + ".",
				(String)null, lineIndex, charIndex);
		setExpectedCharacters(Characters.of(expectedChar)); //save the expected character
		setFoundCharacter(foundChar); //save the character found

	}

	/**
	 * Constructor for an unexpected character error from a parse reader, when multiple characters were expected.
	 * @param reader The reader the data of which is the source of the error.
	 * @param expectedChars The characters expected at this location.
	 * @param foundChar The character found at this location.
	 */
	public ParseUnexpectedDataException(final Reader reader, final Characters expectedChars, final char foundChar) {
		this(expectedChars, foundChar, getLineIndex(reader), getCharacterIndex(reader)); //construct the class with values from the parse reader
	}

	/**
	 * Constructor for an unexpected character error, when multiple characters were expected.
	 * @param expectedChars The characters expected at this location.
	 * @param foundChar The character found at this location.
	 * @param lineIndex The index of the line in which the error occurred.
	 * @param charIndex The index of the character at which the error occurred on the current line.
	 */
	public ParseUnexpectedDataException(final Characters expectedChars, final char foundChar, final long lineIndex, final long charIndex) {
		super("Unexpected character: expected one of " + expectedChars.toLabelArrayString() + " found " + Characters.getLabel(foundChar) + ".", (String)null,
				lineIndex, charIndex);
		setExpectedCharacters(expectedChars); //save the expected characters
		setFoundCharacter(foundChar); //save the character found
	}

	/**
	 * Constructor for an unexpected character error, when multiple strings were expected.
	 * @param expectedStrings An array containing the strings expected at this location.
	 * @param foundString The string found at this location.
	 * @param lineIndex The index of the line in which the error occurred.
	 * @param charIndex The index of the character at which the error occurred on the current line.
	 * @param sourceName The name of the source of the data (perhaps a filename).
	 */
	public ParseUnexpectedDataException(final String[] expectedStrings, final String foundString, final long lineIndex, final long charIndex,
			final String sourceName) {
		super("Unexpected character: expected one of " + convertStringsToMessage(immutableListOf(expectedStrings)) + " found "
				+ convertStringsToMessage(immutableListOf(foundString)) + ".", sourceName, lineIndex, charIndex);
		setExpectedStrings(immutableListOf(expectedStrings)); //save the expected strings
		setFoundString(foundString); //save the string found
	}

	/**
	 * Returns a message with the expected data, either a list of characters or a list of strings.
	 * @return A message with the expected data.
	 */
	public String getExpectedMessage() {
		if(getExpectedCharacters().isPresent()) { //if we have expected characters
			return getExpectedCharacters().map(Characters::toLabelArrayString).get(); //return a string of our expected characters
		} else if(getExpectedStrings().isPresent()) //if we have expected strings
			return getExpectedStrings().map(ParseIOException::convertStringsToMessage).get(); //return a string of our expected strings
		else { //if we don't know what we were expecting
			return ""; //return a null string; this in theory should never happen
		}
	}

	/**
	 * Returns a message with the data found, either a single character or a string.
	 * @return A message with the data found.
	 */
	public String getFoundMessage() {
		if(getExpectedCharacters().isPresent()) { //if we were expecting characters
			return Characters.getLabel(getFoundCharacter()); //we will have found a character, so return it
		} else if(getExpectedStrings().isPresent()) { //if we were expecting strings
			return convertStringsToMessage(immutableListOf(getFoundString().get())); //we will have found a string, so return what we found
		} else
			//if we don't know what we were expecting
			return ""; //return a null string; this in theory should never happen
	}

}
