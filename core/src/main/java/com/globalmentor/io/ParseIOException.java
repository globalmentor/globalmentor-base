/*
 * Copyright © 1996-2017 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.io.*;
import java.util.List;

import javax.annotation.Nonnull;

/**
 * Exception class for parsing errors that occur during I/O.
 * @see ParseReader
 */
public class ParseIOException extends IOException {

	private static final long serialVersionUID = 1L;

	/** The index of the line on which the error occurred, or -1 if the line index is not known. */
	private final long lineIndex;

	/**
	 * @return The index of the line on which the error occurred, or -1 if the line index is not known..
	 * @see #getCharIndex()
	 */
	public long getLineIndex() {
		return lineIndex;
	}

	/**
	 * The index of the character at which the error occurred on the current line, or -1 if the character index is not known..
	 * @see #lineIndex
	 */
	private final long charIndex;

	/**
	 * @return The index of the character at which the error occurred on the current line, or -1 if the character index is not known..
	 * @see #getLineIndex()
	 */
	public long getCharIndex() {
		return charIndex;
	}

	/** The name of the source of this exception, such as a filename, or <code>null</code> if not known. */
	private String sourceName = "";

	/** @return The name of the source of this exception, such as a filename, or <code>null</code> if not known. */
	public String getSourceName() {
		return sourceName;
	}

	/**
	 * Message constructor with no cause.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 */
	public ParseIOException(final String message) {
		this(message, (Throwable)null, -1, -1); //construct the class with no cause or location
	}

	/**
	 * Cause constructor.
	 * @param cause The cause of the error, or <code>null</code> if there is no cause.
	 */
	public ParseIOException(final Throwable cause) {
		this(null, cause, -1, -1); //construct the class with no given message or location
	}

	/**
	 * Message and cause constructor.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 * @param cause The cause of the error, or <code>null</code> if there is no cause.
	 */
	public ParseIOException(final String message, final Throwable cause) {
		this(message, cause, -1, -1); //construct the class with no location known
	}

	/**
	 * Message, cause, and source constructor.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 * @param cause The cause of the error, or <code>null</code> if there is no cause.
	 * @param sourceName The name of the source of the data (perhaps a filename), or <code>null</code> if not known.
	 */
	public ParseIOException(final String message, final Throwable cause, final String sourceName) {
		this(message, cause, sourceName, -1, -1); //construct the class with no location known
	}

	/**
	 * Message, source, and location constructor with no cause.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 * @param sourceName The name of the source of the data (perhaps a filename), or <code>null</code> if not known.
	 * @param lineIndex The index of the line in which the error occurred, or -1 if not known.
	 * @param charIndex The index of the character at which the error occurred on the current line, or -1 if not known.
	 */
	public ParseIOException(final String message, final String sourceName, final long lineIndex, final long charIndex) {
		this(message, null, sourceName, lineIndex, charIndex); //construct the class with no cause
	}

	/**
	 * Cause, source, and location constructor.
	 * @param cause The cause of the error, or <code>null</code> if there is no cause.
	 * @param sourceName The name of the source of the data (perhaps a filename), or <code>null</code> if not known.
	 * @param lineIndex The index of the line in which the error occurred, or -1 if not known.
	 * @param charIndex The index of the character at which the error occurred on the current line, or -1 if not known.
	 */
	public ParseIOException(final Throwable cause, final String sourceName, final long lineIndex, final long charIndex) {
		this(null, cause, sourceName, lineIndex, charIndex); //construct the class with no given message
	}

	/**
	 * Reader and message constructor with no cause. This constructor will attempt to determine the location from the reader.
	 * @param reader The reader from which the error occurred.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @see LineNumberReader
	 * @see ParseReader
	 */
	public ParseIOException(final Reader reader, final String message) {
		this(reader, message, (Throwable)null); //construct the class with no cause
	}

	/**
	 * Reader, and cause constructor. This constructor will attempt to determine the location from the reader.
	 * @param reader The reader from which the error occurred.
	 * @param cause The cause of the error, or <code>null</code> if there is no cause.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @see LineNumberReader
	 * @see ParseReader
	 */
	public ParseIOException(final Reader reader, final Throwable cause) {
		this(reader, null, cause); //construct the class with no given message		
	}

	/**
	 * Reader, message, and cause constructor. This constructor will attempt to determine the location from the reader.
	 * @param reader The reader from which the error occurred.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 * @param cause The cause of the error, or <code>null</code> if there is no cause.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @see LineNumberReader
	 * @see ParseReader
	 */
	public ParseIOException(final Reader reader, final String message, final Throwable cause) {
		this(message, cause, getLineIndex(reader), getCharacterIndex(reader)); //construct the class after attempting to get the line and character indexes from the reader
	}

	/**
	 * Message and location constructor with no cause.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 * @param lineIndex The index of the line in which the error occurred, or -1 if not known.
	 * @param charIndex The index of the character at which the error occurred on the current line, or -1 if not known.
	 */
	public ParseIOException(final String message, final long lineIndex, final long charIndex) {
		this(message, (Throwable)null, lineIndex, charIndex); //construct the class with no cause
	}

	/**
	 * Cause and location constructor.
	 * @param cause The cause of the error, or <code>null</code> if there is no cause.
	 * @param lineIndex The index of the line in which the error occurred, or -1 if not known.
	 * @param charIndex The index of the character at which the error occurred on the current line, or -1 if not known.
	 */
	public ParseIOException(final Throwable cause, final long lineIndex, final long charIndex) {
		this(null, cause, lineIndex, charIndex); //construct the class with no given message
	}

	/**
	 * Message, cause, and location constructor.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 * @param cause The cause of the error, or <code>null</code> if there is no cause.
	 * @param lineIndex The index of the line in which the error occurred, or -1 if not known.
	 * @param charIndex The index of the character at which the error occurred on the current line, or -1 if not known.
	 */
	public ParseIOException(final String message, final Throwable cause, final long lineIndex, final long charIndex) {
		this(message, cause, null, lineIndex, charIndex); //construct the class with no source
	}

	/**
	 * Reader, message, and source constructor with no cause. This constructor will attempt to determine the location from the reader.
	 * @param reader The reader from which the error occurred.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 * @param sourceName The name of the source of the data (perhaps a filename), or <code>null</code> if not known.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @see LineNumberReader
	 * @see ParseReader
	 */
	public ParseIOException(final Reader reader, final String message, final String sourceName) {
		this(reader, message, null, sourceName); //construct the class with no cause
	}

	/**
	 * Reader, cause, and source constructor. This constructor will attempt to determine the location from the reader.
	 * @param reader The reader from which the error occurred.
	 * @param cause The cause of the error, or <code>null</code> if there is no cause.
	 * @param sourceName The name of the source of the data (perhaps a filename), or <code>null</code> if not known.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @see LineNumberReader
	 * @see ParseReader
	 */
	public ParseIOException(final Reader reader, final Throwable cause, final String sourceName) {
		this(reader, null, cause, sourceName); //construct the class with no given message		
	}

	/**
	 * Reader, message, cause, and source constructor. This constructor will attempt to determine the location from the reader.
	 * @param reader The reader from which the error occurred.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 * @param cause The cause of the error, or <code>null</code> if there is no cause.
	 * @param sourceName The name of the source of the data (perhaps a filename), or <code>null</code> if not known.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @see LineNumberReader
	 * @see ParseReader
	 */
	public ParseIOException(final Reader reader, final String message, final Throwable cause, final String sourceName) {
		this(message, cause, sourceName, getLineIndex(reader), getCharacterIndex(reader)); //construct the class after attempting to get the line and character indexes from the reader
	}

	/**
	 * Message and source constructor with no cause.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 * @param sourceName The name of the source of the data (perhaps a filename), or <code>null</code> if not known.
	 */
	public ParseIOException(final String message, final String sourceName) {
		this(message, null, sourceName, -1, -1); //construct the class with no cause or location
	}

	/**
	 * Cause and source constructor.
	 * @param cause The cause of the error, or <code>null</code> if there is no cause.
	 * @param sourceName The name of the source of the data (perhaps a filename), or <code>null</code> if not known.
	 */
	public ParseIOException(final Throwable cause, final String sourceName) {
		this(null, cause, sourceName, -1, -1); //construct the class with no given message or location
	}

	/**
	 * Message, cause, source, and location constructor.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 * @param cause The cause of the error, or <code>null</code> if there is no cause.
	 * @param sourceName The name of the source of the data (perhaps a filename), or <code>null</code> if not known.
	 * @param lineIndex The index of the line in which the error occurred, or -1 if not known.
	 * @param charIndex The index of the character at which the error occurred on the current line, or -1 if not known.
	 */
	public ParseIOException(final String message, final Throwable cause, final String sourceName, final long lineIndex, final long charIndex) {
		super(createMessage(message, cause, sourceName, lineIndex, charIndex), cause); //construct the parent class
		this.lineIndex = lineIndex;
		this.charIndex = charIndex;
		this.sourceName = sourceName;
	}

	/**
	 * Creates a message for the exception, determining arguments from the given reader if possible.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 * @param reader The reader that is the source of the message.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @return A constructed message.
	 * @see #getLineIndex(Reader)
	 * @see #getCharacterIndex(Reader)
	 */
	protected static String createMessage(final String message, final Reader reader) {
		return createMessage(message, null, reader);
	}

	/**
	 * Creates a message for the exception, determining arguments from the given reader if possible.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 * @param cause The cause of the error, or <code>null</code> if there is no cause.
	 * @param reader The reader that is the source of the message.
	 * @return A constructed message.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @see #getLineIndex(Reader)
	 * @see #getCharacterIndex(Reader)
	 */
	protected static String createMessage(final String message, final Throwable cause, final Reader reader) {
		return createMessage(message, cause, null, getLineIndex(reader), getCharacterIndex(reader));
	}

	/**
	 * Creates a message for the exception.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 * @param cause The cause of the error, or <code>null</code> if there is no cause.
	 * @param sourceName The name of the source of the data (perhaps a filename), or <code>null</code> if not known.
	 * @param lineIndex The index of the line in which the error occurred, or -1 if not known.
	 * @param charIndex The index of the character at which the error occurred on the current line, or -1 if not known.
	 * @return A constructed message.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 */
	protected static String createMessage(String message, final Throwable cause, final String sourceName, final long lineIndex, final long charIndex) {
		if(message == null && cause != null) { //if there is no message, but there is a cause
			message = cause.getMessage(); //use the cause message
		}
		final StringBuilder stringBuilder = new StringBuilder(); //create a string builder
		if(message != null) { //if there is a message
			stringBuilder.append(message); //add the message
		}
		if(sourceName != null || lineIndex >= 0 || charIndex >= 0) { //if there is a source name or a location known
			if(stringBuilder.length() > 0) { //if there are already characters in the string builder
				stringBuilder.append(' '); //add a separator
			}
			stringBuilder.append('('); //(
			if(sourceName != null) { //if there is a source name
				stringBuilder.append(sourceName); //append the source name
			}
			if(lineIndex >= 0) { //if there is a line index
				if(sourceName != null && !sourceName.isEmpty()) { //if there is a source name
					stringBuilder.append(' '); //add a separator
				}
				stringBuilder.append(lineIndex + 1); //append the line number
				if(charIndex >= 0) { //if there is a character index
					stringBuilder.append(':').append(charIndex + 1); //append the character number after a delimiter
				}
			}
			stringBuilder.append(')'); //)
		}
		return stringBuilder.length() > 0 ? stringBuilder.toString() : null; //if there are no characters, return null
	}

	/**
	 * Determines the line index of the given reader if possible.
	 * @param reader The reader from which the line index should be determined.
	 * @return The line index, or <code>null</code> if the line index could not be determined.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @see LineNumberReader
	 * @see ParseReader
	 */
	protected static long getLineIndex(final Reader reader) {
		if(reader instanceof LineNumberReader) { //if this is a line number reader
			return ((LineNumberReader)reader).getLineNumber(); //the line number is really the line index
		} else if(reader instanceof ParseReader) { //if this is a parse reader
			return ((ParseReader)reader).getLineIndex(); //it knows the line index
		} else { //if we don't recognize the reader type
			return -1; //we can't find a line index
		}
	}

	/**
	 * Determines the character index of the given reader if possible.
	 * @param reader The reader from which the character index should be determined.
	 * @return The character index, or <code>null</code> if the character index could not be determined.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @see ParseReader
	 */
	protected static long getCharacterIndex(final Reader reader) {
		if(reader instanceof ParseReader) { //if this is a parse reader
			return ((ParseReader)reader).getCharIndex(); //it knows the character index
		} else { //if we don't recognize the reader type
			return -1; //we can't find a character index
		}
	}

	/**
	 * Converts an array of strings to a message with the strings separated by commas.
	 * @param strings An array of strings to be converted to a string.
	 * @return The message string constructed
	 */
	//TODO convert the characters in these strings so that whitespace gets converted to characters
	public static String convertStringsToMessage(@Nonnull final List<String> strings) {
		final StringBuilder messageStringBuilder = new StringBuilder(); //this will receive the message to return
		for(int i = 0; i < strings.size(); ++i) { //look at each string
			messageStringBuilder.append('"').append(strings.get(i)); //add a double quote character followed by this string
			if(i < strings.size() - 1) { //if this isn't the last string
				messageStringBuilder.append("\", "); //show that there will be another string
			} else { //if this is the last string
				messageStringBuilder.append('"'); //add just a double quote
			}
		}
		return messageStringBuilder.toString(); //return the message string we constructed
	}

}
