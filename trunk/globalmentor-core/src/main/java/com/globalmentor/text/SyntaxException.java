/*
 * Copyright © 1996-2013 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.text;

import static com.globalmentor.java.Conditions.*;

import com.globalmentor.util.DataException;

/**
 * Generic checked exception thrown to indicate that input contains a syntax error.
 * @author Garret Wilson
 */
public class SyntaxException extends DataException {

	/** The input, or <code>null</code> if the input is not known. */
	private final String input;

	/** @return The input, or <code>null</code> if the input is not known. */
	public String getInput() {
		return input;
	}

	/** The index into the input of the position at which the error occurred, or -1 if the position is not known. */
	private final int index;

	/** @return The index into the input of the position at which the error occurred, or -1 if the position is not known. */
	public int getIndex() {
		return index;
	}

	/**
	 * Message constructor.
	 * @param message An explanation of why the input could not be parsed, or <code>null</code> if a default message should be used.
	 */
	public SyntaxException(final String message) {
		this(message, (Throwable)null); //construct the class with no cause
	}

	/**
	 * Message, cause, input, and index constructor. A message will be constructed including the given message, if any, or the given message of the cause, if any.
	 * @param message An explanation of why the input could not be parsed, or <code>null</code> if a default message should be used.
	 * @param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	 */
	public SyntaxException(final String message, final Throwable cause) {
		this(message, cause, null); //construct the class with no input
	}

	/**
	 * Message, cause, and input constructor. A message will be constructed including the given message, if any, or the given message of the cause, if any.
	 * @param message An explanation of why the input could not be parsed, or <code>null</code> if a default message should be used.
	 * @param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	 * @param input The input, or <code>null</code> if the input is not known.
	 */
	public SyntaxException(final String message, final Throwable cause, final String input) {
		this(message, cause, input, -1); //construct the class with an unknown input
	}

	/**
	 * input and index constructor.
	 * @param input The input, or <code>null</code> if the input is not known.
	 * @param index The index into the input of the position at which the parse error occurred, or -1 if the position is not known.
	 * @throws IllegalArgumentException if the given index is less than -1.
	 */
	public SyntaxException(final String input, final int index) {
		this((String)null, input, index); //construct the class with no message
	}

	/**
	 * Message and input constructor.
	 * @param message An explanation of why the input could not be parsed, or <code>null</code> if a default message should be used.
	 * @param input The input, or <code>null</code> if the input is not known.
	 */
	public SyntaxException(final String message, final String input) {
		this(message, input, -1); //construct the class with an unknown index
	}

	/**
	 * Message, input, and index constructor.
	 * @param message An explanation of why the input could not be parsed, or <code>null</code> if a default message should be used.
	 * @param input The input, or <code>null</code> if the input is not known.
	 * @param index The index into the input of the position at which the parse error occurred, or -1 if the position is not known.
	 * @throws IllegalArgumentException if the given index is less than -1.
	 */
	public SyntaxException(final String message, final String input, final int index) {
		this(message, null, input, index); //construct the class with no cause
	}

	/**
	 * Cause constructor.
	 * @param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	 */
	public SyntaxException(final Throwable cause) {
		this(cause, null); //construct the class with no input
	}

	/**
	 * Cause and input constructor.
	 * @param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	 * @param input The input, or <code>null</code> if the input is not known.
	 */
	public SyntaxException(final Throwable cause, final String input) {
		this(cause, input, -1); //construct the class with an unknown index
	}

	/**
	 * Cause, input, and index constructor.
	 * @param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	 * @param input The input, or <code>null</code> if the input is not known.
	 * @param index The index into the input of the position at which the parse error occurred, or -1 if the position is not known.
	 * @throws IllegalArgumentException if the given index is less than -1.
	 */
	public SyntaxException(final Throwable cause, final String input, final int index) {
		this(null, cause, input, index); //construct the class with no message
	}

	/**
	 * Message, cause, input, and index constructor. A message will be constructed including the given message, if any, or the given message of the cause, if any.
	 * @param message An explanation of why the input could not be parsed, or <code>null</code> if a default message should be used.
	 * @param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	 * @param input The input, or <code>null</code> if the input is not known.
	 * @param index The index into the input of the position at which the parse error occurred, or -1 if the position is not known.
	 * @throws IllegalArgumentException if the given index is less than -1.
	 */
	public SyntaxException(final String message, final Throwable cause, final String input, final int index) {
		super(createMessage(message, cause, input, index), cause); //construct the parent class with the message and the cause
		this.input = input; //save the input, if any
		this.index = index; //save the index		
	}

	/**
	 * Creates a message based upon a given input string, an optional index, and an optional message.
	 * @param message The message to include, or <code>null</code> if there is no custom message to include.
	 * @param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	 * @param input The input character sequence, or <code>null</code> if the input string is not known.
	 * @param index The index into the input string of the position at which the parse error occurred, or -1 if the position is not known.
	 * @return A string explaining the exception based upon the given input and optional index.
	 * @throws IllegalArgumentException if the given index is less than -1.
	 */
	public static String createMessage(String message, final Throwable cause, final CharSequence input, final int index) {
		checkArgumentMinimum(index, -1); //make sure the index is not less than negative one, the "unknown index" value
		if(message == null) { //if there is no message
			message = cause != null ? cause.getMessage() : null; //try to get the message from the cause
			if(message == null) { //if there is still no message
				message = "Syntax exception"; //use a general message
				final StringBuilder stringBuilder = new StringBuilder(message); //create a string builder
				if(index >= 0) { //if there is an index
					stringBuilder.append("at index ").append(Integer.valueOf(index)).append(' '); //add the index
				}
				if(input != null) { //if there is input
					stringBuilder.append("in input ").append(input); //append the input			
				}
				return stringBuilder.toString(); //return the constructed message
			}
		}
		return message; //we found a message that we didn't have to consruct, so return it
	}

}
