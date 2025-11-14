/*
 * Copyright © 1996-2013 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static com.globalmentor.java.Conditions.*;

import java.util.*;

/**
 * An unchecked illegal argument exception to indicate that an argument was not in the correct format or did not have the correct checksums.
 * @author Garret Wilson
 */
public class ArgumentSyntaxException extends IllegalArgumentException {

	private static final long serialVersionUID = 1L;

	/** The input, or <code>null</code> if the input is not known. */
	private final String input;

	/**
	 * @return The input, or <code>null</code> if the input is not known.
	 * @deprecated to be removed in favor of {@link #findInput()}.
	 */
	@Deprecated(forRemoval = true)
	public String getInput() {
		return input;
	}

	/**
	 * Returns the input if known.
	 * @return The input if known.
	 */
	public Optional<String> findInput() {
		return Optional.ofNullable(input);
	}

	/** The index into the input of the position at which the error occurred, or -1 if the position is not known. */
	private final int index;

	/**
	 * Returns the index into the input of the position at which the error occurred.
	 * @return The index into the input of the position at which the error occurred, or -1 if the position is not known.
	 * @deprecated to be removed in favor of {@link #findIndex()}.
	 */
	@Deprecated(forRemoval = true)
	public int getIndex() {
		return index;
	}

	/**
	 * Returns the index into the input of the position at which the error occurred.
	 * @return The index into the input of the position at which the error occurred if known.
	 */
	public OptionalInt findIndex() {
		return index == -1 ? OptionalInt.empty() : OptionalInt.of(index);
	}

	/**
	 * Message constructor.
	 * @param message An explanation of why the input could not be parsed, or <code>null</code> if a default message should be constructed.
	 */
	public ArgumentSyntaxException(final String message) {
		this(message, (Throwable)null); //construct the class with no cause
	}

	/**
	 * Message and cause constructor.
	 * @param message An explanation of why the input could not be parsed, or <code>null</code> if a default message should be constructed.
	 * @param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	 * @implSpec If no message is given, a default message will be constructed from the cause message if available, or a generic message otherwise.
	 */
	public ArgumentSyntaxException(final String message, final Throwable cause) {
		this(message, cause, null); //construct the class with no input
	}

	/**
	 * Message, cause, and input constructor.
	 * @param message An explanation of why the input could not be parsed, or <code>null</code> if a default message should be constructed.
	 * @param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	 * @param input The input, or <code>null</code> if the input is not known.
	 * @implSpec If no message is given, a default message will be constructed from the cause message if available, or a generic message including input details otherwise.
	 */
	public ArgumentSyntaxException(final String message, final Throwable cause, final CharSequence input) {
		this(message, cause, input, -1); //construct the class with an unknown input
	}

	/**
	 * input and index constructor.
	 * @param input The input, or <code>null</code> if the input is not known.
	 * @param index The index into the input of the position at which the parse error occurred, or -1 if the position is not known.
	 * @throws IllegalArgumentException if the given index is less than -1.
	 */
	public ArgumentSyntaxException(final CharSequence input, final int index) {
		this((String)null, input, index); //construct the class with no message
	}

	/**
	 * Message and input constructor.
	 * @param message An explanation of why the input could not be parsed, or <code>null</code> if a default message should be constructed.
	 * @param input The input, or <code>null</code> if the input is not known.
	 * @implSpec If no message is given, a default message will be constructed including input details.
	 */
	public ArgumentSyntaxException(final String message, final CharSequence input) {
		this(message, input, -1); //construct the class with an unknown index
	}

	/**
	 * Message, input, and index constructor.
	 * @param message An explanation of why the input could not be parsed, or <code>null</code> if a default message should be constructed.
	 * @param input The input, or <code>null</code> if the input is not known.
	 * @param index The index into the input of the position at which the parse error occurred, or -1 if the position is not known.
	 * @throws IllegalArgumentException if the given index is less than -1.
	 * @implSpec If no message is given, a default message will be constructed including input and index details.
	 */
	public ArgumentSyntaxException(final String message, final CharSequence input, final int index) {
		this(message, null, input, index); //construct the class with no cause
	}

	/**
	 * Cause constructor.
	 * @param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	 */
	public ArgumentSyntaxException(final Throwable cause) {
		this(cause, null); //construct the class with no input
	}

	/**
	 * Cause and input constructor.
	 * @param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	 * @param input The input, or <code>null</code> if the input is not known.
	 */
	public ArgumentSyntaxException(final Throwable cause, final CharSequence input) {
		this(cause, input, -1); //construct the class with an unknown index
	}

	/**
	 * Cause, input, and index constructor.
	 * @param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	 * @param input The input, or <code>null</code> if the input is not known.
	 * @param index The index into the input of the position at which the parse error occurred, or -1 if the position is not known.
	 * @throws IllegalArgumentException if the given index is less than -1.
	 */
	public ArgumentSyntaxException(final Throwable cause, final CharSequence input, final int index) {
		this(null, cause, input, index); //construct the class with no message
	}

	/**
	 * Message, cause, input, and index constructor.
	 * @param message An explanation of why the input could not be parsed, or <code>null</code> if a default message should be constructed.
	 * @param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	 * @param input The input, or <code>null</code> if the input is not known.
	 * @param index The index into the input of the position at which the parse error occurred, or -1 if the position is not known.
	 * @throws IllegalArgumentException if the given index is less than -1.
	 * @implSpec If no message is given, a default message will be constructed from the cause message if available, or a generic message including input and index details otherwise.
	 */
	public ArgumentSyntaxException(final String message, final Throwable cause, final CharSequence input, final int index) {
		super(createMessage(message, cause, input, index), cause); //construct the parent class with the message and the cause
		this.input = input != null ? input.toString() : null; //save the input, if any, as a string
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
	private static String createMessage(String message, final Throwable cause, final CharSequence input, final int index) {
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
		return message; //we found a message that we didn't have to construct, so return it
	}

}
