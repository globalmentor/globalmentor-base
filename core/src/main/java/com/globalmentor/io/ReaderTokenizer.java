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

package com.globalmentor.io;

import java.io.Reader;
import java.io.IOException;
import java.util.*;

import com.globalmentor.java.Characters;

import static com.globalmentor.java.Characters.*;

/**
 * Tokenizes input from a reader, recognizing groups. All text within a group will be ignored when delimiting tokens, except that group delimiters are checked
 * for matching. If a group has the same beginning and ending character, direct nesting of that group is not allowed, although other groups may be interspersed.
 * This class does not return independent iterators.
 * @author Garret Wilson
 */
public class ReaderTokenizer implements Iterator<String>, Iterable<String> {

	/**
	 * The default delimiter characters: whitespace.
	 * @see Characters#TRIM_CHARACTERS
	 */
	protected static final Characters DEFAULT_DELIMITERS = TRIM_CHARACTERS;

	/** The default beginning group characters: "([{". */
	protected static final String DEFAULT_GROUP_BEGINS = "([{";

	/** The default ending group characters: ")]}". */
	protected static final String DEFAULT_GROUP_ENDS = ")]}";

	/** The source of the input characters. */
	private final Reader reader;

	/**
	 * Returns the source of the input characters.
	 * @return The source of the input characters.
	 */
	protected Reader getReader() {
		return reader;
	}

	/** The characters delimiting tokens. */
	private Characters delimiters;

	/**
	 * Returns the characters delimiting tokens.
	 * @return The characters delimiting tokens.
	 */
	public Characters getDelimiters() {
		return delimiters;
	}

	/** The valid group beginning characters. */
	private final String groupBegins;

	/**
	 * Returns the valid group beginning characters.
	 * @return The valid group beginning characters.
	 */
	public String getGroupBegins() {
		return groupBegins;
	}

	/** The valid group ending characters. */
	private final String groupEnds;

	/**
	 * Returns the valid group ending characters, matching to beginning characters.
	 * @return The valid group ending characters, matching to beginning characters.
	 */
	public String getGroupEnds() {
		return groupEnds;
	}

	/** We'll use the string builder as a stack to keep track of our group depth. */
	private final StringBuilder groupStackStringBuilder = new StringBuilder();

	/**
	 * Pushes a group onto the stack.
	 * @param groupBegin The character representing the start of the group.
	 */
	protected void pushGroup(final char groupBegin) {
		groupStackStringBuilder.append(groupBegin); //append the group beginning character
	}

	/**
	 * Pops a group from the stack and returns the group beginning character.
	 * @return The character used to begin the group.
	 * @throws EmptyStackException if there are no groups left.
	 */
	protected char popGroup() throws EmptyStackException {
		final int groupDepth = getGroupDepth(); //see how many groups there are
		if(groupDepth > 0) { //if we have groups
			final char groupBegin = groupStackStringBuilder.charAt(groupDepth - 1); //get the group beginning character
			groupStackStringBuilder.deleteCharAt(groupDepth - 1); //delete the group beginning character
			return groupBegin; //return the group beginning character
		} else { //if we are out of groups
			throw new EmptyStackException(); //show that there are no more groups
		}
	}

	/**
	 * Returns the character of the group currently on the stack.
	 * @return The character used to begin the group.
	 * @throws EmptyStackException if there are no groups left.
	 */
	protected char peekGroup() throws EmptyStackException {
		final int groupDepth = getGroupDepth(); //see how many groups there are
		if(groupDepth > 0) { //if we have groups
			return groupStackStringBuilder.charAt(groupDepth - 1); //get the group beginning character
		} else { //if we are out of groups
			throw new EmptyStackException(); //show that there are no more groups
		}
	}

	/**
	 * Returns the number of nested groups currently being processed.
	 * @return The number of nested groups currently being processed.
	 */
	protected int getGroupDepth() {
		return groupStackStringBuilder.length();
	}

	/**
	 * The next primed token, or <code>null</code> if there is no next token or the token has not been primed.
	 */
	private String primedToken = null;

	//	/**
	//	 * @return The next primed token, or <code>null</code> if there is no next token or the token has not been primed.
	//	 */
	//TODO del		protected String getPrimedToken() {return nextToken;}

	/**
	 * Reader constructor with default token delimiters and no group delimiters.
	 * @param reader The input characters to tokenize.
	 * @see #DEFAULT_DELIMITERS
	 */
	public ReaderTokenizer(final Reader reader) {
		this(reader, DEFAULT_DELIMITERS);
	}

	/**
	 * Token delimiter constructor with no group delimiters.
	 * @param reader The input characters to tokenize.
	 * @param delimiters The delimiter characters.
	 */
	public ReaderTokenizer(final Reader reader, final Characters delimiters) {
		this(reader, delimiters, null, null); //construct the tokenizer with no group recognition
	}

	/**
	 * Delimiter and group constructor.
	 * @param reader The input characters to tokenize.
	 * @param delimiters The delimiter characters.
	 * @param groupBegins The valid group beginning characters.
	 * @param groupEnds The valid group ending characters, matching to beginning characters.
	 */
	public ReaderTokenizer(final Reader reader, final Characters delimiters, final String groupBegins, final String groupEnds) {
		this.reader = reader;
		this.delimiters = delimiters;
		this.groupBegins = groupBegins != null ? groupBegins : "";
		this.groupEnds = groupEnds != null ? groupEnds : "";
	}

	/** @return <code>true</code> if there are more tokens. */
	public boolean hasNext() {
		return primeToken() != null; //prime a token and see if there is one available
	}

	/**
	 * @return The next token available.
	 * @throws NoSuchElementException if there are no more tokens.
	 */
	public String next() {
		final String token = primeToken(); //prime the next token
		if(token != null) { //if there is a token available
			primedToken = null; //show that we've used the primed token
			return token; //return the token
		} else { //if there is no token available
			throw new NoSuchElementException(); //show that there are no more tokens
		}
	}

	/**
	 * Removes the last token from the underlying collection. This method is not supported, and an <code>UnsupportedOperationException</code> is always thrown.
	 * @throws UnsupportedOperationException if the the remove operation is not supported .
	 * @throws IllegalStateException if the <code>next()</code> method has not yet been called, or the <code>remove</code> method has already been called after
	 *           the last call to the <code>next</code> method.
	 */
	public void remove() {
		throw new UnsupportedOperationException();
	}

	/** The last delimiter encountered, which introduced the current token. */
	private char lastDelimiter = UNDEFINED_CHAR;

	/**
	 * Returns the last delimiter encountered, which introduced the current token. If no last delimiter has been encountered, {@link Characters#UNDEFINED_CHAR} is
	 * returned.
	 * @return The last delimiter encountered.
	 * @see #getDelimiters()
	 */
	public char getLastDelimiter() {
		return lastDelimiter;
	}

	/** The current delimiter encountered, which delimits the current token. */
	private char delimiter = UNDEFINED_CHAR;

	/**
	 * Returns the current delimiter encountered, which delimits the current token. If no delimiters have been encountered, or the end of the reader was reached,
	 * {@link Characters#UNDEFINED_CHAR} is returned.
	 * @return The current delimiter encountered.
	 * @see #getDelimiters()
	 */
	public char getDelimiter() {
		return delimiter;
	}

	/**
	 * If there is no token primed, attempts to retrieve the next token.
	 * @return Either the token that was already primed or, if there is no token already, the token newly primed by this method. If no token is available, this
	 *         method returns <code>null</code>.
	 */
	public String primeToken() {
		if(primedToken == null) { //if there is no token primed
			final Reader reader = getReader(); //get a reference to our reader
			final Characters delimiters = getDelimiters(); //get our delimiters
			final String groupBegins = getGroupBegins(); //get our group beginning delimiters
			final String groupEnds = getGroupEnds(); //get our group ending delimiters
			final StringBuilder stringBuilder = new StringBuilder(); //create a string builder to build our token
			try {
				while(true) {
					final int value = reader.read(); //read the next character
					if(value < 0) { //if there are no more characters
						this.lastDelimiter = this.delimiter; //move the delimiter to the last delimiter
						this.delimiter = UNDEFINED_CHAR; //indicate that we did not encounter a delimiter; we ran out of characters
						break; //stop processing characters
					}
					final char character = (char)value; //cast the character value to a char
					if(getGroupDepth() == 0 && delimiters.contains(character)) { //only look at delimiters when we're not in a group
						this.lastDelimiter = this.delimiter; //move the delimiter to the last delimiter
						this.delimiter = character; //save the delimiter							
						//TODO improve; does this not allow empty tokens? there should at least be an option for this
						if(stringBuilder.length() > 0) { //if we've already started a token, return the token; otherwise, just skip the delimiter
							break; //we found a delimiter, so we've finished a token; this will shift delimiters
						}
					} else { //if this is not a delimiter, or we're inside the group, we'll keep the character
						final int groupBeginCharIndex = groupBegins.indexOf(character); //see if this is a group beginning character
						if(groupBeginCharIndex >= 0) { //if this is a group beginning
							if(groupEnds.charAt(groupBeginCharIndex) == character //if the beginning and ending character is the same for this group...
									&& getGroupDepth() > 0 //...and if we're inside a group...
									&& peekGroup() == character) { //...and the group we're in has the same character as we just found, assume we're closing the group
								try {
									final char groupBegin = popGroup(); //pop a group from the stack
								} catch(EmptyStackException emptyStackException) { //this should never happen, as we've already checked the group depth, and we even know that the delimiters match
									throw new AssertionError(emptyStackException);
								}
							} else { //if this group has different beginning or ending characters, or we're inside another group, assume we really are starting a new group
								pushGroup(character); //push the group onto the stack
							}
						} else { //if this is not a group beginning
							final int groupEndCharIndex = groupEnds.indexOf(character); //see if this is a group ending character
							if(groupEndCharIndex >= 0) { //if this is a group ending
								try {
									final char groupBegin = popGroup(); //pop a group from the stack
									//TODO check to make sure groupBegins[groupEndCharIndex]==groupBegin and do something interesting
								} catch(final EmptyStackException emptyStackException) {
									throw new AssertionError(emptyStackException);
								}
							}
						}
						stringBuilder.append(character); //append the character
					}
				}
			} catch(final IOException ioException) {
				throw new AssertionError(ioException);
			}
			primedToken = stringBuilder.length() > 0 ? stringBuilder.toString() : null; //store the token, or null if we didn't collect any characters
		}
		return primedToken; //return the waiting token
	}

	/** @return An iterator over the tokens. */
	public Iterator<String> iterator() {
		return this;
	}

}
