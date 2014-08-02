/*
 * Copyright © 1996-2009 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import com.globalmentor.java.Characters;

import static com.globalmentor.java.Conditions.*;

/**
 * Parsing methods that work on a {@link Reader}. The reader must support marking.
 * @author Garret Wilson
 * @see Reader#markSupported()
 */
public class ReaderParser
{

	/**
	 * Checks that reader has no more data.
	 * @param reader The reader the contents of which to be parsed.
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 * @exception ParseUnexpectedDataException if the reader is not at the end of data.
	 */
	public static void checkReaderEnd(final Reader reader) throws IOException, ParseUnexpectedDataException
	{
		final int c = reader.read(); //get the current character
		if(c >= 0) //if this character is valid (the reader is not out of data)
		{
			throw new ParseUnexpectedDataException("Expected end of data; found " + Characters.getLabel(c) + ".", (char)c, reader);
		}
	}

	/**
	 * Checks that a read character does not represent the end of the reader's data.
	 * @param reader The reader the contents of which to be parsed.
	 * @param c The character returned from a reader's {@link Reader#read()} operation.
	 * @exception ParseEOFException if the given character represents the end of the reader's data.
	 */
	public static void checkReaderNotEnd(final Reader reader, final int c) throws ParseEOFException
	{
		checkReaderNotEnd(reader, c, true);
	}

	/**
	 * Checks that a read character does not represent the end of the reader's data.
	 * @param reader The reader the contents of which to be parsed.
	 * @param c The character returned from a reader's {@link Reader#read()} operation.
	 * @param isException <code>true</code> if an error condition should throw an exception.
	 * @return <code>false</code> if the given character represents the end of the reader's data, else <code>true</code>.
	 * @exception ParseEOFException if the given character represents the end of the reader's data.
	 */
	public static boolean checkReaderNotEnd(final Reader reader, final int c, final boolean isException) throws ParseEOFException
	{
		if(c < 0) //if this returned character represents the end of the reader's data
		{
			if(isException)
			{
				throw new ParseEOFException(reader);
			}
			return false;
		}
		return true;
	}

	/**
	 * Checks that the current character matches a specific character and advances to the next character.
	 * @param reader The reader the contents of which to be parsed.
	 * @param character The character against which which the current character should be checked.
	 * @return The character returned the reader's {@link Reader#read()} operation.
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 * @exception ParseUnexpectedDataException if the current character in the reader does not match the specified character.
	 * @exception ParseEOFException if the reader has no more characters.
	 */
	public static char check(final Reader reader, final char character) throws IOException, ParseUnexpectedDataException
	{
		final int c = reader.read(); //get the current character
		if(c != character) //if this character does not match what we expected
		{
			checkReaderNotEnd(reader, c); //make sure we're not at the end of the reader
			throw new ParseUnexpectedDataException(character, (char)c, reader);
		}
		return (char)c; //return the character read
	}

	/**
	 * Checks that the current character matches a character in a range and advances to the next character.
	 * @param reader The reader the contents of which to be parsed.
	 * @param lowerBound The lowest character in the range.
	 * @param upperBound The highest character in the range.
	 * @return The character returned the reader's {@link Reader#read()} operation.
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 * @exception ParseUnexpectedDataException if the current character in the reader does not fall within the given range.
	 * @exception ParseEOFException if the reader has no more characters.
	 */
	public static char check(final Reader reader, final char lowerBound, final char upperBound) throws IOException, ParseUnexpectedDataException
	{
		final int c = reader.read(); //get the current character
		if(c < lowerBound || c > upperBound) //if this character is not in the range
		{
			checkReaderNotEnd(reader, c); //make sure we're not at the end of the reader
			throw new ParseUnexpectedDataException("Expected character from " + Characters.getLabel(lowerBound) + " to " + Characters.getLabel(upperBound)
					+ "; found " + Characters.getLabel(c) + ".", (char)c, reader);
		}
		return (char)c; //return the character read
	}

	/**
	 * Checks that the current characters matches a given set of characters and advances to the next character.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters to accept.
	 * @return The character returned the reader's {@link Reader#read()} operation.
	 * @exception NullPointerException if the given reader and/or the given characters is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 * @exception ParseUnexpectedDataException if the current character in the reader does not match one of the specified characters.
	 * @exception ParseEOFException if the reader has no more characters.
	 */
	public static char check(final Reader reader, final Characters characters) throws IOException, ParseUnexpectedDataException
	{
		final int c = reader.read(); //get the current character
		checkReaderNotEnd(reader, c); //make sure we're not at the end of the reader
		if(!characters.contains((char)c)) //if this character does not match one of the expected characters
		{
			throw new ParseUnexpectedDataException(characters, (char)c, reader);
		}
		return (char)c; //return the character read
	}

	/**
	 * Checks that the current and subsequent characters matches a specified character sequence.
	 * @param reader The reader the contents of which to be parsed.
	 * @param match The character sequence with which the current characters should be checked.
	 * @return The character sequence that was checked.
	 * @exception NullPointerException if the given reader and/or match character sequence is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 * @exception ParseUnexpectedDataException if the current character in the reader does not match the specified character sequence.
	 * @exception ParseEOFException if the reader has no more characters.
	 */
	public static CharSequence check(final Reader reader, final CharSequence match) throws IOException, ParseUnexpectedDataException
	{
		final int matchLength = match.length(); //get the length to match
		for(int i = 0; i < matchLength; ++i) //for each match index
		{
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
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 */
	public static boolean confirm(final Reader reader, final char character) throws IOException
	{
		reader.mark(1); //mark our current position
		final int c = reader.read(); //get the current character
		if(c >= 0) //if the end of the reader was not reached
		{
			if(c == character) //if the expected character was read
			{
				return true; //indicate that the character was the one expected
			}
			else
			//if the character was not the one expected
			{
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
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 */
	public static boolean confirm(final Reader reader, final Characters characters) throws IOException
	{
		reader.mark(1); //mark our current position
		final int c = reader.read(); //get the current character
		if(c >= 0) //if the end of the reader was not reached
		{
			if(characters.contains((char)c)) //if one of the expected characters was read
			{
				return true; //indicate that the character was the one expected
			}
			else
			//if the character was not the one expected
			{
				reader.reset(); //reset to the last mark, which was set right before the character we found
			}
		}
		return false; //indicate that another character was encountered or the end of the reader was reached
	}

	/**
	 * Reads a character and, if a character does not match a character in the given range, resets the reader as if the character were not read.
	 * @param reader The reader the contents of which to be parsed.
	 * @param lowerBound The lowest character in the range.
	 * @param upperBound The highest character in the range.
	 * @return <code>true</code> if a character in the given range was read, or <code>false</code> if the next character is not one of the expected characters and
	 *         was therefore replaced if the end of the reader was not reached.
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 */
	public static boolean confirm(final Reader reader, final char lowerBound, final char upperBound) throws IOException
	{
		reader.mark(1); //mark our current position
		final int c = reader.read(); //get the current character
		if(c >= 0) //if the end of the reader was not reached
		{
			if(c >= lowerBound && c <= upperBound) //if this character is in the range
			{
				return true; //indicate that the character was the one expected
			}
			else
			//if the character was not the one expected
			{
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
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 */
	public static boolean confirm(final Reader reader, final CharSequence charSequence) throws IOException
	{
		final int length = charSequence.length();
		final char[] buffer = new char[length]; //create a buffer for reading all the characters more efficiently than one at a time
		reader.mark(length); //mark our current position
		if(reader.read(buffer) < length) //read the characters; if there weren't enough characters left before the end
		{
			reader.reset(); //reset to the last mark, right before we read the string
			return false;
		}
		for(int i = 0; i < length; ++i) //check all the characters
		{
			if(buffer[i] != charSequence.charAt(i)) //if a character doesn't match
			{
				reader.reset(); //reset to the last mark, right before we read the string
				return false;
			}
		}
		return true;
	}

	/**
	 * Reads a character, throwing an error if the end of the reader was reached.
	 * @param reader The reader the contents of which to be parsed.
	 * @return The character returned from the reader's {@link Reader#read()} operation.
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 * @exception ParseEOFException if the reader has no more characters.
	 */
	public static char readCharacter(final Reader reader) throws IOException, ParseEOFException
	{
		final int c = reader.read(); //read the next character
		checkReaderNotEnd(reader, c); //make sure we're not at the end of the reader
		return (char)c; //return the character read
	}

	/**
	 * Reads a given number of characters, throwing an error if the end of the reader was reached.
	 * @param reader The reader the contents of which to be parsed.
	 * @param count The number of characters to read.
	 * @return The string representing the characters returned from the reader's {@link Reader#read()} operation.
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IllegalArgumentException if the given count is less than zero.
	 * @exception IOException if there is an error reading from the reader.
	 * @exception ParseEOFException if the reader has no more characters.
	 */
	public static String readString(final Reader reader, final int count) throws IOException, ParseEOFException
	{
		checkArgumentNotNegative(count); //make sure the count isn't negative
		final char[] characters = new char[count]; //create a new buffer
		if(reader.read(characters) != count) //read the characters; if all the character weren't read
		{
			throw new ParseEOFException(reader);
		}
		return new String(characters); //return a new string from the characters read 
	}

	/**
	 * Reads a given number of characters, throwing an error if the end of the reader was reached or if a character in the string does not match a character in
	 * the given range.
	 * @param reader The reader the contents of which to be parsed.
	 * @param count The number of characters to read.
	 * @param lowerBound The lowest character in the range.
	 * @param upperBound The highest character in the range.
	 * @return The string representing the characters returned from the reader's {@link Reader#read()} operation.
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IllegalArgumentException if the given count is less than zero.
	 * @exception IOException if there is an error reading from the reader.
	 * @exception ParseUnexpectedDataException if a character in the string does not fall within the given range.
	 * @exception ParseEOFException if the reader has no more characters.
	 */
	public static String readStringCheck(final Reader reader, final int count, final char lowerBound, final char upperBound) throws IOException,
			ParseUnexpectedDataException
	{
		checkArgumentNotNegative(count); //make sure the count isn't negative
		final char[] characters = new char[count]; //create a new buffer
		if(reader.read(characters) != count) //read the characters; if all the character weren't read
		{
			throw new ParseEOFException(reader);
		}
		for(int i = 0; i < count; ++i) //look at each character
		{
			final char c = characters[i]; //look at this character
			if(c < lowerBound || c > upperBound) //if this character is not in the range
			{
				throw new ParseUnexpectedDataException("Expected character from " + Characters.getLabel(lowerBound) + " to " + Characters.getLabel(upperBound)
						+ "; found " + Characters.getLabel(c) + ".", (char)c, reader);
			}
		}
		return new String(characters); //return a new string from the characters read 
	}

	/**
	 * Skips all characters in a reader until the given delimiter is passed. The new position will be immediately after that of the given character.
	 * @param reader The reader the contents of which to be parsed.
	 * @param character The character to pass.
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 * @exception ParseEOFException if the reader has no more characters.
	 */
	public static void pass(final Reader reader, final char character) throws IOException, ParseEOFException
	{
		int c; //the character read
		while((c = reader.read()) != character) //keep reading until we find the character
		{
			checkReaderNotEnd(reader, c); //make sure we're not at the end of the reader
		}
	}

	/**
	 * Reads a character and and resets the reader as if the character were not read, throwing an exception if the end of the reader has been reached.
	 * @param reader The reader the contents of which to be parsed.
	 * @return The next character that will be returned the reader's {@link Reader#read()} operation.
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 * @exception ParseEOFException if the reader has no more characters.
	 */
	public static char peek(final Reader reader) throws IOException, ParseEOFException
	{
		final int c = peekEnd(reader); //peek a character
		checkReaderNotEnd(reader, c); //make sure we're not at the end of the reader
		return (char)c; //return the character peeked
	}

	/**
	 * Reads a character and, if a character was read (i.e. the reader is not out of data), resets the reader as if the character were not read.
	 * @param reader The reader the contents of which to be parsed.
	 * @return The next character that will be returned the reader's {@link Reader#read()} operation, or <code>-1</code> if the end of the reader has been
	 *         reached.
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 */
	public static int peekEnd(final Reader reader) throws IOException
	{
		reader.mark(1); //mark our current position
		final int c = reader.read(); //get the current character
		if(c >= 0) //if the reader is not out of data
		{
			reader.reset(); //reset to the last mark, which was set right before the character we found
		}
		return c; //return the character read
	}

	/**
	 * Reads a character and, if a character was read (i.e. the reader is not out of data), resets the reader as if the character were not readm returning whether
	 * the character matches a character in the given range.
	 * @param reader The reader the contents of which to be parsed.
	 * @param lowerBound The lowest character in the range.
	 * @param upperBound The highest character in the range.
	 * @return <code>true</code> if a characters in the given range was peeked.
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 */
	public static boolean isPeek(final Reader reader, final char lowerBound, final char upperBound) throws IOException
	{
		final char c = peek(reader);
		return c >= lowerBound && c <= upperBound; //see if this character is in the range
	}

	/**
	 * Reads all characters in a reader until the given character is reached. The new position will be that of the given character.
	 * @param reader The reader the contents of which to be parsed.
	 * @param character The character to reach.
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 * @exception ParseEOFException if the reader has no more characters.
	 */
	public static String reach(final Reader reader, final char character) throws IOException, ParseEOFException
	{
		final StringBuilder stringBuilder = new StringBuilder(); //the string builder to collect read characters
		int c; //the character read
		while(true)
		{
			reader.mark(1); //mark our current position
			c = reader.read(); //read another character
			checkReaderNotEnd(reader, c); //make sure we're not at the end of the reader
			if(c == character) //if we've reached the character
			{
				break; //stop searching for the character
			}
			else
			//if we haven't reached the match character
			{
				stringBuilder.append((char)c); //add the read character to the string builder
			}
		}
		reader.reset(); //reset to the last mark, which was set right before the character we found
		return stringBuilder.toString(); //return the string we read
	}

	/**
	 * Reads all characters in a reader until one of the given characters is reached. The new position will be that of the given character.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters one of which to reach.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseEOFException if the reader has no more characters.
	 */
	public static String reach(final Reader reader, final Characters characters) throws IOException, ParseEOFException
	{
		return reach(reader, characters, true);
	}

	/**
	 * Reads all characters in a reader until one of the given characters or the end is reached. The new position will be that of the given character.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters one of which to reach.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 */
	public static String reachEnd(final Reader reader, final Characters characters) throws IOException
	{
		return reach(reader, characters, false);
	}

	/**
	 * Reads all characters in a reader until one of the given characters is reached. The new position will be that of the given character.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters one of which to reach.
	 * @param isEndError Whether reaching the end of the reader is an error condition.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @throws IOException if there is an error reading from the reader.
	 * @throws ParseEOFException if the reader has no more characters and the end-is-error flag is <code>true</code>.
	 */
	protected static String reach(final Reader reader, final Characters characters, final boolean isEndError) throws IOException, ParseEOFException
	{
		final StringBuilder stringBuilder = new StringBuilder(); //the string builder to collect read characters
		int c; //the character read
		while(true)
		{
			reader.mark(1); //mark our current position
			c = reader.read(); //read another character
			if(!checkReaderNotEnd(reader, c, isEndError)) //make sure we're not at the end of the reader
			{
				break;
			}
			final char character = (char)c;
			if(characters.contains(character)) //if we've reached the character
			{
				break; //stop searching for the character
			}
			else
			//if we haven't reached the match character
			{
				stringBuilder.append(character); //add the read character to the string builder
			}
		}
		reader.reset(); //reset to the last mark, which was set right before the character we found
		return stringBuilder.toString(); //return the string we read
	}

	/**
	 * Reads all characters in a reader until the given delimiter is reached. The new position will be immediately after that of the given character.
	 * @param reader The reader the contents of which to be parsed.
	 * @param character The character to reach.
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 * @exception ParseEOFException if the reader has no more characters.
	 */
	public static String reachAfter(final Reader reader, final char character) throws IOException, ParseEOFException
	{
		final StringBuilder stringBuilder = new StringBuilder(); //the string builder to collect read characters
		int c; //the character read
		while(true)
		{
			c = reader.read(); //read another character
			checkReaderNotEnd(reader, c); //make sure we're not at the end of the reader
			if(c == character) //if we've reached the character
			{
				break; //stop searching for the character
			}
			else
			//if we haven't reached the match character
			{
				stringBuilder.append((char)c); //add the read character to the string builder
			}
		}
		return stringBuilder.toString(); //return the string we read
	}

	/**
	 * Reads all characters in a reader that lie within a given range. The new position will either be the first character not in the range or the end of the
	 * reader.
	 * @param reader The reader the contents of which to be parsed.
	 * @param lowerBound The lowest character in the range.
	 * @param upperBound The highest character in the range.
	 * @return The characters that were read.
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 */
	public static String read(final Reader reader, final char lowerBound, final char upperBound) throws IOException
	{
		final StringBuilder stringBuilder = new StringBuilder(); //create a string builder
		skip(reader, lowerBound, upperBound, stringBuilder); //read the characters
		return stringBuilder.toString(); //return the collected characters
	}

	/**
	 * Reads text from a reader that has certain beginning and ending delimiter characters. The delimiters will be discarded.
	 * @param reader The reader the contents of which to be parsed.
	 * @param startDelimiter The character to expect at the first of the characters.
	 * @param endDelimiter The character to expect at the end of the characters.
	 * @throws IOException Thrown when an I/O error occurs.
	 * @throws ParseUnexpectedDataException Thrown when an unexpected character is found.
	 * @throws ParseEOFException Thrown when the end of the reader is reached unexpectedly.
	 * @return The characters between the delimiters.
	 */
	public static String readDelimited(final Reader reader, final char startDelimiter, final char endDelimiter) throws IOException, ParseUnexpectedDataException,
			ParseEOFException
	{
		check(reader, startDelimiter); //read the first delimiter
		final String string = reachAfter(reader, endDelimiter); //read until the end delimiter is reached
		return string; //return the string in-between
	}

	/**
	 * Reads at least a minimum number of characters in a reader that lie within a given range. The new position will either be the first character not in the
	 * range or the end of the reader.
	 * @param reader The reader the contents of which to be parsed.
	 * @param minimumCount The minimum number of characters to read.
	 * @param lowerBound The lowest character in the range.
	 * @param upperBound The highest character in the range.
	 * @return The characters that were read.
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 */
	public static String readMinimum(final Reader reader, int minimumCount, final char lowerBound, final char upperBound) throws IOException
	{
		final StringBuilder stringBuilder = new StringBuilder(); //create a string builder
		while(minimumCount > 0) //while we should read a minimum count of characters
		{
			stringBuilder.append(check(reader, lowerBound, upperBound)); //check another character and append it to the string builder
			--minimumCount; //note that we have more minimum characters to read
		}
		skip(reader, lowerBound, upperBound, stringBuilder); //read the remaining characters
		return stringBuilder.toString(); //return the collected characters
	}

	/**
	 * Reads all characters in a reader that appear within a given array. The new position will either be the first character not in the array or the end of the
	 * reader.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters to read.
	 * @return The characters that were read.
	 * @exception NullPointerException if the given reader and/or the given characters is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 */
	public static String read(final Reader reader, final Characters characters) throws IOException
	{
		final StringBuilder stringBuilder = new StringBuilder(); //create a string builder
		skip(reader, characters, stringBuilder); //read the characters
		return stringBuilder.toString(); //return the collected characters
	}

	/**
	 * Skips over characters in a reader that lie within a given range. The new position will either be the first character not in the range or the end of the
	 * reader.
	 * @param reader The reader the contents of which to be parsed.
	 * @param lowerBound The lowest character in the range.
	 * @param upperBound The highest character in the range.
	 * @return The next character that will be returned the reader's {@link Reader#read()} operation, or <code>-1</code> if the end of the reader has been
	 *         reached.
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 */
	public static int skip(final Reader reader, final char lowerBound, final char upperBound) throws IOException
	{
		return skip(reader, lowerBound, upperBound, null); //skip the characters without saving them
	}

	/**
	 * Skips over characters in a reader that lie within a given range and optionally collects those characters in a string builder. The new position will either
	 * be the first character not in the range or the end of the reader.
	 * @param reader The reader the contents of which to be parsed.
	 * @param lowerBound The lowest character in the range.
	 * @param upperBound The highest character in the range.
	 * @param stringBuilder The string builder to collect the characters skipped, or <code>null</code> if the skipped characters should be discarded.
	 * @return The next character that will be returned the reader's {@link Reader#read()} operation, or <code>-1</code> if the end of the reader has been
	 *         reached.
	 * @exception NullPointerException if the given reader is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 */
	protected static int skip(final Reader reader, final char lowerBound, final char upperBound, final StringBuilder stringBuilder) throws IOException
	{
		int c; //the character read
		boolean skip; //we'll note when we should skip
		do
		{
			skip = false; //start out assuming we shouldn't skip this character
			reader.mark(1); //mark our current position
			c = reader.read(); //read another character
			//Log.trace("just read for skipping:", c);
			if(c < 0) //if we're at the end of the reader
			{
				return c; //stop skipping and return without resetting the reader to the mark
			}
			//Log.trace("trying to skip character", (char)c);
			if(c >= lowerBound && c <= upperBound) //if the character is within the range of characters, we'll skip it
			{
				if(stringBuilder != null) //if a string builder was given
				{
					stringBuilder.append((char)c); //save the character to be sent back
				}
				skip = true; //indicate that we should skip this character
			}
		}
		while(skip); //keep reading characters until we find one we shouldn't skip
		reader.reset(); //reset to the last mark, which was set right before the character we found
		return c; //return the next character to be read
	}

	/**
	 * Skips over characters in a reader that appear within a given array. The new position will either be the first character not in the array or the end of the
	 * reader.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters to skip.
	 * @return The next character that will be returned the reader's {@link Reader#read()} operation, or <code>-1</code> if the end of the reader has been
	 *         reached.
	 * @exception NullPointerException if the given reader and/or the given characters is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 */
	public static int skip(final Reader reader, final Characters characters) throws IOException
	{
		return skip(reader, characters, null); //skip the characters without saving them
	}

	/**
	 * Skips over characters in a reader that appear within a given array and optionally collects those characters in a string builder. The new position will
	 * either be the first character not in the array or the end of the reader.
	 * @param reader The reader the contents of which to be parsed.
	 * @param characters The characters to skip.
	 * @param stringBuilder The string builder to collect the characters skipped, or <code>null</code> if the skipped characters should be discarded.
	 * @return The next character that will be returned the reader's {@link Reader#read()} operation, or <code>-1</code> if the end of the reader has been
	 *         reached.
	 * @exception NullPointerException if the given reader and/or the given characters is <code>null</code>.
	 * @exception IOException if there is an error reading from the reader.
	 */
	protected static int skip(final Reader reader, final Characters characters, final StringBuilder stringBuilder) throws IOException
	{
		int c; //the character read
		boolean skip; //we'll note when we should skip
		do
		{
			skip = false; //start out assuming we shouldn't skip this character
			reader.mark(1); //mark our current position
			c = reader.read(); //read another character
			//Log.trace("just read for skipping:", c);
			if(c < 0) //if we're at the end of the reader
			{
				return c; //stop skipping and return without resetting the reader to the mark
			}
			//Log.trace("trying to skip character", (char)c);
			if(characters.contains((char)c))
			{
				if(stringBuilder != null) //if a string builder was given
				{
					stringBuilder.append((char)c); //save the character to be sent back
				}
				skip = true; //indicate that we should skip this character
			}
		}
		while(skip); //keep reading characters until we find one we shouldn't skip
		reader.reset(); //reset to the last mark, which was set right before the character we found
		return c; //return the next character to be read
	}

}
