package com.garretwilson.io;

import java.io.*;
import java.util.Arrays;

import static com.garretwilson.util.ArrayUtilities.*;

import com.garretwilson.util.Debug;

/**Parsing methods that work on a {@link Reader}.
The reader must support marking.
@author Garret Wilson
@see Reader#markSupported()
*/
public class ReaderParser
{

	/**Checks that a read character does not represent the end of the reader's data.
	@param c The character returned from a reader's {@link Reader#read()} operation.
	@exception ParseIOException if the given character represents the end of the reader's data.
	*/
	public static void checkReaderEnd(final int c) throws ParseIOException
	{
		if(c<0)	//if this returned character represents the end of the reader's data
		{
			throw new ParseIOException("End of data.");
		}
	}

	/**Checks that the current character matches a specific character and advances to the next character.
	@param reader The reader the contents of which to be parsed.
	@param character The character against which which the current character should be checked.
	@return The character returned the reader's {@link Reader#read()} operation.
	@exception NullPointerException if the given reader is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	@exception ParseIOException if the current character in the reader does not match the specified character or if the reader has no more characters.
	*/
	public static char check(final Reader reader, final char character) throws IOException, ParseIOException
	{
		final int c=reader.read();	//get the current character
		if(c!=character)	//if this character does not match what we expected
		{
			checkReaderEnd(c);	//make sure we're not at the end of the reader
			throw new ParseIOException("Expected "+(char)character+"; found "+(char)c+".");
		}
		return (char)c;	//return the character read
	}

	/**Checks that the current character matches a character in a range and advances to the next character.
	@param reader The reader the contents of which to be parsed.
	@param lowerBound The lowest character in the range.
	@param upperBound The highest character in the range.	
	@return The character returned the reader's {@link Reader#read()} operation.
	@exception NullPointerException if the given reader is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	@exception ParseIOException if the current character in the reader does not fall within the given range or if the reader has no more characters.
	*/
	public static char check(final Reader reader, final char lowerBound, final char upperBound) throws IOException, ParseIOException
	{
		final int c=reader.read();	//get the current character
		if(c<lowerBound || c>upperBound)	//if this character is not in the range
		{
			checkReaderEnd(c);	//make sure we're not at the end of the reader
			throw new ParseIOException("Expected character from "+(char)lowerBound+" to "+(char)upperBound+"; found "+(char)c+".");
		}
		return (char)c;	//return the character read
	}
	
	/**Checks that the current characters matches a given set of characters and advances to the next character.
	@param reader The reader the contents of which to be parsed.
	@param characters The characters to accept.
	@return The character returned the reader's {@link Reader#read()} operation.
	@exception NullPointerException if the given reader and/or the given characters is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	@exception ParseIOException if the current character in the reader does not match one of the specified characters or if the reader has no more characters.
	*/
	public static char check(final Reader reader, final char[] characters) throws IOException, ParseIOException
	{
		final int c=reader.read();	//get the current character
		if(indexOf(characters, c)<0)	//if this character does not match one of the expected characters
		{
			checkReaderEnd(c);	//make sure we're not at the end of the reader
			throw new ParseIOException("Expected one of "+Arrays.toString(characters)+"; found "+(char)c+".");
		}
		return (char)c;	//return the character read
	}

	/**Checks that the current and subsequent characters matches a specified character sequence.
	@param reader The reader the contents of which to be parsed.
	@param match The character sequence with which the current characters should be checked.
	@exception NullPointerException if the given reader and/or match character sequence is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	@exception ParseIOException if the current character in the reader does not match the specified character sequence or if the reader has no more characters.
	*/
	public static void check(final Reader reader, final CharSequence match) throws IOException, ParseIOException
	{
		final int matchLength=match.length();	//get the length to match
		for(int i=0; i<matchLength; ++i)	//for each match index
		{
			check(reader, match.charAt(i));	//compare the current character with the match character
		}
	}

	/**Reads a character, throwing an error if the end of the reader was reached.
	@param reader The reader the contents of which to be parsed.
	@return The character be returned the reader's {@link Reader#read()} operation.
	@exception NullPointerException if the given reader is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	@exception ParseIOException if the reader has no more characters.
	*/
	public static char readCharacter(final Reader reader) throws IOException, ParseIOException
	{
		final int c=reader.read();	//read the next character
		checkReaderEnd(c);	//make sure we're not at the end of the reader
		return (char)c;	//return the character read
	}

	/**Reads a character and, if a character was read (i.e. the reader is not out of data), resets the reader as if the character were not read.
	@param reader The reader the contents of which to be parsed.
	@return The next character that will be returned the reader's {@link Reader#read()} operation, or <code>-1</code> if the end of the reader has been reached.
	@exception NullPointerException if the given reader is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	*/
	public static int peek(final Reader reader) throws IOException, ParseIOException
	{
//Debug.trace("ready to peek");
		reader.mark(1);	//mark our current position
		final int c=reader.read();	//get the current character
//Debug.trace("peek-read char", c);
		if(c>=0)	//if the reader is not out of data
		{
			reader.reset();	//reset to the last mark, which was set right before the character we found
		}
		return c;	//return the character read
	}

	/**Reads all characters in a reader until the given character is reached.
	The new position will be that of the given character.
	@param reader The reader the contents of which to be parsed.
	@param character The character to reach.
	@exception NullPointerException if the given reader is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	@exception ParseIOException if the reader has no more characters.
	*/
	public static String reach(final Reader reader, final char character) throws IOException
	{
		final StringBuilder stringBuilder=new StringBuilder();	//the string builder to collect read characters
		int c;	//the character read
		while(true)
		{
			reader.mark(1);	//mark our current position
			c=reader.read();	//read another character
			checkReaderEnd(c);	//make sure we're not at the end of the reader
			if(c==character)	//if we've reached the character
			{
				break;	//stop searching for the character
			}
			else	//if we haven't reached the match character
			{
				stringBuilder.append((char)c);	//add the read character to the string builder
			}
		}
		reader.reset();	//reset to the last mark, which was set right before the character we found
		return stringBuilder.toString();	//return the string we read
	}

	/**Reads all characters in a reader until the given delimiter is reached.
	The new position will be immediately after that of the given character.
	@param reader The reader the contents of which to be parsed.
	@param character The character to reach.
	@exception NullPointerException if the given reader is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	@exception ParseIOException if the reader has no more characters.
	*/
	public static String reachAfter(final Reader reader, final char character) throws IOException
	{
		final StringBuilder stringBuilder=new StringBuilder();	//the string builder to collect read characters
		int c;	//the character read
		while(true)
		{
			c=reader.read();	//read another character
			checkReaderEnd(c);	//make sure we're not at the end of the reader
			if(c==character)	//if we've reached the character
			{
				break;	//stop searching for the character
			}
			else	//if we haven't reached the match character
			{
				stringBuilder.append((char)c);	//add the read character to the string builder
			}
		}
		return stringBuilder.toString();	//return the string we read
	}
	
	/**Reads all characters in a reader that lie within a given range.
	The new position will either be the first character not in the range or the end of the reader.
	@param reader The reader the contents of which to be parsed.
	@param lowerBound The lowest character in the range.
	@param upperBound The highest character in the range.	
	@return The characters that were read.
	@exception NullPointerException if the given reader is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	*/
	public static String read(final Reader reader, final char lowerBound, final char upperBound) throws IOException
	{
		final StringBuilder stringBuilder=new StringBuilder();	//create a string builder
		skip(reader, lowerBound, upperBound, stringBuilder);	//read the characters
		return stringBuilder.toString();	//return the collected characters
	}
	
	/**Reads all characters in a reader that appear within a given array.
	The new position will either be the first character not in the array or the end of the reader.
	@param reader The reader the contents of which to be parsed.
	@param characters The characters to read.
	@return The characters that were read.
	@exception NullPointerException if the given reader and/or the given characters is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	*/
	public static String read(final Reader reader, final char[] characters) throws IOException
	{
		final StringBuilder stringBuilder=new StringBuilder();	//create a string builder
		skip(reader, characters, stringBuilder);	//read the characters
		return stringBuilder.toString();	//return the collected characters
	}
	
	/**Skips over characters in a reader that lie within a given range.
	The new position will either be the first character not in the range or the end of the reader.
	@param reader The reader the contents of which to be parsed.
	@param lowerBound The lowest character in the range.
	@param upperBound The highest character in the range.	
	@return The next character that will be returned the reader's {@link Reader#read()} operation, or <code>-1</code> if the end of the reader has been reached.
	@exception NullPointerException if the given reader is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	*/
	public static int skip(final Reader reader, final char lowerBound, final char upperBound) throws IOException
	{
		return skip(reader, lowerBound, upperBound, null);	//skip the characters without saving them
	}
	
	/**Skips over characters in a reader that lie within a given range and optionally collects those characters in a string builder.
	The new position will either be the first character not in the range or the end of the reader.
	@param reader The reader the contents of which to be parsed.
	@param lowerBound The lowest character in the range.
	@param upperBound The highest character in the range.	
	@param stringBuilder The string builder to collect the characters skipped, or <code>null</code> if the skipped characters should be discarded.
	@return The next character that will be returned the reader's {@link Reader#read()} operation, or <code>-1</code> if the end of the reader has been reached.
	@exception NullPointerException if the given reader is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	*/
	protected static int skip(final Reader reader, final char lowerBound, final char upperBound, final StringBuilder stringBuilder) throws IOException
	{
		int c;	//the character read
		boolean skip;	//we'll note when we should skip
		do
		{
			skip=false;	//start out assuming we shouldn't skip this character
			reader.mark(1);	//mark our current position
			c=reader.read();	//read another character
//Debug.trace("just read for skipping:", c);
			if(c<0)	//if we're at the end of the reader
			{
				return c;	//stop skipping and return without resetting the reader to the mark
			}
//Debug.trace("trying to skip character", (char)c);
			if(c>=lowerBound && c<upperBound)	//if the character is within the range of characters, we'll skip it
			{
				if(stringBuilder!=null)	//if a string builder was given
				{
					stringBuilder.append(c);	//save the character to be sent back
				}
				skip=true;	//indicate that we should skip this character
			}
		}
		while(skip);	//keep reading characters until we find one we shouldn't skip
		reader.reset();	//reset to the last mark, which was set right before the character we found
		return c;	//return the next character to be read
	}

	/**Skips over characters in a reader that appear within a given array.
	The new position will either be the first character not in the array or the end of the reader.
	@param reader The reader the contents of which to be parsed.
	@param characters The characters to skip.
	@return The next character that will be returned the reader's {@link Reader#read()} operation, or <code>-1</code> if the end of the reader has been reached.
	@exception NullPointerException if the given reader and/or the given characters is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	*/
	public static int skip(final Reader reader, final char[] characters) throws IOException
	{
		return skip(reader, characters, null);	//skip the characters without saving them
	}

	/**Skips over characters in a reader that appear within a given array and optionally collects those characters in a string builder.
	The new position will either be the first character not in the array or the end of the reader.
	@param reader The reader the contents of which to be parsed.
	@param characters The characters to skip.
	@param stringBuilder The string builder to collect the characters skipped, or <code>null</code> if the skipped characters should be discarded.
	@return The next character that will be returned the reader's {@link Reader#read()} operation, or <code>-1</code> if the end of the reader has been reached.
	@exception NullPointerException if the given reader and/or the given characters is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	*/
	protected static int skip(final Reader reader, final char[] characters, final StringBuilder stringBuilder) throws IOException
	{
		char lowerBound=Character.MAX_VALUE;	//we'll determine the lower bound of the range
		char upperBound=0;	//we'll determine the lower bound of the range
		for(int i=characters.length-1; i>=0; --i)	//look at each characters to skip
		{
			final char c=characters[i];	//get this character
			if(c<lowerBound)	//if this is a lower character than the one we already have for the lower bound
			{
				lowerBound=c;	//update the lower bound
			}
			if(c>upperBound)	//if this is a higher character than the one we already have for the upper bound
			{
				upperBound=c;	//update the upper bound
			}
		}
		int c;	//the character read
		boolean skip;	//we'll note when we should skip
		do
		{
			skip=false;	//start out assuming we shouldn't skip this character
			reader.mark(1);	//mark our current position
			c=reader.read();	//read another character
//Debug.trace("just read for skipping:", c);
			if(c<0)	//if we're at the end of the reader
			{
				return c;	//stop skipping and return without resetting the reader to the mark
			}
//Debug.trace("trying to skip character", (char)c);
			if(c>=lowerBound && c<upperBound)	//if the character is within the range of characters, make sure it's one of the characters
			{
				for(int i=characters.length-1; i>=0 && !skip; --i)	//look at each characters to skip
				{
					if(c==characters[i])	//if we found a character to skip
					{
						if(stringBuilder!=null)	//if a string builder was given
						{
							stringBuilder.append(c);	//save the character to be sent back
						}
						skip=true;	//indicate that we should skip this character
					}
				}
			}
		}
		while(skip);	//keep reading characters until we find one we shouldn't skip
		reader.reset();	//reset to the last mark, which was set right before the character we found
		return c;	//return the next character to be read
	}

}
