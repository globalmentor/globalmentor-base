/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.java;

import java.io.UnsupportedEncodingException;

import static com.globalmentor.java.Characters.*;
import static com.globalmentor.text.CharacterEncoding.*;

import com.globalmentor.text.unicode.UnicodeCharacter;
import com.globalmentor.util.Arrays;

/**Various text manipulating functions. These methods work on
	objects that implement the {@link CharSequence} interface.
	To avoid creation of new strings, some of these methods should
	be avoided in favor of their corresponding {@link StringBuilders}
	methods, which operate on {@link StringBuilder} objects.
@see StringBuilders
@author Garret Wilson
*/
public class CharSequences
{

	/**Searches a character sequence and returns the first index of any character
		in the specified string, starting at the beginning.
	@param charSequence The character sequence to be searched.
	@param charString The string of characters to check.
	@return The index of the first occurrence of one of the supplied characters, or -1 if none were found.
	*/
	public static int charIndexOf(final CharSequence charSequence, final String charString)
	{
		return charIndexOf(charSequence, charString, 0);	//look of the characters, starting at the beginning of the string
	}

	/**Searches a character sequence and returns the first index of any character
		in the specified string, starting at <var>fromIndex</var>.
	@param charSequence The character sequence to be searched.
	@param charString The string of characters to check.
	@param fromIndex The index to search from.
	@return The index of the first occurrence of one of the supplied characters, or -1 if none were found.
	*/
	public static int charIndexOf(final CharSequence charSequence, final String charString, final int fromIndex)
	{
		for(int i=fromIndex; i<charSequence.length(); ++i)	//look at each character in the sequence
		{
			if(charString.indexOf(charSequence.charAt(i))!=-1)	//if this character is in our character string
				return i;	//return the index we're at
		}
		return -1;	//if we make it to here, we didn't find any of the characters
	}

	/**Searches a character sequence in reverse and returns the last index of any
		character, starting from <var>fromIndex</var>.
	@param charSequence The character sequence to be searched.
	@param charString The string of characters to check.
	@return The index of the last occurrence of one of the supplied characters, or
		-1 if none were found.
	*/
	public static int charLastIndexOf(final CharSequence charSequence, final String charString)
	{
		return charLastIndexOf(charSequence, charString, charSequence.length()-1);  //search the sequence, starting at the end
	}

	/**Searches a character sequence in reverse and returns the last index of any
		character in the specified string, starting from <var>fromIndex</var>.
	@param charSequence The character sequence to be searched.
	@param charString The string of characters to check.
	@param fromIndex The index to search from.
	@return The index of the last occurrence of one of the supplied characters, or
		-1 if none were found.
	*/
	public static int charLastIndexOf(final CharSequence charSequence, final String charString, final int fromIndex)
	{
		for(int i=fromIndex; i>=0; --i)	//look at each character in the sequence , starting at the end
		{
			if(charString.indexOf(charSequence.charAt(i))!=-1)	//if this character is in our character string
				return i;	//return the index we're at
		}
		return -1;	//if we make it to here, we didn't find any of the characters
	}

	/**Ensures that the given character sequence has a minimum the specified number of characters.
	@param <T> The type of character sequence being used.
	@param charSequence The character sequence to check.
	@param minLength The minimum length required.
	@return The given character sequence.
	@exception NullPointerException if the given character sequence is <code>null</code>.
	@exception IllegalArgumentException if the length of the given character sequence is less than the indicated minimum length.
	*/
	public static <T extends CharSequence> T checkMinLength(final T charSequence, final int minLength)
	{
		if(charSequence.length()<minLength)	//if the length of the given characters sequence is less than required
		{
			throw new IllegalArgumentException("Character sequence is not at least "+minLength+" characters long: "+charSequence);
		}
		return charSequence;	//return the character sequence
	}
	
	/**Determines if a character sequence contains the given character.
	@param charSequence The character sequence to be searched.
	@param character The character to check.
	@return <code>true</code> if the given character sequence contains the given character.
	*/
	public static boolean contains(final CharSequence charSequence, final char character)
	{
		return indexOf(charSequence, character)>=0;	//see if the given character is in the character sequence
	}

	/**Determines if a character sequence contains any of the given characters.
	@param charSequence The character sequence to be searched.
	@param characters The characters to check.
	@return <code>true</code> if the given character sequence contains one of the given characters.
	*/
	public static boolean contains(final CharSequence charSequence, final char[] characters)
	{
		return indexOf(charSequence, characters)>=0;	//see if any of the given characters are in the character sequence
	}

	/**Determines if a character sequence contains any of the given characters.
	@param charSequence The character sequence to be searched.
	@param charString The string of characters to check.
	@return <code>true</code> if the given character sequence contains one of the
		given characters.
	*/
	public static boolean containsChar(final CharSequence charSequence, final String charString)
	{
		return charIndexOf(charSequence, charString)>=0;	//see if any of the given characters are in the character sequence
	}

	/**Determines if the following character sequence contains a letter.
	@param charSequence The character sequence to search.
	@return <code>true</code> if the sequence has at least one letter.
	*/
	public static boolean containsLetter(final CharSequence charSequence) //TODO maybe change this to indexOfLetterOrDigit
	{
		for(int i=charSequence.length()-1; i>=0; --i) //look at each character in the string
		{
			if(Character.isLetter(charSequence.charAt(i)))  //if this is a letter
				return true;  //we found a letter
		}
		return false; //we found no letters
	}

	/**Determines if the following character sequence contains a letter or a digit.
	@param charSequence The character sequence to search.
	@return <code>true</code> if the sequence has at least one letter or digit.
	@see Character#isLetterOrDigit(char)
	*/
	public static boolean containsLetterOrDigit(final CharSequence charSequence) //TODO maybe change this to indexOfLetterOrDigit
	{
		for(int i=charSequence.length()-1; i>=0; --i) //look at each character in the string
		{
			if(Character.isLetterOrDigit(charSequence.charAt(i)))  //if this is a letter or digit
				return true;  //we found a letter or digit
		}
		return false; //we found no letters or digits
	}

	/**Determines if a character sequence contains Unicode whitespace.
	Whitespace is denoted by the "WS" bidi class in <code>UnicodeData.txt</code>.
	This method does not handle Unicode supplementary characters.
	@param charSequence The character sequence to be searched.
	@return <code>true</code> if the given character sequence contains whitespace.
	@see Character#isSpaceChar(char)
	*/
	public static boolean containsWhitespace(final CharSequence charSequence)
	{
		for(int i=charSequence.length()-1; i>=0; --i) //look at each character in the string
		{
			if(Character.isSpaceChar(charSequence.charAt(i)))  //if this is whitespace
			{
				return true;  //we found whitespace
			}
		}
		return false; //we found no whitespace
	}

	/**Determines if a character sequence contains characters that are not Unicode whitespace (marked by "WS" in Unicode data).
	Whitespace is denoted by the "WS" bidi class in <code>UnicodeData.txt</code>.
	This method does not handle Unicode supplementary characters.
	@param charSequence The character sequence to be searched.
	@return <code>true</code> if the given character sequence contains non-whitespace.
	@see Character#isSpaceChar(char)
	*/
	public static boolean containsNonWhitespace(final CharSequence charSequence)
	{
		for(int i=charSequence.length()-1; i>=0; --i) //look at each character in the string
		{
			if(!Character.isSpaceChar(charSequence.charAt(i)))  //if this is not whitespace
			{
				return true;  //we found non-whitespace
			}
		}
		return false; //we found no non-whitespace
	}

	/**Determines if a character sequence contains characters that can be trimmed.
	Trimmed characters are denoted by the "WS" (whitespace) bidi class, the "Cc" (control) category,
	or the "Cf" (format) category  in <code>UnicodeData.txt</code>.
	This method does not handle Unicode supplementary characters.
	@param charSequence The character sequence to be searched.
	@return <code>true</code> if the given character sequence contains trim characters.
	@see Character#isSpaceChar(char)
	@see Character#isISOControl(char)
	@see Character#getType(char)
	*/
	public static boolean containsTrim(final CharSequence charSequence)
	{
		for(int i=charSequence.length()-1; i>=0; --i) //look at each character in the string
		{
			final char c=charSequence.charAt(i);	//get the current character
			if(Character.isSpaceChar(c) || Character.isISOControl(c) || Character.getType(c)==Character.FORMAT)  //if this is whitespace, control, or format
			{
				return true;  //we found a trim character
			}
		}
		return false; //we found no trim character
	}

	/**Determines if a character sequence contains characters that are not characters that can be trimmed.
	This is useful for determining if a characer sequence actually contains useful data.
	Trimmed characters are denoted by the "WS" (whitespace) bidi class, the "Cc" (control) category,
	or the "Cf" (format) category  in <code>UnicodeData.txt</code>.
	This method does not handle Unicode supplementary characters.
	@param charSequence The character sequence to be searched.
	@return <code>true</code> if the given character sequence contains non-trim characters.
	@see Character#isSpaceChar(char)
	@see Character#isISOControl(char)
	@see Character#getType(char)
	*/
	public static boolean containsNonTrim(final CharSequence charSequence)
	{
		for(int i=charSequence.length()-1; i>=0; --i) //look at each character in the string
		{
			final char c=charSequence.charAt(i);	//get the current character
			if(!Character.isSpaceChar(c) && !Character.isISOControl(c) && Character.getType(c)!=Character.FORMAT)  //if this is not whitespace, control, or format
			{
				return true;  //we found a non-trim character
			}
		}
		return false; //we found no non-trim character
	}

	/**Counts the number of occurences of a particular character in a character sequence.
	@param charSequence The character sequence to examine.
	@param character The character to count.
	@return The number of occurences of the character in the character sequence.
	*/
	public static int count(final CharSequence charSequence, final char character)
	{
		int count=0;	//start out without knowing any occurrences
		for(int i=charSequence.length()-1; i>=0; --i)	//look at each character
		{
			if(charSequence.charAt(i)==character)	//if this character matches the given characters
			{
				++count;	//show that we found one more occurence characters
			}
		}
		return count;	//return the total count
	}

	/**Determines if the character sequence ends with the given character.
	@param charSequence The character sequence to examine.
	@param character The character to compare.
	@return <code>true</code> if the last character of the character sequence
		matches that of the given string.
	*/
	public static boolean endsWith(final CharSequence charSequence, final char character)
	{
			//see if the character sequence has at least one character, and the last character matches our character
		return charSequence.length()>0 && charSequence.charAt(charSequence.length()-1)==character;
	}

	/**Determines if the character sequence ends with the given string.
	@param charSequence The character sequence to examine.
	@param string The string to compare.
	@return <code>true</code> if the last characters of the character sequence
		match those of the given string.
	*/
	public static boolean endsWith(final CharSequence charSequence, final String string)
	{
		final int delta=charSequence.length()-string.length();  //find out the difference in length between the strings
		if(delta<0) //if the substring is too long
			return false; //the substring is too big to start the character sequence
		for(int i=string.length()-1; i>=0; --i) //look at each character of the string
		{
			if(string.charAt(i)!=charSequence.charAt(i+delta))  //if these characters don't match in the same position
				return false; //the string doens't match
		}
		return true;  //the character sequence ends with the string
	}

	/**Determines if the character sequence ends with the given string without
		case sensitivity.
	@param charSequence The character sequence to examine.
	@param string The string to compare.
	@return <code>true</code> if the last characters of the character sequence
		match those of the given string, case insensitively.
	*/
	public static boolean endsWithIgnoreCase(final CharSequence charSequence, final String string)
	{
		final int delta=charSequence.length()-string.length();  //find out the difference in length between the strings
		if(delta<0) //if the substring is too long
			return false; //the substring is too big to start the character sequence
		for(int i=string.length()-1; i>=0; --i) //look at each character of the string
		{
			if(Character.toUpperCase(string.charAt(i))!=Character.toUpperCase(charSequence.charAt(i+delta)))  //if these characters don't match in the same position
				return false; //the string doens't match
		}
		return true;  //the character sequence ends with the string
	}

	/**Escapes the indicated characters in the character sequence using the supplied escape character.
	All characters are first encoded using UTF-8.
	Every invalid character is converted to its Unicode hex equivalent and prefixed with the given escape character.
	This method uses lowercase hexadecimal escape codes.
	Characters are assumed to be valid unless specified otherwise.
	The escape character, if encountered, is not escaped unless it specifically meets one of the specified criteria;
	this allows re-escaping strings that may contain escape characters produced under less-strict rules
	(e.g. a URI containing escaped restricted characters, but still containing non-ASCII characters).
	@param charSequence The data to escape.
	@param validCharacters The characters that should not be escaped and all others should be escaped, or <code>null</code> if characters should not be matched against valid characters.
	@param invalidCharacters The characters that, if they appear, should be escaped, or <code>null</code> if characters should not be matched against invalid characters.
	@param maxCharacter The character value that represents the highest non-escaped value.
	@param escapeChar The character to prefix the hex representation.
	@param escapeLength The number of characters to use for the hex representation.
	@return A string containing the escaped data.
	@exception IllegalArgumentException if neither valid nor invalid characters are given.
	*/
	public static String escapeHex(final CharSequence charSequence, final String validCharacters, final String invalidCharacters, final int maxCharacter, final char escapeChar, final int escapeLength)
	{
		final StringBuilder stringBuilder=new StringBuilder(charSequence); //put the string in a string builder so that we can work with it; although inserting encoded sequences may seem inefficient, it should be noted that filling a string buffer with the entire string is more efficient than doing it one character at a time, that characters needed encoding are generally uncommon, and that any copying of the string characters during insertion is done via a native method, which should happen very quickly
		for(int characterIndex=stringBuilder.length()-1; characterIndex>=0; --characterIndex) //work backwords; this keeps us from having a separate variable for the length, but it also makes it simpler to calculate the next position when we swap out characters
		{
			final char c=stringBuilder.charAt(characterIndex); //get the current character
			final boolean encode=(validCharacters!=null && validCharacters.indexOf(c)<0)	//encode if there is a list of valid characters and this character is not one of them
				|| (invalidCharacters!=null && invalidCharacters.indexOf(c)>=0)	//encode if there is a list of invalid characters and this character is one of them
				|| (c>maxCharacter);	//encode the character if it is past the given upper bound
			if(encode)	//if this a character to escape
			{
				try
				{
					final byte[] bytes=String.valueOf(c).getBytes(UTF_8); //convert this character to a sequence of UTF-8 bytes
					final int byteCount=bytes.length; //find out how many bytes there are
					final StringBuilder encodeStringBuilder=new StringBuilder(byteCount*3); //create a string builder to hold three characters for each byte we have (the escape character plus a two-digit encoded value)
					for(int byteIndex=0; byteIndex<byteCount; ++byteIndex) //look at each byte
					{
						encodeStringBuilder.append(escapeChar); //escape character
						encodeStringBuilder.append(Integers.toHexString(bytes[byteIndex], escapeLength)); //hh
					}
					stringBuilder.replace(characterIndex, characterIndex+1, encodeStringBuilder.toString()); //replace the character with its encoding
				}
				catch(final UnsupportedEncodingException unsupportedEncodingException) //the JVM should always know how to convert a string to UTF-8
				{
					throw new AssertionError(unsupportedEncodingException);
				}
			}
		}
		return stringBuilder.toString(); //return the encoded version of the string
	}

	/**Decodes the escaped characters in the character sequence by converting the hex value after each occurance of the escape character to the corresponding Unicode character using UTF-8.
	@param charSequence The data to unescape.
	@param escapeChar The character that prefixes the hex representation.
	@param escapeLength The number of characters used for the hex representation.
	@return A string containing the unescaped data.
	@exception IllegalArgumentException if the given characters contains a character greater than U+00FF.
	@exception IllegalArgumentException if a given escape character is not followed by an escape sequence.
	*/
	public static String unescapeHex(final CharSequence charSequence, final char escapeChar, final int escapeLength)
	{
		final int charSequenceLength=charSequence.length(); //get the length of the character sequence
		final byte[] decodedBytes=new byte[charSequenceLength]; //create an array of byte to hold the UTF-8 data
		int byteArrayIndex=0; //start at the first position in the byte array
		for(int i=0; i<charSequenceLength; ++i) //look at each character in the character sequence
		{
			final char c=charSequence.charAt(i); //get a reference to this character in the character sequence
			final byte b; //we'll determine what byte goes at this position
			if(c==escapeChar) //if this is the beginning of an escaped sequence
			{
				if(i<charSequenceLength-escapeLength) //if there's room for enough hex characters after it
				{
					final String escapeSequence=charSequence.subSequence(i+1, i+escapeLength+1).toString(); //get the hex characters in the escape sequence							
					try
					{
						b=(byte)Integer.parseInt(escapeSequence, 16); //convert the escape sequence to a single integer value and add it to the buffer
						i+=2; //skip the escape sequence (we'll go to the last character, and we'll be advanced one character when we go back to the start of the loop)
					}
					catch(NumberFormatException numberFormatException) //if the characters weren't really hex characters
					{
						throw new IllegalArgumentException("Invalid escape sequence "+escapeSequence+" at index "+i+" in character sequence \""+charSequence+"\".");
					}
				}
				else	//if there is no room for an escape sequence at the end of the string
				{
					throw new IllegalArgumentException("Invalid escape sequence "+charSequence.subSequence(i+1, charSequenceLength)+" at index "+i+" in character sequence \""+charSequence+"\".");
				}
			}
			else	//if this is not an escaped character
			{
				if(c>0xff) //if this character is larger than a byte, the character sequence was not encoded correctly
				{
					throw new IllegalArgumentException("Invalid encoded character "+UnicodeCharacter.getCodePointString(c)+" at index "+i+" in character sequence \""+charSequence+"\".");
				}
				b=(byte)c; //add this character to the result with no change
			}
			decodedBytes[byteArrayIndex++]=b; //add the byte to the buffer and keep going
		}
		try
		{
			return new String(decodedBytes, 0, byteArrayIndex, UTF_8); //consider the bytes as a series of UTF-8 encoded characters.
		}
		catch(final UnsupportedEncodingException unsupportedEncodingException) //UTF-8 should always be supported
		{
			throw new AssertionError(unsupportedEncodingException);
		}
	}


	/**Determines the first index of the given character.
	@param charSequence The character sequence to check.
	@param character The character to search for.
	@return The index of the first occurrence of the given character, or -1 if
		the character was not found.
	*/
	public static int indexOf(final CharSequence charSequence, final char character)
	{
		return indexOf(charSequence, character, 0);	//search from the beginning
	}

	/**Determines the first index of the given character.
	@param charSequence The character sequence to check.
	@param character The character to search for.
	@return The index of the first occurrence of the given character, or the length of the character sequence if the character was not found.
	*/
	public static int indexOfLength(final CharSequence charSequence, final char character)
	{
		final int index=indexOf(charSequence, character);	//perform the search
		return index>=0 ? index : charSequence.length();	//return the length if we're out of characters
	}

	/**Determines the first index of the given character.
	If the character sequence is a {@link String}, this method delegates to {@link String#indexOf(int, int)}.
	@param charSequence The character sequence to check.
	@param character The character to search for.
	@param index The first index to examine.
	@return The index of the first occurrence of the given character, or -1 if the character was not found.
	*/
	public static int indexOf(final CharSequence charSequence, final char character, final int index)
	{
		if(charSequence instanceof String)	//if the character sequence is a string
		{
			return ((String)charSequence).indexOf(character, index);	//delegate to the String version, which is much more efficient
		}
		final int length=charSequence.length();
		for(int i=index; i<length; ++i)	//look at each character
		{
			if(charSequence.charAt(i)==character)	//if this character matches
			{
				return i;	//return the matching index
			}
		}
		return -1;	//show that we couldn't find the character
	}

	/**Determines the first index of the given character.
	If the character sequence is a {@link String}, this method delegates to {@link String#indexOf(int, int)}.
	@param charSequence The character sequence to check.
	@param character The character to search for.
	@param index The first index to examine.
	@return The index of the first occurrence of the given character, or the length of the character sequence if the character was not found.
	*/
	public static int indexOfLength(final CharSequence charSequence, final char character, int index)
	{
		index=indexOf(charSequence, character, index);	//perform the search
		return index>=0 ? index : charSequence.length();	//return the length if we're out of characters		
	}

	/**Searches a character sequence and returns the first index of any character in the specified array, starting at the beginning.
	@param charSequence The character sequence to be searched.
	@param characters The characters to check.
	@return The index of the first occurrence of one of the supplied characters, or -1 if none were found.
	*/
	public static int indexOf(final CharSequence charSequence, final char[] characters)
	{
		return indexOf(charSequence, characters, 0);	//look of the characters, starting at the beginning of the string
	}

	/**Searches a character sequence and returns the first index of any character in the specified array, starting at the beginning.
	@param charSequence The character sequence to be searched.
	@param characters The characters to check.
	@return The index of the first occurrence of one of the supplied characters, or the length of the character sequence if none were found.
	*/
	public static int indexOfLength(final CharSequence charSequence, final char[] characters)
	{
		final int index=indexOf(charSequence, characters);	//perform the search
		return index>=0 ? index : charSequence.length();	//return the length if we're out of characters
	}

	/**Searches a character sequence and returns the first index of any character in the specified array, starting at <var>fromIndex</var>.
	@param charSequence The character sequence to be searched.
	@param characters The characters to check.
	@param fromIndex The index to search from.
	@return The index of the first occurrence of one of the supplied characters, or -1 if none were found.
	*/
	public static int indexOf(final CharSequence charSequence, final char[] characters, final int fromIndex)
	{
		for(int i=fromIndex; i<charSequence.length(); ++i)	//look at each character in the sequence
		{
			if(Arrays.contains(characters, charSequence.charAt(i)))	//if this character is in our character string
				return i;	//return the index we're at
		}
		return -1;	//if we make it to here, we didn't find any of the characters
	}

	/**Searches a character sequence and returns the first index of any character in the specified array, starting at <var>fromIndex</var>.
	@param charSequence The character sequence to be searched.
	@param characters The characters to check.
	@param fromIndex The index to search from.
	@return The index of the first occurrence of one of the supplied characters, or the length of the character sequence if none were found.
	*/
	public static int indexOfLength(final CharSequence charSequence, final char[] characters, final int fromIndex)
	{
		final int index=indexOf(charSequence, characters, fromIndex);	//perform the search
		return index>=0 ? index : charSequence.length();	//return the length if we're out of characters
	}

	/**Searches a character sequence and returns the first index of any character
		<em>not</em> in the specified string, starting from the beginning.
	@param charSequence The character sequence to be searched.
	@param notCharString The string of characters to check.
	@return The index of the first occurrence of one of the supplied characters,
		or -1 if none were found.
	*/
	public static int notCharIndexOf(final CharSequence charSequence, final String notCharString)
	{
		return notCharIndexOf(charSequence, notCharString, 0);  //start looking from the beginning
	}

	/**Searches a character sequence and returns the first index of any character
		<em>not</em> in the specified string, starting at <var>fromIndex</var>.
	@param charSequence  The character sequence to be searched.
	@param notCharString The string of characters to check.
	@param fromIndex The index to search from.
	@return The index of the first occurrence of one of the supplied characters,
		or -1 if none were found.
	*/
	public static int notCharIndexOf(final CharSequence charSequence, final String notCharString, final int fromIndex)
	{
		for(int i=fromIndex; i<charSequence.length(); ++i)	//look at each character in the sequence
		{
			if(notCharString.indexOf(charSequence.charAt(i))<0)	//if this character is not in our character string
				return i;	//return the index we're at
		}
		return -1;	//if we make it to here, we didn't find any characters which weren't in our character string
	}

	/**Searches a character sequence and returns the last index of any character
		<em>not</em> in the specified string, starting at the last index.
	@param charSequence  The character sequence to be searched.
	@param notCharString The string of characters to check.
	@return The index of the last occurrence of one of the supplied characters,
		or -1 if none were found.
	*/
	public static int notCharLastIndexOf(final CharSequence charSequence, final String notCharString)
	{
		return notCharLastIndexOf(charSequence, notCharString, charSequence.length()-1);  //start searching from the end
	}

	/**Searches a character sequence and returns the last index of any character
		<em>not</em> in the specified string, starting at <var>fromIndex</var>.
	@param charSequence The character sequence to be searched.
	@param notCharString The string of characters to check.
	@param fromIndex The index to search from.
	@return The index of the last occurrence of one of the supplied characters,
		or -1 if none were found.
	*/
	public static int notCharLastIndexOf(final CharSequence charSequence, final String notCharString, final int fromIndex)
	{
		for(int i=fromIndex; i>=0; --i)	//look at each character in the sequence , looking from right to left
		{
			if(notCharString.indexOf(charSequence.charAt(i))<0)	//if this character is not in our character string
				return i;	//return the index we're at
		}
		return -1;	//if we make it to here, we didn't find any characters which weren't in our character string
	}

	/**Determines if the character sequence consists of nothing but the following
		character.
	@param charSequence The character sequence to examine.
	@param c The character that could make up the entire sequence.
	@return <code>true</code> if there are no other characters but the specified
	  character, <code>false</code> if there are other characters or if the string
		is the empty string.
	*/
	public final static boolean isAll(final CharSequence charSequence, final char c)
	{
		if(charSequence.length()==0) //if this is an empty string
			return false; //there are no characters to check
		for(int i=charSequence.length()-1; i>=0; --i)  //look at each character in the string
		{
			if(charSequence.charAt(i)!=c) //if this isn't the specified character
				return false; //show that the string contains other characters besides the one specified
		}
		return true;  //if we make it to here, there weren't any characters other than the one specified
	}

	/**Determines if the character sequence consists of nothing but characters in
		the given string.
	@param charSequence The character sequence to examine.
	@param characters The characters that could make up the entire string, in any
		order.
	@return <code>true</code> if there are no other characters but the specified
	  characters, <code>false</code> if there are other characters or if the
		character sequence is empty.
	*/
	public final static boolean isAllChars(final CharSequence charSequence, final String characters)
	{
		if(charSequence.length()==0) //if this is an empty string
			return false; //there are no characters to check
		for(int i=charSequence.length()-1; i>=0; --i)  //look at each character in the string
		{
			if(characters.indexOf(charSequence.charAt(i))<0) //if this character isn't in the string
				return false; //show that the string contains other characters besides the ones specified
		}
		return true;  //if we make it to here, there weren't any characters other than the ones specified
	}

	/**Determines whether a character sequence is capitalized.
		A character sequence is capitalized if it contains any characters and the
		first character is uppercase.
	@param charSequence The character sequence to examine.
	@return <code>true</code> if the character sequence is capitalized.
	*/
	public final static boolean isCapitalized(final CharSequence charSequence)
	{
		return charSequence.length()>0 && Character.isUpperCase(charSequence.charAt(0)); //determine if the first character is capitalized
	}

	/**Determines whether a character sequence contains only Unicode digits.
	@param charSequence The character sequence to examine.
	@return <code>true</code> if all the characters in the sequence are digits.
	*/
	public final static boolean isDigits(final CharSequence charSequence)
	{
		if(charSequence.length()==0) //if this is an empty string
			return false; //there are no characters to check
		for(int i=charSequence.length()-1; i>=0; --i)  //look at each letter in the string
		{
			if(!Character.isDigit(charSequence.charAt(i))) //if this isn't a digit
				return false; //show that the string doesn't contain only digits
		}
		return true;  //if we make it to here, there weren't any non-digits in the string
	}

	/**Determines whether a character sequence contains only the digits '0'-'9'.
	@param charSequence The character sequence to examine.
	@return <code>true</code> if all the characters in the sequence are ISO_LATIN_1 digits.
	*/
	public final static boolean isLatinDigits(final CharSequence charSequence)
	{
		if(charSequence.length()==0) //if this is an empty string
			return false; //there are no characters to check
		for(int i=charSequence.length()-1; i>=0; --i)  //look at each letter in the string
		{
			if(!isLatinDigit(charSequence.charAt(i))) //if this isn't a Latin digit
				return false; //show that the string doesn't contain only latin digits
		}
		return true;  //if we make it to here, there weren't any non-latin-digits in the string
	}

	/**Determines whether a character sequence contains only Unicode letters.
	@param charSequence The character sequence to examine.
	@return <code>true</code> if all the characters in the sequence are letters.
	*/
	public final static boolean isLetters(final CharSequence charSequence)
	{
		if(charSequence.length()==0) //if this is an empty string
			return false; //there are no characters to check
		for(int i=charSequence.length()-1; i>=0; --i)  //look at each letter in the string
		{
			if(!Character.isLetter(charSequence.charAt(i))) //if this isn't a letter
				return false; //show that the string doesn't contain only letters
		}
		return true;  //if we make it to here, there weren't any non-letters in the string
	}

	/**Determines whether a character sequence contains only Unicode letters
		and digits.
	@param charSequence The character sequence to examine.
	@return <code>true</code> if all the characters in the sequence are letters
		and digits.
	*/
	public final static boolean isLettersDigits(final CharSequence charSequence)
	{
		if(charSequence.length()==0) //if this is an empty string
			return false; //there are no characters to check
		for(int i=charSequence.length()-1; i>=0; --i)  //look at each letter in the string
		{
			final char character=charSequence.charAt(i);  //get this character
			if(!Character.isLetter(character) && !Character.isDigit(character))  //if this is not a letter or a digit
				return false; //show that the string contains non-letter or non-digit characters
		}
		return true;  //if we make it to here, there weren't any non-letters or non-digits in the string
	}

	/**Determines whether a character sequence contains only Unicode letters, digits,
	 	and the supplied extra characters.
	@param charSequence The character sequence to examine.
	@param characters Extra characters to allow.
	@return <code>true</code> if all the characters in the sequence are letters, digits, and/or allowed characters.
	*/
	public final static boolean isLettersDigitsCharacters(final CharSequence charSequence, final String characters)
	{
		if(charSequence.length()==0) //if this is an empty string
			return false; //there are no characters to check
		for(int i=charSequence.length()-1; i>=0; --i)  //look at each letter in the string
		{
			final char character=charSequence.charAt(i);  //get this character
			if(!Character.isLetter(character) && !Character.isDigit(character) && !contains(characters, character))  //if this is not a letter or a digit, and it's not in our extra character list
				return false; //show that the string contains something in none of our lists 
		}
		return true;  //if we make it to here, there weren't any non-letters or non-digits in the string
}

	/**Determines whether a character sequence contains only numbers and decimals
		or commas.
	@param charSequence The character sequence to examine.
	@return <code>true</code> if all the characters represent a number.
	*/
	public final static boolean isNumber(final CharSequence charSequence) //TODO use a regex, and verify format
	{
		if(charSequence.length()==0) //if this is an empty string
			return false; //there are no characters to check
		for(int i=charSequence.length()-1; i>=0; --i)  //look at each letter in the string
		{
			final char c=charSequence.charAt(i);  //get this character
			if(!Character.isDigit(c) && c!='.' && c!=',') //if this isn't a digit, a decimal, or a comma
				return false; //show that the string doesn't contain a number
		}
		return true;  //if we make it to here, this is a number
	}

	/**Determines whether a character sequeence contains only Roman numerals.
	@param charSequence The character sequence to examine.
	@return <code>true</code> if all the characters in the sequence are roman numerals.
	*/
	public final static boolean isRomanNumerals(final CharSequence charSequence)
	{
		if(charSequence.length()==0) //if this is an empty string
			return false; //there are no characters to check
		for(int i=charSequence.length()-1; i>=0; --i)  //look at each character in the string
		{
			if(!isRomanNumeral(charSequence.charAt(i))) //if this isn't a roman numberal
				return false; //show that the string doesn't contain only roman numberals
		}
		return true;  //if we make it to here, there weren't any characters in the string that were not roman numerals
	}

	/**Determines whether all the letters in a character sequence are capital letters.
	@param charSequence The character sequence to examine.
	@return <code>true</code> if all the letters in the sequence are capitalized.
	*/
	public final static boolean isUpperCase(final CharSequence charSequence)
	{
		if(charSequence.length()==0) //if this is an empty string
			return false; //there are no characters to check
		for(int i=charSequence.length()-1; i>=0; --i)  //look at each letter in the string
		{
			final char character=charSequence.charAt(i);  //get this character
			if(Character.isLetter(character) && !Character.isUpperCase(character))  //if this is a letter that is not uppercase
				return false; //show that the string contains non-uppercase characters
		}
		return true;  //if we make it to here, there weren't any non-uppercase characters in the string
	}

	/**Concatenates the given character sequences.
	@param charSequences The character sequences to be concatenated.
	@return The string containing the concatenated character sequences.
	@throws NullPointerException if the given character sequences is <code>null</code>.
	*/
	public static CharSequence join(final CharSequence... charSequences)
	{
		final int length=charSequences.length;	//find out how many character sequences there are
		if(length>1)	//if there are more than one character sequence
		{
			return StringBuilders.append(new StringBuilder(), charSequences);	//join the character strings using a string builder
		}
		else if(length==1)	//if there is only one character sequence
		{
			return charSequences[0];	//return the one character sequence
		}
		else	//if there are no character sequences
		{
			return "";	//return the empty string
		}
	}

	/**Concatenates the given character sequences, separated by the given delimiter.
	@param delimiter The delimiter to be placed between each character sequence.
	@param charSequences The character sequences to be concatenated.
	@return The string containing the concatenated character sequences.
	@throws NullPointerException if the given character sequences is <code>null</code>.
	*/
	public static CharSequence join(final char delimiter, final CharSequence... charSequences)
	{
		final int length=charSequences.length;	//find out how many character sequences there are
		if(length>1)	//if there are more than one character sequence
		{
			return StringBuilders.append(new StringBuilder(), delimiter, charSequences);	//join the character strings using a string builder
		}
		else if(length==1)	//if there is only one character sequence
		{
			return charSequences[0];	//return the one character sequence
		}
		else	//if there are no character sequences
		{
			return "";	//return the empty string
		}
	}

	/**Splits a characters sequence into subsequences based upon the given delimiter.
	Subsequences will be returned between delimiters even if they are empty, and
		a subsequence will be returned after the last delimiter, even if there are no
		remaining characters. In other words, the number of character subsequences returned
		is <var>delimiterCount</var>+1.
	@param charSequence The character sequence to split.
	@param delimiter The delimiter to use for splitting.
	@return An array of character subsequences between the delimiters.
	@deprecated
	*/
	public static CharSequence[] split(final CharSequence charSequence, final char delimiter)	//TODO convert to using regular expressions
	{
		final int length=charSequence.length();	//get the length of the character sequence
		if(length>0)	//if there are any characters
		{
			final int delimiterCount=count(charSequence, delimiter);	//count the number of delimiters
			if(delimiterCount>0)	//if there is at least one delimiter
			{
					//count the delimiters; this should be faster than creating a list and dynamically adding subsequences
				final CharSequence[] subSequences=new CharSequence[delimiterCount+1];	//there will always be one more character sequence than delimiter
				int start=0;	//start searching at the beginning
				int delimiterIndex;	//we'll keep track of where we find the delimiter each time
				int i=0;	//keep track of the subsequence index
				do
				{
					assert start<charSequence.length() : "Delmiter counting and splitting logic out of synchronization.";
					delimiterIndex=indexOf(charSequence, delimiter, start);	//find the index of the next delimiter
					final int end=delimiterIndex>=0 ? delimiterIndex : length;	//if we didn't find a delimiter, just use the rest of the character sequence
					subSequences[i]=charSequence.subSequence(start, end);	//create a subsequence between delimiters
					start=end+1;	//start looking at the position after the delimiter
					++i;	//go to the next position for storing subsequences
				}
				while(i<subSequences.length);	//keep looking until we've found the correct number of delimiters
				return subSequences;	//return the array of subsequences
			}
		}
		return new CharSequence[]{charSequence};	//return an array cotaining the character sequence itself if there are no characters or no delimiters
	}
	
	/**Determines if the character sequence starts with the given character.
	@param charSequence The character sequence to examine.
	@param character The character to compare.
	@return <code>true</code> if the first character of the character sequence matches that of the given string.
	*/
	public static boolean startsWith(final CharSequence charSequence, final char character)
	{
			//see if the character sequence has at least one character, and the first character matches our character
		return charSequence.length()>0 && charSequence.charAt(0)==character;
	}

	/**Determines if the character sequence starts with the given string.
	@param charSequence The character sequence to examine.
	@param string The string to compare.
	@return <code>true</code> if the first characters of the character sequence
		match those of the given string.
	*/
	public static boolean startsWith(final CharSequence charSequence, final String string)	//TODO refactor startsWith() and endsWith() into a generic method
	{
		if(charSequence.length()<string.length()) //if the substring is too long
			return false; //the substring is too big to start the character sequence
		for(int i=string.length()-1; i>=0; --i) //look at each character of the string
		{
			if(string.charAt(i)!=charSequence.charAt(i))  //if these characters don't match in the same position
				return false; //the string doens't match
		}
		return true;  //the character sequence starts with the string
	}

	/**Determines if the character sequence starts with one of the given characters.
	@param charSequence The character sequence to examine.
	@param characters The characters to compare.
	@return <code>true</code> if the first character of the character sequence
		matches one of those in the given string.
	*/
	public static boolean startsWithChar(final CharSequence charSequence, final String characters)
	{
			//see if the character sequence has at least one character, and the first character matches our character
		return charSequence.length()>0 && characters.indexOf(charSequence.charAt(0))>=0;
	}

	/**Trims the right side of the string beginning at the first occurrence of the
		given character. If the character sequence does not contain the trim
		character, no action takes place.
	@param charSequence The character sequence to check.
	@param trimChar The character indicating the part of the sequence to trim.
	@return A new character sequence with the specified character and following
		characters removed.
	*/
	public static CharSequence trimRightFirst(final CharSequence charSequence, final char trimChar)
	{
		final int index=indexOf(charSequence, trimChar);	//find the first occurrence of the trim character
		return index>=0 ? charSequence.subSequence(index+1, charSequence.length()) : charSequence;	//trim the character sequence if we can		
	}

	/**Determines if the given character sequence is composed of the single given character.
	This method allows comparison of a character string with a character without creating a string for the character, for example.
	@param charSequence The character sequence to compare.
	@param character The character to compare with the character sequence.
	@return <code>true</code> if the character sequence is composed of one character and that character matches the given character.
	*/
	public final static boolean equals(final CharSequence charSequence, final char character)
	{
		return charSequence.length()==1 && charSequence.charAt(0)==character;	//see if the character sequence has only one character, the given character
	}
}
