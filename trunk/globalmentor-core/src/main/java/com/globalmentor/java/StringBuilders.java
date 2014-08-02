/*
 * Copyright © 1996-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.io.Charsets.*;
import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Characters.*;

import com.globalmentor.text.Case;

/**
 * Various methods that manipulate <code>StringBuilder</code> objects. These methods are fast relative to their {@link Strings} counterparts, because the
 * {@link StringBuilder} objects on which they operate can be modified within the object instead of forcing a new object to be created. These methods
 * furthermore modify the specified {@link StringBuilder} object rather than returning a new object.
 * @author Garret Wilson
 * @see Strings
 */
public class StringBuilders
{

	private StringBuilders()
	{
	}

	/**
	 * Concatenates the string representations of the objects in the array by appending them to the string buffer. Null objects are handled as per
	 * {@link StringBuilder#append(Object)}.
	 * @param stringBuilder The string builder which the result should be placed.
	 * @param objects The array of objects (such as strings) to be concatenated.
	 * @return The string builder containing the new information.
	 * @see StringBuilder#append(Object)
	 */
	public static StringBuilder append(final StringBuilder stringBuilder, final Object... objects)
	{
		for(final Object object : objects) //for each object
		{
			stringBuilder.append(object); //append this object
		}
		return stringBuilder; //return the string builder object
	}

	/**
	 * Concatenates the given character sequences by appending them to the string buffer.
	 * @param stringBuilder The string builder which the result should be placed.
	 * @param charSequences The character sequences to be appended.
	 * @return The string builder containing the new information.
	 * @throws NullPointerException if the given character sequences is <code>null</code>.
	 */
	public static StringBuilder append(final StringBuilder stringBuilder, final CharSequence... charSequences)
	{
		return append(stringBuilder, UNDEFINED_CHAR, charSequences);
	}

	/**
	 * Concatenates the given character sequences by appending them to the string buffer, separated by the given delimiter.
	 * @param stringBuilder The string builder which the result should be placed.
	 * @param delimiter The delimiter to be placed between each character sequence, or {@link Characters#UNDEFINED_CHAR} if no delimiter should be placed between
	 *          the character sequences.
	 * @param charSequences The character sequences to be appended.
	 * @return The string builder containing the new information.
	 * @throws NullPointerException if the given string builder and/or character sequences is <code>null</code>.
	 */
	public static StringBuilder append(final StringBuilder stringBuilder, final char delimiter, final CharSequence... charSequences)
	{
		final int length = charSequences.length; //find out how many character sequences there are
		if(length > 0) //if there are any character sequences
		{
			for(final CharSequence charSequence : charSequences) //for each character sequence
			{
				stringBuilder.append(charSequence); //append this character sequence
				if(delimiter != UNDEFINED_CHAR) //if we have a delimiter
				{
					stringBuilder.append(delimiter); //append the given delimiter
				}
			}
			stringBuilder.deleteCharAt(stringBuilder.length() - 1); //remove the last delimiter
		}
		return stringBuilder; //return the string builder object
	}

	/**
	 * Appends a given repetition of characters to a string builder.
	 * @param stringBuilder The string builder to which the characters should be appended.
	 * @param character The character to append.
	 * @param count The number of repetitions of the character.
	 * @return The string builder with the appended repetitions of the character.
	 */
	public static StringBuilder append(final StringBuilder stringBuilder, final char character, int count)
	{
		insert(stringBuilder, stringBuilder.length(), character, count); //insert the characters at the end of the string builder
		return stringBuilder; //return the string builder object
	}

	/**
	 * Returns a string builder version of the given characters sequence. If the given character sequence is already a string builder, it is returned; otherwise,
	 * a new string builder will be created from the contents of the character sequence.
	 * @param charSequence The character sequence for which a string builder should be returned.
	 * @return A string builder with the contents of the given character sequence.
	 */
	public static StringBuilder asStringBuilder(final CharSequence charSequence)
	{
		return charSequence instanceof StringBuilder ? (StringBuilder)charSequence : new StringBuilder(charSequence); //return a new or existing string builder
	}

	/**
	 * Appends a character to a string builder. This method supports Unicode supplementary code points.
	 * @param stringBuilder The string builder to which to append.
	 * @param c The character to append.
	 * @return The string builder.
	 * @throws NullPointerException if the given string builder is <code>null</code>.
	 */
	public static StringBuilder appendChar(final StringBuilder stringBuilder, final int c)
	{
		if(Character.isSupplementaryCodePoint(c)) //if this is a supplementary code point
		{
			for(final char surrogate : Character.toChars(c)) //add each surrogate
			{
				stringBuilder.append(surrogate);
			}
		}
		else
		//if this is not a supplementary code point
		{
			stringBuilder.append((char)c); //append the character normally
		}
		return stringBuilder;
	}

	/**
	 * Removes all the content of a string builder.
	 * @param stringBuilder The string builder the content of which should be cleared.
	 * @return The string builder after all content is removed.
	 */
	public static StringBuilder clear(final StringBuilder stringBuilder)
	{
		return stringBuilder.delete(0, stringBuilder.length()); //delete all characters in the string builder
	}

	/**
	 * Inserts a given repetition of characters into a string builder.
	 * @param stringBuilder The string builder into which the characters should be inserted.
	 * @param offset The index at which to insert the characters.
	 * @param character The character to append.
	 * @param count The number of repetitions of the character.
	 * @return The string builder with the inserted repetitions of the character.
	 * @throws StringIndexOutOfBoundsException if the index is negative or greater than the length.
	 */
	public static StringBuilder insert(final StringBuilder stringBuilder, final int offset, final char character, final int count)
	{
		final char[] buffer = new char[count]; //create a new array of characters
		for(int i = count - 1; i >= 0; buffer[i--] = character)
			; //fill the buffer with the specified character
		stringBuilder.insert(offset, buffer); //insert the characters into the string builder
		return stringBuilder; //return the string builder object
	}

	/**
	 * Inserts a character sequence into a string builder. This method offers identical functionality to JDK 5.0
	 * {@link StringBuilder#insert(int, java.lang.CharSequence)} and is provided here for backwards-compatibility using RetroWeaver, for example.
	 * @param stringBuilder The string builder into which the characters should be inserted.
	 * @param offset The index at which to insert the characters.
	 * @throws IndexOutOfBoundsException if the given destination index is outside the bounds of the string builder.
	 * @see StringBuilder#insert(int, java.lang.CharSequence)
	 */
	public static StringBuilder insert(final StringBuilder stringBuilder, final int offset, CharSequence charSequence)
	{
		if(charSequence == null) //if the character sequence is null
			charSequence = "null"; //replace it with the string "null"
		if(charSequence instanceof String) //if the character sequence is a string
		{
			return stringBuilder.insert(offset, (String)charSequence); //use the already-made string insertion method
		}
		else
		//if the character sequence is not a string
		{
			//TODO fix full non-JDK 1.5 support
			return stringBuilder.insert(offset, charSequence, 0, charSequence.length()); //insert the character sequence into the string builder
		}
	}

	/**
	 * Collapses every run of any number of collapseChars to a single replaceString.
	 * @param stringBuilder The buffer in which the information will be collapsed.
	 * @param collapseChars The characters to be removed from the string.
	 * @param replaceString The string which will replace the collapseChars.
	 * @return The new length of the run with collapsed characters.
	 */
	public static int collapse(final StringBuilder stringBuilder, final Characters collapseChars, final String replaceString)
	{
		return collapse(stringBuilder, collapseChars, replaceString, 0); //collapse the entire string builder
	}

	/**
	 * Collapses every run of any number of collapseChars to a single replaceString, starting with the given offset and collapsing until the end of the string
	 * @param stringBuilder The buffer in which the information will be collapsed.
	 * @param collapseChars The characters to be removed from the string.
	 * @param replaceString The string which will replace the collapseChars.
	 * @param offset The offset at which to start collapsing.
	 * @return The new length of the run with collapsed characters.
	 */
	public static int collapse(final StringBuilder stringBuilder, final Characters collapseChars, final String replaceString, final int offset)
	{
		return collapse(stringBuilder, collapseChars, replaceString, offset, stringBuilder.length()); //collapse the rest of the string builder
	}

	/**
	 * Collapses every run of any number of collapseChars to a single replaceString.
	 * @param stringBuilder The buffer in which the information will be collapsed.
	 * @param collapseChars The characters to be removed from the string.
	 * @param replaceString The string which will replace the collapseChars.
	 * @param offset The offset at which to start collapsing.
	 * @param length The length of the run of characters to check.
	 * @return The new length of the run with collapsed characters.
	 */
	public static int collapse(final StringBuilder stringBuilder, final Characters collapseChars, final String replaceString, final int offset, final int length)
	{
		final int replaceLength = replaceString.length(); //find the length of the replacement string
		int nextIndex = offset; //start at the requested offset
		int newLength = length; //find out the resulting length; this will change as we collapse characters
		while(nextIndex < newLength) //keep going until we reach the endpoint
		{
			if(collapseChars.contains(stringBuilder.charAt(nextIndex))) //if this character is one of our remove characters
			{
				//TODO del Log.trace("found collapse character at index: ", nextIndex);  //TODO del
				int removeEnd = nextIndex + 1; //start looking at the next character to see how long this run is
				while(removeEnd < stringBuilder.length() && collapseChars.contains(stringBuilder.charAt(removeEnd))) //while we still have more characters, and the characters we're finding are characters to remove
				{
					++removeEnd;
				}
				stringBuilder.replace(nextIndex, removeEnd, replaceString); //replace our characters with the given string
				newLength += replaceLength - (removeEnd - nextIndex); //find out how much the string changed and add this to our length
				nextIndex += replaceLength; //move to the position after the replacement string
			}
			else
			//if this is not a character to replace
			{
				++nextIndex; //look at the next character in the string
			}
		}
		return newLength; //return the new length of the run
	}

	/**
	 * Deletes the last character of a string builder.
	 * @param stringBuilder The string builder to modify.
	 * @throws NullPointerException if the given string builder is <code>null</code>.
	 * @throws StringIndexOutOfBoundsException if the given string builder has no characters.
	 */
	public static void deleteEnd(final StringBuilder stringBuilder)
	{
		stringBuilder.deleteCharAt(stringBuilder.length() - 1); //remove the last character
	}

	/**
	 * Deletes the last characters of a string builder.
	 * @param stringBuilder The string builder to modify.
	 * @param count The number of characters to delete.
	 * @throws IllegalArgumentException if the given string builder has insuficient characters.
	 * @throws StringIndexOutOfBoundsException if the given string builder is <code>null</code>.
	 */
	public static void deleteEnd(final StringBuilder stringBuilder, final int count)
	{
		final int length = stringBuilder.length(); //get the length of the string builder
		stringBuilder.delete(length - count, length); //remove the last character
	}

	/**
	 * Escapes the indicated characters in the string builder using the supplied escape character. All characters are first encoded using UTF-8. Every invalid
	 * character is converted to its Unicode hex equivalent and prefixed with the given escape character. Characters are assumed to be valid unless specified
	 * otherwise. The escape character, if encountered, is not escaped unless it specifically meets one of the specified criteria; this allows re-escaping strings
	 * that may contain escape characters produced under less-strict rules (e.g. a URI containing escaped restricted characters, but still containing non-ASCII
	 * characters).
	 * @param stringBuilder The data to escape.
	 * @param validCharacters The characters that should not be escaped and all others should be escaped, or <code>null</code> if characters should not be matched
	 *          against valid characters.
	 * @param invalidCharacters The characters that, if they appear, should be escaped, or <code>null</code> if characters should not be matched against invalid
	 *          characters.
	 * @param maxCharacter The character value that represents the highest non-escaped value.
	 * @param escapeChar The character to prefix the hex representation.
	 * @param escapeLength The number of characters to use for the hex representation.
	 * @param hexCase Whether the hex characters should be lowercase or uppercase.
	 * @return The string builder, now containing the escaped data.
	 * @throws IllegalArgumentException if neither valid nor invalid characters are given.
	 */
	public static StringBuilder escapeHex(final StringBuilder stringBuilder, final Characters validCharacters, final Characters invalidCharacters,
			final int maxCharacter, final char escapeChar, final int escapeLength, final Case hexCase)
	{
		return escapeHex(stringBuilder, 0, stringBuilder.length(), validCharacters, invalidCharacters, maxCharacter, escapeChar, escapeLength, hexCase); //start at the beginning
	}

	/**
	 * Escapes the indicated characters in the string builder using the supplied escape character. All characters are first encoded using UTF-8. Every invalid
	 * character is converted to its Unicode hex equivalent and prefixed with the given escape character. Characters are assumed to be valid unless specified
	 * otherwise. The escape character, if encountered, is not escaped unless it specifically meets one of the specified criteria; this allows re-escaping strings
	 * that may contain escape characters produced under less-strict rules (e.g. a URI containing escaped restricted characters, but still containing non-ASCII
	 * characters).
	 * @param stringBuilder The data to escape.
	 * @param start The starting index to escape, inclusive; escaping will continue to the end of the string.
	 * @param validCharacters The characters that should not be escaped and all others should be escaped, or <code>null</code> if characters should not be matched
	 *          against valid characters.
	 * @param invalidCharacters The characters that, if they appear, should be escaped, or <code>null</code> if characters should not be matched against invalid
	 *          characters.
	 * @param maxCharacter The character value that represents the highest non-escaped value.
	 * @param escapeChar The character to prefix the hex representation.
	 * @param escapeLength The number of characters to use for the hex representation.
	 * @param hexCase Whether the hex characters should be lowercase or uppercase.
	 * @return The string builder, now containing the escaped data.
	 * @throws IllegalArgumentException if neither valid nor invalid characters are given.
	 */
	public static StringBuilder escapeHex(final StringBuilder stringBuilder, final int start, final Characters validCharacters,
			final Characters invalidCharacters, final int maxCharacter, final char escapeChar, final int escapeLength, final Case hexCase) //go to the end
	{
		return escapeHex(stringBuilder, start, stringBuilder.length(), validCharacters, invalidCharacters, maxCharacter, escapeChar, escapeLength, hexCase);
	}

	/**
	 * Escapes the indicated characters in the string builder using the supplied escape character. All characters are first encoded using UTF-8. Every invalid
	 * character is converted to its Unicode hex equivalent and prefixed with the given escape character. Characters are assumed to be valid unless specified
	 * otherwise. The escape character, if encountered, is not escaped unless it specifically meets one of the specified criteria; this allows re-escaping strings
	 * that may contain escape characters produced under less-strict rules (e.g. a URI containing escaped restricted characters, but still containing non-ASCII
	 * characters).
	 * @param stringBuilder The data to escape.
	 * @param start The starting index to escape, inclusive.
	 * @param end The ending index to escape, exclusive.
	 * @param validCharacters The characters that should not be escaped and all others should be escaped, or <code>null</code> if characters should not be matched
	 *          against valid characters.
	 * @param invalidCharacters The characters that, if they appear, should be escaped, or <code>null</code> if characters should not be matched against invalid
	 *          characters.
	 * @param maxCharacter The character value that represents the highest non-escaped value.
	 * @param escapeChar The character to prefix the hex representation.
	 * @param escapeLength The number of characters to use for the hex representation.
	 * @param hexCase Whether the hex characters should be lowercase or uppercase.
	 * @return The string builder, now containing the escaped data.
	 * @throws IllegalArgumentException if neither valid nor invalid characters are given.
	 */
	public static StringBuilder escapeHex(final StringBuilder stringBuilder, final int start, final int end, final Characters validCharacters,
			final Characters invalidCharacters, final int maxCharacter, final char escapeChar, final int escapeLength, final Case hexCase)
	{
		for(int characterIndex = end - 1; characterIndex >= start; --characterIndex) //work backwards; this keeps us from having a separate variable for the length, but it also makes it simpler to calculate the next position when we swap out characters
		{
			final char c = stringBuilder.charAt(characterIndex); //get the current character
			final boolean encode = (validCharacters != null && !validCharacters.contains(c)) //encode if there is a list of valid characters and this character is not one of them
					|| (invalidCharacters != null && invalidCharacters.contains(c)) //encode if there is a list of invalid characters and this character is one of them
					|| (c > maxCharacter); //encode the character if it is past the given upper bound
			if(encode) //if this a character to escape
			{
				escapeHex(stringBuilder, characterIndex, c, escapeChar, escapeLength, hexCase); //escape this character in the sequence
			}
		}
		return stringBuilder; //return the encoded version of the string
	}

	/**
	 * Escapes the indicated character in the string builder using the supplied escape character. All characters are first encoded using UTF-8. The character is
	 * converted to its Unicode hex equivalent and prefixed with the given escape character. This method uses <em>lowercase</em> hexadecimal escape codes.
	 * @param stringBuilder The string builder containing data to escape.
	 * @param index The index of the character to escape.
	 * @param escapeChar The character to prefix the hex representation.
	 * @param escapeLength The number of characters to use for the hex representation.
	 * @param hexCase Whether the hex characters should be lowercase or uppercase.
	 * @return A string representing the escaped data that was used as a replacement for the character.
	 * @throws StringIndexOutOfBoundsException if the given index does not represent a valid location in the string builder.
	 */
	public static String escapeHex(final StringBuilder stringBuilder, final int index, final char escapeChar, final int escapeLength)
	{
		return escapeHex(stringBuilder, index, escapeChar, escapeLength, Case.LOWERCASE);
	}

	/**
	 * Escapes the indicated character in the string builder using the supplied escape character. All characters are first encoded using UTF-8. The character is
	 * converted to its Unicode hex equivalent and prefixed with the given escape character.
	 * @param stringBuilder The string builder containing data to escape.
	 * @param index The index of the character to escape.
	 * @param escapeChar The character to prefix the hex representation.
	 * @param escapeLength The number of characters to use for the hex representation.
	 * @param hexCase Whether the hex characters should be lowercase or uppercase.
	 * @return A string representing the escaped data that was used as a replacement for the character.
	 * @throws StringIndexOutOfBoundsException if the given index does not represent a valid location in the string builder.
	 */
	public static String escapeHex(final StringBuilder stringBuilder, final int index, final char escapeChar, final int escapeLength, final Case hexCase)
	{
		return escapeHex(stringBuilder, index, stringBuilder.charAt(index), escapeChar, escapeLength, hexCase);
	}

	/**
	 * Escapes the indicated character in the string builder using the supplied escape character. All characters are first encoded using UTF-8. The character is
	 * converted to its Unicode hex equivalent and prefixed with the given escape character.
	 * @param stringBuilder The string builder containing data to escape.
	 * @param index The index of the character to replace.
	 * @param c The character to be escaped.
	 * @param escapeChar The character to prefix the hex representation.
	 * @param escapeLength The number of characters to use for the hex representation.
	 * @param hexCase Whether the hex characters should be lowercase or uppercase.
	 * @return A string representing the escaped data that was used as a replacement for the character.
	 * @throws StringIndexOutOfBoundsException if the given index does not represent a valid location in the string builder.
	 */
	public static String escapeHex(final StringBuilder stringBuilder, final int index, final char c, final char escapeChar, final int escapeLength,
			final Case hexCase)
	{
		final byte[] bytes = String.valueOf(c).getBytes(UTF_8_CHARSET); //convert this character to a sequence of UTF-8 bytes
		final int byteCount = bytes.length; //find out how many bytes there are
		final StringBuilder encodeStringBuilder = new StringBuilder(byteCount * 3); //create a string builder to hold three characters for each byte we have (the escape character plus a two-digit encoded value)
		for(int byteIndex = 0; byteIndex < byteCount; ++byteIndex) //look at each byte
		{
			encodeStringBuilder.append(escapeChar); //escape character
			encodeStringBuilder.append(Integers.toHexString(bytes[byteIndex], escapeLength, hexCase)); //hh or HH
		}
		final String escapeString = encodeStringBuilder.toString(); //encode the characters
		stringBuilder.replace(index, index + 1, escapeString); //replace the character with its encoding
		return escapeString; //return the escape string
	}

	/**
	 * Unescapes a value int a string builder using the provided escape character. Every instance of the escape character will be removed if followed by another
	 * character and the subsequent character will be ignored.
	 * @param stringBuilder The string builder to unescape.
	 * @return The string builder with the unescaped content.
	 * @throws NullPointerException if the given string builder is <code>null</code>.
	 * @throws IllegalArgumentException if the string builder ends with the given escape character.
	 */
	public static StringBuilder unescape(final StringBuilder stringBuilder, final char escapeChar)
	{
		if(endsWith(stringBuilder, escapeChar)) //if the string builder ends with the escape character (check now so that we don't have to check with each match)
		{
			throw new IllegalArgumentException("String builder  must not end with the escape character.");
		}
		int matchIndex = indexOf(stringBuilder, escapeChar, 0); //find the first match character
		while(matchIndex >= 0) //while there are more matches of the escape character (we know that this isn't the last character in the string, because we already checked to make sure the string doesn't end with the escape character)
		{
			stringBuilder.deleteCharAt(matchIndex); //delete the escape character
			matchIndex = indexOf(stringBuilder, escapeChar, matchIndex + 1); //find the next match character, ignoring the following character which is now at the current match position
		}
		return stringBuilder; //return the string builder
	}

	/**
	 * Appends a character sequence, ensuring that the given character sequence is the correct length by adding or deleting characters to or from the end.
	 * @param stringBuilder The string builder to which the character sequence should be added.
	 * @param charSequence The character sequence to add.
	 * @param forceLength The requested length.
	 * @param character The character to be added to the character sequence, if needed.
	 * @return A character sequence that is the requested number of characters longer.
	 */
	public static StringBuilder appendForceLength(final StringBuilder stringBuilder, final CharSequence charSequence, final int forceLength, final char character)
	{
		return appendForceLength(stringBuilder, charSequence, forceLength, character, -1);
	}

	/**
	 * Appends a character sequence, ensuring that the given character sequence is the correct length by adding or deleting characters to or from the given
	 * position.
	 * @param stringBuilder The string builder to which the character sequence should be added.
	 * @param charSequence The character sequence to add.
	 * @param forceLength The requested length.
	 * @param character The character to be added to the character sequence, if needed.
	 * @param position The position at which to insert or delete characters, or -1 if the end should be used.
	 * @return A character sequence that is the requested number of characters longer.
	 */
	public static StringBuilder appendForceLength(final StringBuilder stringBuilder, final CharSequence charSequence, final int forceLength,
			final char character, int position)
	{
		final int originalLength = charSequence.length(); //get the length of the original string
		if(originalLength == forceLength) //if the string is already the correct length
		{
			stringBuilder.append(charSequence); //append it as it is
		}
		else
		//if we need to make changes
		{
			if(position == -1) //if they want to insert/delete characters at the end of the string
			{
				position = originalLength; //find that position
			}
			final int removeEnd;
			if(originalLength > forceLength) //if the string is too long
			{
				final int removeCount = originalLength - forceLength; //find out how many characters to remove
				if(position > originalLength - removeCount) //if our position is too close to the end to successfully remove all characters
				{
					position = originalLength - removeCount; //place our position at just the right place to remove the characters
				}
				removeEnd = position + removeCount; //remove the required number of characters
			}
			else
			//if the string is too short
			{
				removeEnd = position; //there is nothing to remove
			}
			if(position > 0) //append the first part, if any
			{
				stringBuilder.append(charSequence, 0, position);
			}
			if(removeEnd == position) //if nothing is being removed, then we must be adding something
			{
				append(stringBuilder, character, forceLength - originalLength); //append the needed padding characters
			}
			if(removeEnd < originalLength) //if there are remaining characters
			{
				stringBuilder.append(charSequence, removeEnd, originalLength);
			}
		}
		return stringBuilder;
	}

	/**
	 * Returns the index of the first occurrence of the given character in the string buffer.
	 * @param stringBuilder The string buffer to search.
	 * @param c The character to look for.
	 * @return The index in the string buffer of the given character, or -1 if no occurrence was found.
	 */
	public static int indexOf(final StringBuilder stringBuilder, final char c)
	{
		return indexOf(stringBuilder, c, 0); //start searching from the beginning
	}

	/**
	 * Returns the index of the first occurrence of the given character in the string buffer from the given index.
	 * @param stringBuilder The string buffer to search.
	 * @param c The character to look for.
	 * @param fromIndex The index at which to start the search.
	 * @return The index in the string buffer of the given character, or -1 if no occurrence was found.
	 */
	public static int indexOf(final StringBuilder stringBuilder, final char c, final int fromIndex) //TODO optimize; this is very inefficient
	{
		final int length = stringBuilder.length(); //see how many characters are in the string buffer
		for(int i = fromIndex; i < length; ++i) //look at each character in the string buffer
		{
			if(stringBuilder.charAt(i) == c) //if this character matches the supplied character
				return i; //return the index at which the character occurs
		}
		return -1; //show that the character was not found
	}

	/**
	 * Searches a string buffer and returns the first index of any character <em>not</em> in the specified characters, starting from the beginning.
	 * @param stringBuilder The string buffer to be searched.
	 * @param notCharCharacters The characters to check.
	 * @return The index of the first occurrence of one of the supplied characters, or -1 if none were found.
	 */
	public static int notCharIndexOf(final StringBuilder stringBuilder, final Characters notCharCharacters)
	{
		return notCharIndexOf(stringBuilder, notCharCharacters, 0); //start looking from the beginning
	}

	/**
	 * Returns the index of the first non-occurrence of the given character in the string buffer.
	 * @param stringBuilder The string buffer to search.
	 * @param c The character to look for.
	 * @return The index in the string buffer of anything but the given character, or -1 if no non-occurrence was found.
	 */
	public static int indexNotOf(final StringBuilder stringBuilder, final char c)
	{
		return indexNotOf(stringBuilder, c, 0); //start searching from the beginning
	}

	/**
	 * Returns the index of the first non-occurrence of the given character in the string buffer from the given index.
	 * @param stringBuilder The string buffer to search.
	 * @param c The character to look for.
	 * @param fromIndex The index at which to start the search.
	 * @return The index in the string buffer of anything but the given character, or -1 if no non-occurrence was found.
	 */
	public static int indexNotOf(final StringBuilder stringBuilder, final char c, final int fromIndex)
	{
		final int length = stringBuilder.length(); //see how many characters are in the string buffer
		for(int i = fromIndex; i < length; ++i) //look at each character in the string buffer
		{
			if(stringBuilder.charAt(i) != c) //if this character doesn't match the supplied character
				return i; //return the index at which the character occurs
		}
		return -1; //show that the character was not found
	}

	/**
	 * Searches a string buffer and returns the first index of any character <em>not</em> in the specified characters, starting at <code>fromIndex</code>.
	 * @param stringBuilder The string buffer to be searched.
	 * @param notCharacters The characters to check.
	 * @param fromIndex The index to search from
	 * @return The index of the first occurrence of one of the supplied characters, or -1 if none were found.
	 */
	public static int notCharIndexOf(final StringBuilder stringBuilder, final Characters notCharacters, final int fromIndex)
	{
		for(int i = fromIndex; i < stringBuilder.length(); ++i) //look at each character in the string
		{
			if(!notCharacters.contains(stringBuilder.charAt(i))) //if this character is not in our character string
				return i; //return the index we're at
		}
		return -1; //if we make it to here, we didn't find any characters which weren't in our character string
	}

	/**
	 * Removes all content after and including the first occurrence of a character appearing in the delimiter string.
	 * @param stringBuilder The characters to examine.
	 * @param delimiters The characters to search for.
	 * @return The string buffer after removal.
	 */
	public static StringBuilder removeFirstCharLength(final StringBuilder stringBuilder, final Characters delimiters)
	{
		final int index = CharSequences.charIndexOf(stringBuilder, delimiters); //find the first occurrence of the delimiters
		if(index >= 0) //if one of the delimiters was found
			stringBuilder.delete(index, stringBuilder.length()); //remove everything after and including the character
		return stringBuilder; //return the string buffer
	}

	/**
	 * Removes every occurrence of a specified character.
	 * @param stringBuilder The string buffer from which the information will be removed.
	 * @param removeChar The character to remove from the string buffer.
	 * @return The string buffer after removal.
	 */
	public static StringBuilder removeEvery(final StringBuilder stringBuilder, final char removeChar)
	{
		for(int i = stringBuilder.length() - 1; i >= 0; --i) //look at each character, from the last to the first
		{
			if(stringBuilder.charAt(i) == removeChar) //if we have found a character to remove
			{
				stringBuilder.deleteCharAt(i); //delete this character
			}
		}
		return stringBuilder; //return the string buffer
	}

	/**
	 * Removes every occurrence of any of the given specified characters.
	 * @param stringBuilder The string buffer from which the information will be removed.
	 * @param removeChars The characters to be removed from the string buffer.
	 * @return The string buffer after removal.
	 */
	public static StringBuilder removeEveryChar(final StringBuilder stringBuilder, final String removeChars)
	{
		for(int i = stringBuilder.length() - 1; i >= 0; --i) //look at each character, from the last to the first
		{
			if(removeChars.indexOf(stringBuilder.charAt(i)) >= 0) //if this character is one of our remove characters
			{
				stringBuilder.deleteCharAt(i); //delete this character
			}
		}
		return stringBuilder; //return the string buffer
	}

	/**
	 * Converts all characters in the string buffer to corresponding characters in a given lookup table. Any characters not in the table will remain unchanged.
	 * @param stringBuilder The string buffer in which characters should be replaced.
	 * @param conversionTable A map of replacement characters each stored at the index of the matching chararacter (e.g. character code <em>x</em> will be
	 *          replaced by <code>characterTable[<em>x</em>]</code>).
	 * @return The number of replacements made.
	 */
	public static int replace(final StringBuilder stringBuilder, final char[] conversionTable)
	{
		int replacementCount = 0; //show that we have not yet made any replacements
		final int conversionTableLength = conversionTable.length; //find out how many characters we recognize
		//TODO del		final int stringBuilderLength=stringBuilder.length(); //find out how many characters there are to convert
		for(int i = stringBuilder.length() - 1; i >= 0; --i) //look at each character in the string buffer
		{
			final char c = stringBuilder.charAt(i); //get the code of the character candidate for replacement
			if(c < conversionTableLength) //if this character  has a replacement
			{
				stringBuilder.setCharAt(i, conversionTable[c]); //replace the character with the one at the index in the lookup table of its character code
				++replacementCount; //show that we replaced another character
			}
		}
		return replacementCount; //show how many characters we replaced
	}

	/**
	 * Replaces each matching character with a given replacement.
	 * @param stringBuilder The buffer in which the replacements will be made.
	 * @param matchReplaceSetArray An array of two-character arrays, each of the latter representing a set of characters, the first being a match character and
	 *          the second a replacement character.
	 * @return The number of replacements made.
	 */
	public static int replace(final StringBuilder stringBuilder, final char[][] matchReplaceSetArray)
	{
		int replacementCount = 0; //show that we have not replaced any characters, yet
		for(int matchSetIndex = 0; matchSetIndex < matchReplaceSetArray.length; ++matchSetIndex) //look at every set of match/replace pairs
		{
			final char matchChar = matchReplaceSetArray[matchSetIndex][0]; //get the match chararacter
			final char replacementChar = matchReplaceSetArray[matchSetIndex][1]; //get the replacement chararacter
			replacementCount += replace(stringBuilder, matchChar, replacementChar); //replace every occurrence of this character in the original string, and update our record of how many replacements were made
		}
		return replacementCount; //show how many characters we replaced
	}

	/**
	 * Replaces each matching character with the given replacement character.
	 * @param stringBuilder The buffer in which the replacements will be made.
	 * @param matchChar The character to be replaced.
	 * @param replacementChar The character for replacing the match character.
	 * @return The number of replacements made.
	 */
	public static int replace(final StringBuilder stringBuilder, final char matchChar, final char replacementChar)
	{
		int replacementCount = 0; //show that we have not replaced any characters, yet
		final int length = stringBuilder.length(); //get the length of the string buffer
		for(int i = 0; i < length; ++i) //look at each character in the string buffer
		{
			if(matchChar == stringBuilder.charAt(i)) //if this character matches
			{
				stringBuilder.setCharAt(i, replacementChar); //replace the original character with its replacement
				++replacementCount; //show that we replaced a character
			}
		}
		return replacementCount; //show how many characters we replaced
	}

	/**
	 * Replaces each matching character with the given replacement string.
	 * @param stringBuilder The buffer in which the replacements will be made.
	 * @param matchChar The character to be replaced.
	 * @param replacementString The string for replacing the match character.
	 * @return The number of replacements made.
	 */
	public static int replace(final StringBuilder stringBuilder, final char matchChar, final String replacementString)
	{
		int replacementCount = 0; //show that we have not replaced any characters, yet
		int beginSearchIndex = 0; //we'll start searching from the beginning of the string
		int nextReplaceIndex; //this will hold the next location of the replaceString each time we search for it
		while(beginSearchIndex < stringBuilder.length()) //while we haven't examined all the characters
		{
			nextReplaceIndex = indexOf(stringBuilder, matchChar, beginSearchIndex); //search for another occurrence of the character
			if(nextReplaceIndex >= 0) //if there is another occurrence of the character to replace
			{
				stringBuilder.replace(nextReplaceIndex, nextReplaceIndex + 1, replacementString); //replace this character with the replacement string
				beginSearchIndex = nextReplaceIndex + replacementString.length(); //skip over the string we replaced in the input string
				++replacementCount; //show that we replaced a character
			}
			else
			//if there are no more occurrences of the string to replace
			{
				break; //stop searching for more matches
			}
		}
		return replacementCount; //show how many characters we replaced
	}

	/**
	 * Replaces each matching character with the given replacement.
	 * @param stringBuilder The buffer in which the replacements will be made.
	 * @param matchCharacters The characters, each of which will to be replaced.
	 * @param replacementChar The character for replacing the match characters.
	 * @return The number of replacements made.
	 */
	public static int replace(final StringBuilder stringBuilder, final Characters matchCharacters, final char replacementChar)
	{
		int replacementCount = 0; //show that we have not replaced any characters, yet
		final int length = stringBuilder.length(); //get the length of the string buffer
		for(int i = 0; i < length; ++i) //look at each character in the string buffer
		{
			if(matchCharacters.contains(stringBuilder.charAt(i))) //if this character matches one of the match characters
			{
				stringBuilder.setCharAt(i, replacementChar); //replace the original character with its replacement
				++replacementCount; //show that we replaced a character
			}
		}
		return replacementCount; //show how many characters we replaced
	}

	/**
	 * Replaces each matching character with the given replacement string.
	 * @param stringBuilder The buffer in which the replacements will be made.
	 * @param matchCharacters The characters, each of which will to be replaced.
	 * @param replacementString The string for replacing the match characters.
	 * @return The number of replacements made.
	 */
	public static int replace(final StringBuilder stringBuilder, final Characters matchCharacters, final String replacementString)
	{
		int replacementCount = 0; //show that we have not replaced any characters, yet
		int beginSearchIndex = 0; //we'll start searching from the beginning of the string
		int nextReplaceIndex; //this will hold the next location of the replaceString each time we search for it
		while(beginSearchIndex < stringBuilder.length()) //while we haven't examined all the characters
		{
			nextReplaceIndex = charIndexOf(stringBuilder, matchCharacters, beginSearchIndex); //search for another occurrence of the characters
			if(nextReplaceIndex >= 0) //if there is another occurrence of the character to replace
			{
				stringBuilder.replace(nextReplaceIndex, nextReplaceIndex + 1, replacementString); //replace this character with the replacement string
				beginSearchIndex = nextReplaceIndex + replacementString.length(); //skip over the string we replaced in the input string
				++replacementCount; //show that we replaced a character
			}
			else
			//if there are no more occurrences of the string to replace
			{
				break; //stop searching for more matches
			}
		}
		return replacementCount; //show how many characters we replaced
	}

	/**
	 * Replaces each matching character with the corresponding replacement string.
	 * @param stringBuilder The buffer in which the replacements will be made.
	 * @param matchChars An array of characters to be replaced.
	 * @param replacementStrings An array of strings to replace the characters appearing at the same indexes as those in <var>matchChars</var>.
	 * @return The number of replacements made.
	 */
	public static int replace(final StringBuilder stringBuilder, final char[] matchChars, final String[] replacementStrings)
	{
		final int matchCount = matchChars.length; //find out how many match characters there are
		//we can optimize the process by finding the range of match characters up front, assuming that most characters will not be in the matching character range
		char minChar = Character.MAX_VALUE; //we'll find the lowest match character 
		char maxChar = Character.MIN_VALUE; //we'll find the highest match character 
		for(int matchIndex = matchCount - 1; matchIndex >= 0; --matchIndex) //look at each of the characters to match
		{
			final char matchChar = matchChars[matchIndex]; //get this match character
			if(matchChar < minChar) //if this is a lower character that the lowest
			{
				minChar = matchChar; //this character is the new lowest
			}
			if(matchChar > maxChar) //if this is a higher character that the highest
			{
				maxChar = matchChar; //this character is the new highest
			}
		}
		int replacementCount = 0; //show that we have not replaced any characters, yet
		for(int charIndex = 0; charIndex < stringBuilder.length(); ++charIndex) //look at each character in the string buffer
		{
			final char c = stringBuilder.charAt(charIndex); //get this character
			if(c >= minChar && c <= maxChar) //if this character is within the match range, see if it actually matches
			{
				for(int matchIndex = matchCount - 1; matchIndex >= 0; --matchIndex) //look at each of the characters to match
				{
					if(c == matchChars[matchIndex]) //if the character matches this match character
					{
						//replace this character with the replacement string
						stringBuilder.replace(charIndex, charIndex + 1, replacementStrings[matchIndex]);
						charIndex += replacementStrings[matchIndex].length() - 1; //skip to the last character of the replaced string
						++replacementCount; //show that we replaced a character
						break; //stop looking for matches for this character, and go on to the next character
					}
				}
			}
		}
		return replacementCount; //show how many characters we replaced
	}

	/**
	 * Replaces every occurrence of a specified string with another string.
	 * @param stringBuilder The string buffer in which the information will be replaced.
	 * @param replaceString The string to replace.
	 * @param withString The string that will replace replaceString.
	 * @return The number of replacements made.
	 */
	static public int replace(final StringBuilder stringBuilder, final String replaceString, final String withString)
	{
		int replacementCount = 0; //show that we have not replaced any characters, yet
		int beginSearchIndex = 0; //we'll start searching from the beginning of the string
		int nextReplaceIndex; //this will hold the next location of the replaceString each time we search for it
		while(beginSearchIndex < stringBuilder.length()) //while we haven't examined all the characters
		{
			nextReplaceIndex = stringBuilder.indexOf(replaceString, beginSearchIndex); //search for another occurrence of the string
			if(nextReplaceIndex >= 0) //if there is another occurrence of the string to replace
			{
				stringBuilder.replace(nextReplaceIndex, nextReplaceIndex + replaceString.length(), withString); //replace this string with the replacement string
				beginSearchIndex = nextReplaceIndex + replaceString.length(); //skip over the string we replaced in the input string
				++replacementCount; //show that we replaced a character
			}
			else
			//if there are no more occurrences of the string to replace
			{
				break; //stop searching for more matches
			}
		}
		return replacementCount; //show how many characters we replaced
	}

	/**
	 * Replaces all runs of the given character with another character. A run is subsequent repeated characters. If a run is shorter than
	 * <code>minRunLength</code> or longer than <code>maxRunLength</code>, it will be ignored.
	 * @param stringBuilder The string buffer to search.
	 * @param matchChar The char runs of which to replace.
	 * @param minRunLength The minimum number of subsequent characters before replacement.
	 * @param maxRunLength The maximum number of subsequent characters before replacement.
	 * @param replaceChar The character that will replace all matching runs.
	 * @return The string buffer with all matching character runs replaced.
	 */
	public static StringBuilder replaceRuns(final StringBuilder stringBuilder, final char matchChar, final int minRunLength, final int maxRunLength,
			final char replaceChar)
	{
		int nextIndex = 0; //start at the beginning of the string
		while(nextIndex < stringBuilder.length()) //keep going until we reach the end of the string buffer
		{
			nextIndex = indexOf(stringBuilder, matchChar, nextIndex); //find the next character match
			if(nextIndex >= 0) //if we found the character
			{
				int runEndIndex = nextIndex + 1; //we'll use this variable to find the end of the run
				//advance the runEndIndex until we run out of string buffer or find a non-matching character
				while(runEndIndex < stringBuilder.length() && stringBuilder.charAt(runEndIndex) == matchChar)
					++runEndIndex; //keep looking for a non-matching character
				final int runLength = runEndIndex - nextIndex; //find the length of the run
				if(runLength >= minRunLength && runLength <= maxRunLength) //if this run size is in our range
				{
					replace(stringBuilder, nextIndex, runEndIndex, replaceChar); //replace the run with the given character
					++nextIndex; //skip the character that replaced the run
				}
				else
				//if the run size doesn't meet our criteria
				{
					nextIndex = runEndIndex; //skip the run
				}
			}
			else
				//if the character doesn't appear any more in the string
				break; //stop replacing
		}
		return stringBuilder; //return the string buffer
	}

	/**
	 * Removes several characters at the specified index and replaces them with the given character. This duplicates <code>StringBuilder.replace</code>
	 * functionality without the overhead of a string.
	 * @param stringBuilder The buffer in which the information will be replaced.
	 * @param startIndex The index of the information to remove.
	 * @param endIndex One character past the last of the information to remove; must be greater than <code>startIndex</code>.
	 * @param replaceChar The character to replace the removed characters.
	 * @return The string buffer with the specified contents replaced.
	 * @see StringBuilder#replace
	 */
	public static StringBuilder replace(final StringBuilder stringBuilder, final int startIndex, final int endIndex, final char replaceChar)
	{
		stringBuilder.setCharAt(startIndex, replaceChar); //replace the character at the starting index
		if(endIndex - startIndex > 1) //if we're replacing more than one character
			stringBuilder.delete(startIndex + 1, endIndex); //delete evertything after the starting character up to the end of our replacement section
		return stringBuilder; //return the string buffer
	}

	/**
	 * Determines if the string buffer starts with the given string.
	 * @param stringBuilder The string buffer to examine.
	 * @param string The string to compare.
	 * @return <code>true</code> if the first characters of the string buffer match those of the given string.
	 */
	public static boolean startsWith(final StringBuilder stringBuilder, final String string)
	{
		if(string.length() > stringBuilder.length()) //if the substring is too long
			return false; //the substring is too big to start the string buffer
		for(int i = string.length() - 1; i >= 0; --i) //look at each character of the string
		{
			if(string.charAt(i) != stringBuilder.charAt(i)) //if these characters don't match in the same position
				return false; //the string doesn't match
		}
		return true; //the string buffer starts with the string
	}

	/**
	 * Trims the specified delimiters from the beginning and end of the string buffer.
	 * @param stringBuilder The characters to be processed.
	 * @param delimiters The delimiter characters.
	 * @return The trimmed string buffer.
	 */
	public static StringBuilder trim(final StringBuilder stringBuilder, final Characters delimiters)
	{
		trimBeginning(stringBuilder, delimiters); //trim the beginning of the string builder
		trimEnd(stringBuilder, delimiters); //trim the end of the string builder
		return stringBuilder; //return the resulting string buffer
	}

	/**
	 * Trims the specified delimiters from the beginning of the string buffer.
	 * @param stringBuilder The characters to be processed.
	 * @param delimiters The delimiter characters.
	 * @return The trimmed string buffer.
	 */
	public static StringBuilder trimBeginning(final StringBuilder stringBuilder, final Characters delimiters)
	{
		int beginIndex;
		final int length = stringBuilder.length(); //get the length of the original string
		for(beginIndex = 0; beginIndex < length && delimiters.contains(stringBuilder.charAt(beginIndex)); ++beginIndex)
			; //find the first non-delimiter in the string buffer
		if(beginIndex > 0) //if we should trim the beginning
			stringBuilder.delete(0, beginIndex); //remove the beginning of the buffer
		return stringBuilder; //return the resulting string buffer
	}

	/**
	 * Trims the specified delimiter from the end of the string buffer.
	 * @param stringBuilder The characters to be processed.
	 * @param delimiter The delimiter character.
	 * @return The trimmed string buffer.
	 */
	public static StringBuilder trimEnd(final StringBuilder stringBuilder, final char delimiter)
	{
		int endIndex;
		final int length = stringBuilder.length(); //get the length of the original string
		for(endIndex = length; endIndex > 0 && stringBuilder.charAt(endIndex - 1) == delimiter; --endIndex)
			; //find the last non-delimiter in the string buffer
		if(endIndex < length) //if we should trim the end
			stringBuilder.delete(endIndex, length); //remove the end of the buffer
		return stringBuilder; //return the resulting string buffer
	}

	/**
	 * Trims the specified delimiters from the end of the string buffer.
	 * @param stringBuilder The characters to be processed.
	 * @param delimiters The delimiter characters.
	 * @return The trimmed string buffer.
	 */
	public static StringBuilder trimEnd(final StringBuilder stringBuilder, final Characters delimiters)
	{
		int endIndex;
		final int length = stringBuilder.length(); //get the length of the original string
		for(endIndex = length; endIndex > 0 && delimiters.contains(stringBuilder.charAt(endIndex - 1)); --endIndex)
			; //find the last non-delimiter in the string buffer
		if(endIndex < length) //if we should trim the end
			stringBuilder.delete(endIndex, length); //remove the end of the buffer
		return stringBuilder; //return the resulting string buffer
	}

}