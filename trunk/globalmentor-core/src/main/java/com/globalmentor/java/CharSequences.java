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

import static com.globalmentor.java.Characters.*;
import static com.globalmentor.io.Charsets.*;

import java.util.Collection;

import com.globalmentor.io.UTF8;
import com.globalmentor.text.Case;
import com.globalmentor.text.unicode.UnicodeCharacter;

/**
 * Various text manipulating functions. These methods work on objects that implement the {@link CharSequence} interface. To avoid creation of new strings, some
 * of these methods should be avoided in favor of their corresponding {@link StringBuilders} methods, which operate on {@link StringBuilder} objects.
 * @see StringBuilders
 * @author Garret Wilson
 */
public class CharSequences
{

	/**
	 * Checks the given bounds of a character sequence.
	 * @param charSequence The character sequence against which the bounds should be checked.
	 * @param start The start to check, inclusive.
	 * @param end The end to check, exclusive.
	 * @return The given character sequence.
	 * @throws StringIndexOutOfBoundsException if <code>start</code> or <code>end</code> is negative or greater than <code>length()</code>, or <code>start</code>
	 *           is greater than <code>end</code>.
	 */
	public static CharSequence checkBounds(final CharSequence charSequence, final int start, final int end)
	{
		if(start < 0)
		{
			throw new StringIndexOutOfBoundsException(start);
		}
		if(end > charSequence.length())
		{
			throw new StringIndexOutOfBoundsException(end);
		}
		if(start > end)
		{
			throw new StringIndexOutOfBoundsException(end - start);
		}
		return charSequence;
	}

	/**
	 * Searches a character sequence and returns the first index of any specified characters, starting at the beginning.
	 * @param charSequence The character sequence to be searched.
	 * @param characters The string of characters to check.
	 * @return The index of the first occurrence of one of the supplied characters, or -1 if none were found.
	 */
	public static int charIndexOf(final CharSequence charSequence, final Characters characters)
	{
		return charIndexOf(charSequence, characters, 0); //look of the characters, starting at the beginning of the string
	}

	/**
	 * Searches a character sequence and returns the first index of any specified characters, starting at the given index.
	 * @param charSequence The character sequence to be searched.
	 * @param characters The string of characters to check.
	 * @param index The index to search from.
	 * @return The index of the first occurrence of one of the supplied characters, or -1 if none were found.
	 */
	public static int charIndexOf(final CharSequence charSequence, final Characters characters, final int index)
	{
		for(int i = index; i < charSequence.length(); ++i) //look at each character in the sequence
		{
			if(characters.contains(charSequence.charAt(i))) //if this character is in our characters
				return i; //return the index we're at
		}
		return -1; //if we make it to here, we didn't find any of the characters
	}

	/**
	 * Searches a character sequence in reverse and returns the last index of any specified characters.
	 * @param charSequence The character sequence to be searched.
	 * @param characters The string of characters to check.
	 * @return The index of the last occurrence of one of the supplied characters, or -1 if none were found.
	 */
	public static int charLastIndexOf(final CharSequence charSequence, final Characters characters)
	{
		return charLastIndexOf(charSequence, characters, charSequence.length() - 1); //search the sequence, starting at the end
	}

	/**
	 * Searches a character sequence in reverse and returns the last index of any specified characters, starting from the given index.
	 * @param charSequence The character sequence to be searched.
	 * @param characters The string of characters to check.
	 * @param index The index to search from.
	 * @return The index of the last occurrence of one of the supplied characters, or -1 if none were found.
	 */
	public static int charLastIndexOf(final CharSequence charSequence, final Characters characters, final int index)
	{
		for(int i = index; i >= 0; --i) //look at each character in the sequence, starting at the end
		{
			if(characters.contains(charSequence.charAt(i))) //if this character is in our characters
				return i; //return the index we're at
		}
		return -1; //if we make it to here, we didn't find any of the characters
	}

	/**
	 * Ensures that the given character sequence has a minimum the specified number of characters.
	 * @param <T> The type of character sequence being used.
	 * @param charSequence The character sequence to check.
	 * @param minLength The minimum length required.
	 * @return The given character sequence.
	 * @exception NullPointerException if the given character sequence is <code>null</code>.
	 * @exception IllegalArgumentException if the length of the given character sequence is less than the indicated minimum length.
	 */
	public static <T extends CharSequence> T checkMinLength(final T charSequence, final int minLength)
	{
		if(charSequence.length() < minLength) //if the length of the given characters sequence is less than required
		{
			throw new IllegalArgumentException("Character sequence is not at least " + minLength + " characters long: " + charSequence);
		}
		return charSequence; //return the character sequence
	}

	/**
	 * Determines if a character sequence contains the given character.
	 * @param charSequence The character sequence to be searched.
	 * @param character The character to check.
	 * @return <code>true</code> if the given character sequence contains the given character.
	 */
	public static boolean contains(final CharSequence charSequence, final char character)
	{
		return indexOf(charSequence, character) >= 0; //see if the given character is in the character sequence
	}

	/**
	 * Determines if a character sequence contains any of the given characters.
	 * @param charSequence The character sequence to be searched.
	 * @param characters The characters to check.
	 * @return <code>true</code> if the given character sequence contains one of the given characters.
	 */
	public static boolean contains(final CharSequence charSequence, final Characters characters)
	{
		return indexOf(charSequence, characters) >= 0; //see if any of the given characters are in the character sequence
	}

	/**
	 * Determines if the following character sequence contains a letter.
	 * @param charSequence The character sequence to search.
	 * @return <code>true</code> if the sequence has at least one letter.
	 */
	public static boolean containsLetter(final CharSequence charSequence) //TODO maybe change this to indexOfLetterOrDigit
	{
		for(int i = charSequence.length() - 1; i >= 0; --i) //look at each character in the string
		{
			if(Character.isLetter(charSequence.charAt(i))) //if this is a letter
				return true; //we found a letter
		}
		return false; //we found no letters
	}

	/**
	 * Determines if the following character sequence contains a letter or a digit.
	 * @param charSequence The character sequence to search.
	 * @return <code>true</code> if the sequence has at least one letter or digit.
	 * @see Character#isLetterOrDigit(char)
	 */
	public static boolean containsLetterOrDigit(final CharSequence charSequence) //TODO maybe change this to indexOfLetterOrDigit
	{
		for(int i = charSequence.length() - 1; i >= 0; --i) //look at each character in the string
		{
			if(Character.isLetterOrDigit(charSequence.charAt(i))) //if this is a letter or digit
				return true; //we found a letter or digit
		}
		return false; //we found no letters or digits
	}

	/**
	 * Determines if a character sequence contains Unicode whitespace. Whitespace is denoted by the "WS" bidi class in <code>UnicodeData.txt</code>. This method
	 * does not handle Unicode supplementary characters.
	 * @param charSequence The character sequence to be searched.
	 * @return <code>true</code> if the given character sequence contains whitespace.
	 * @see Character#isSpaceChar(char)
	 */
	public static boolean containsWhitespace(final CharSequence charSequence)
	{
		for(int i = charSequence.length() - 1; i >= 0; --i) //look at each character in the string
		{
			if(Character.isSpaceChar(charSequence.charAt(i))) //if this is whitespace
			{
				return true; //we found whitespace
			}
		}
		return false; //we found no whitespace
	}

	/**
	 * Determines if a character sequence contains characters that are not Unicode whitespace (marked by "WS" in Unicode data). Whitespace is denoted by the "WS"
	 * bidi class in <code>UnicodeData.txt</code>. This method does not handle Unicode supplementary characters.
	 * @param charSequence The character sequence to be searched.
	 * @return <code>true</code> if the given character sequence contains non-whitespace.
	 * @see Character#isSpaceChar(char)
	 */
	public static boolean containsNonWhitespace(final CharSequence charSequence)
	{
		for(int i = charSequence.length() - 1; i >= 0; --i) //look at each character in the string
		{
			if(!Character.isSpaceChar(charSequence.charAt(i))) //if this is not whitespace
			{
				return true; //we found non-whitespace
			}
		}
		return false; //we found no non-whitespace
	}

	/**
	 * Searches the given character sequence for one of the given tokens, separated by the given delimiters.
	 * @param charSequence The character sequence to search.
	 * @param delimiters The delimiters to skip.
	 * @param tokens The tokens for which to check.
	 * @return The <code>true</code> if one of the given tokens was found.
	 * @throws NullPointerException if the given character sequence, delimiters, and/or tokens is <code>null</code>.
	 */
	public static boolean containsToken(final CharSequence charSequence, final Characters delimiters, final CharSequence... tokens)
	{
		return getToken(charSequence, delimiters, tokens) != null;
	}

	/**
	 * Searches the given character sequence for one of the given tokens, separated by the given delimiters.
	 * @param charSequence The character sequence to search.
	 * @param delimiters The delimiters to skip.
	 * @param tokens The tokens for which to check.
	 * @return The token that was found, or <code>null</code> if no token was found.
	 * @throws NullPointerException if the given character sequence, delimiters, and/or tokens is <code>null</code>.
	 */
	public static CharSequence getToken(final CharSequence charSequence, final Characters delimiters, final CharSequence... tokens)
	{
		final int length = charSequence.length();
		for(int i = 0; i < length; ++i) //look through the sequence
		{
			if(delimiters.contains(charSequence.charAt(i))) //skip delimiters
			{
				continue;
			}
			for(final CharSequence token : tokens) //look at each token
			{
				if(equals(token, charSequence, i, i + token.length())) //if this token equals the characters starting at the current position
				{
					return token;
				}
			}
		}
		return null;
	}

	/**
	 * Determines if a character sequence contains characters that can be trimmed. Trimmed characters are denoted by the "WS" (whitespace) bidi class, the "Cc"
	 * (control) category, or the "Cf" (format) category in <code>UnicodeData.txt</code>. This method does not handle Unicode supplementary characters.
	 * @param charSequence The character sequence to be searched.
	 * @return <code>true</code> if the given character sequence contains trim characters.
	 * @see Character#isSpaceChar(char)
	 * @see Character#isISOControl(char)
	 * @see Character#getType(char)
	 */
	public static boolean containsTrim(final CharSequence charSequence)
	{
		for(int i = charSequence.length() - 1; i >= 0; --i) //look at each character in the string
		{
			final char c = charSequence.charAt(i); //get the current character
			if(Character.isSpaceChar(c) || Character.isISOControl(c) || Character.getType(c) == Character.FORMAT) //if this is whitespace, control, or format
			{
				return true; //we found a trim character
			}
		}
		return false; //we found no trim character
	}

	/**
	 * Determines if a character sequence contains characters that are not characters that can be trimmed. This is useful for determining if a characer sequence
	 * actually contains useful data. Trimmed characters are denoted by the "WS" (whitespace) bidi class, the "Cc" (control) category, or the "Cf" (format)
	 * category in <code>UnicodeData.txt</code>. This method does not handle Unicode supplementary characters.
	 * @param charSequence The character sequence to be searched.
	 * @return <code>true</code> if the given character sequence contains non-trim characters.
	 * @see Character#isSpaceChar(char)
	 * @see Character#isISOControl(char)
	 * @see Character#getType(char)
	 */
	public static boolean containsNonTrim(final CharSequence charSequence)
	{
		for(int i = charSequence.length() - 1; i >= 0; --i) //look at each character in the string
		{
			final char c = charSequence.charAt(i); //get the current character
			if(!Character.isSpaceChar(c) && !Character.isISOControl(c) && Character.getType(c) != Character.FORMAT) //if this is not whitespace, control, or format
			{
				return true; //we found a non-trim character
			}
		}
		return false; //we found no non-trim character
	}

	/**
	 * Counts the number of occurrences of a particular character in a character sequence.
	 * @param charSequence The character sequence to examine.
	 * @param character The character to count.
	 * @return The number of occurrences of the character in the character sequence.
	 */
	public static int count(final CharSequence charSequence, final char character)
	{
		return count(charSequence, character, 0); //count, starting at the beginning
	}

	/**
	 * Counts the number of occurrences of a particular character in a character sequence, starting at a specified index and searching forward.
	 * @param charSequence The character sequence to examine.
	 * @param character The character to count.
	 * @param index The index to start counting at.
	 * @return The number of occurrences of the character in the character sequence.
	 */
	public static int count(final CharSequence charSequence, final char character, final int index)
	{
		final int length = charSequence.length();
		int count = 0; //start out without knowing any occurrences
		for(int i = index; i < length; ++i) //look at each character
		{
			if(charSequence.charAt(i) == character) //if this character matches the given characters
			{
				++count; //show that we found one more occurrence characters
			}
		}
		return count; //return the total count
	}

	/**
	 * Determines if the character sequence ends with the given character.
	 * @param charSequence The character sequence to examine.
	 * @param character The character to compare.
	 * @return <code>true</code> if the last character of the character sequence matches that of the given string.
	 */
	public static boolean endsWith(final CharSequence charSequence, final char character)
	{
		//see if the character sequence has at least one character, and the last character matches our character
		return charSequence.length() > 0 && charSequence.charAt(charSequence.length() - 1) == character;
	}

	/**
	 * Determines if the character sequence ends with the given string.
	 * @param charSequence The character sequence to examine.
	 * @param string The string to compare.
	 * @return <code>true</code> if the last characters of the character sequence match those of the given string.
	 */
	public static boolean endsWith(final CharSequence charSequence, final String string)
	{
		final int delta = charSequence.length() - string.length(); //find out the difference in length between the strings
		if(delta < 0) //if the substring is too long
			return false; //the substring is too big to start the character sequence
		for(int i = string.length() - 1; i >= 0; --i) //look at each character of the string
		{
			if(string.charAt(i) != charSequence.charAt(i + delta)) //if these characters don't match in the same position
				return false; //the string doens't match
		}
		return true; //the character sequence ends with the string
	}

	/**
	 * Determines if the character sequence ends with the given string without case sensitivity.
	 * @param charSequence The character sequence to examine.
	 * @param string The string to compare.
	 * @return <code>true</code> if the last characters of the character sequence match those of the given string, case insensitively.
	 */
	public static boolean endsWithIgnoreCase(final CharSequence charSequence, final String string)
	{
		final int delta = charSequence.length() - string.length(); //find out the difference in length between the strings
		if(delta < 0) //if the substring is too long
			return false; //the substring is too big to start the character sequence
		for(int i = string.length() - 1; i >= 0; --i) //look at each character of the string
		{
			if(Character.toUpperCase(string.charAt(i)) != Character.toUpperCase(charSequence.charAt(i + delta))) //if these characters don't match in the same position
				return false; //the string doens't match
		}
		return true; //the character sequence ends with the string
	}

	/**
	 * Escapes the indicated characters in the character sequence using the supplied escape character. All characters are first encoded using UTF-8. Every invalid
	 * character is converted to its Unicode hex equivalent and prefixed with the given escape character. This method uses <em>lowercase</em> hexadecimal escape
	 * codes. Characters are assumed to be valid unless specified otherwise. The escape character, if encountered, is not escaped unless it specifically meets one
	 * of the specified criteria; this allows re-escaping strings that may contain escape characters produced under less-strict rules (e.g. a URI containing
	 * escaped restricted characters, but still containing non-ASCII characters).
	 * @param charSequence The data to escape.
	 * @param validCharacters The characters that should not be escaped and all others should be escaped, or <code>null</code> if characters should not be matched
	 *          against valid characters.
	 * @param invalidCharacters The characters that, if they appear, should be escaped, or <code>null</code> if characters should not be matched against invalid
	 *          characters.
	 * @param maxCharacter The character value that represents the highest non-escaped value.
	 * @param escapeChar The character to prefix the hex representation.
	 * @param escapeLength The number of characters to use for the hex representation.
	 * @return A string containing the escaped data.
	 * @exception IllegalArgumentException if neither valid nor invalid characters are given.
	 */
	public static String escapeHex(final CharSequence charSequence, final Characters validCharacters, final Characters invalidCharacters, final int maxCharacter,
			final char escapeChar, final int escapeLength)
	{
		return escapeHex(charSequence, validCharacters, invalidCharacters, maxCharacter, escapeChar, escapeLength, Case.LOWERCASE); //default to lowercase
	}

	/**
	 * Escapes the indicated characters in the character sequence using the supplied escape character. All characters are first encoded using UTF-8. Every invalid
	 * character is converted to its Unicode hex equivalent and prefixed with the given escape character. Characters are assumed to be valid unless specified
	 * otherwise. The escape character, if encountered, is not escaped unless it specifically meets one of the specified criteria; this allows re-escaping strings
	 * that may contain escape characters produced under less-strict rules (e.g. a URI containing escaped restricted characters, but still containing non-ASCII
	 * characters).
	 * @param charSequence The data to escape.
	 * @param validCharacters The characters that should not be escaped and all others should be escaped, or <code>null</code> if characters should not be matched
	 *          against valid characters.
	 * @param invalidCharacters The characters that, if they appear, should be escaped, or <code>null</code> if characters should not be matched against invalid
	 *          characters.
	 * @param maxCharacter The character value that represents the highest non-escaped value.
	 * @param escapeChar The character to prefix the hex representation.
	 * @param escapeLength The number of characters to use for the hex representation.
	 * @param hexCase Whether the hex characters should be lowercase or uppercase.
	 * @return A string containing the escaped data.
	 * @exception IllegalArgumentException if neither valid nor invalid characters are given.
	 */
	public static String escapeHex(final CharSequence charSequence, final Characters validCharacters, final Characters invalidCharacters, final int maxCharacter,
			final char escapeChar, final int escapeLength, final Case hexCase)
	{
		//put the string in a string builder and escape it; although inserting encoded sequences may seem inefficient,
		//	it should be noted that filling a string buffer with the entire string is more efficient than doing it one character at a time,
		//	that characters needed encoding are generally uncommon, and that any copying of the string characters during insertion is done
		//	via a native method, which should happen very quickly
		return StringBuilders.escapeHex(new StringBuilder(charSequence), validCharacters, invalidCharacters, maxCharacter, escapeChar, escapeLength, hexCase)
				.toString();
	}

	/**
	 * Decodes the escaped characters in the character sequence by converting the hex value after each occurrence of the escape character to the corresponding
	 * Unicode character using UTF-8.
	 * <p>
	 * This implementation does not allow non-escaped UTF-8 characters that would be escaped by UTF-8.
	 * </p>
	 * @param charSequence The data to unescape.
	 * @param escapeChar The character that prefixes the hex representation.
	 * @param escapeLength The number of characters used for the hex representation.
	 * @return A string containing the unescaped data.
	 * @exception IllegalArgumentException if the given characters contains a character greater than U+007F.
	 * @exception IllegalArgumentException if a given escape character is not followed by an escape sequence.
	 */
	public static String unescapeHex(final CharSequence charSequence, final char escapeChar, final int escapeLength)
	{
		final int charSequenceLength = charSequence.length(); //get the length of the character sequence
		final byte[] decodedBytes = new byte[charSequenceLength]; //create an array of byte to hold the UTF-8 data
		int byteArrayIndex = 0; //start at the first position in the byte array
		for(int i = 0; i < charSequenceLength; ++i) //look at each character in the character sequence
		{
			final char c = charSequence.charAt(i); //get a reference to this character in the character sequence
			final byte b; //we'll determine what byte goes at this position
			if(c == escapeChar) //if this is the beginning of an escaped sequence
			{
				if(i < charSequenceLength - escapeLength) //if there's room for enough hex characters after it
				{
					final String escapeSequence = charSequence.subSequence(i + 1, i + escapeLength + 1).toString(); //get the hex characters in the escape sequence							
					try
					{
						b = (byte)Integer.parseInt(escapeSequence, 16); //convert the escape sequence to a single integer value and add it to the buffer
						i += 2; //skip the escape sequence (we'll go to the last character, and we'll be advanced one character when we go back to the start of the loop)
					}
					catch(NumberFormatException numberFormatException) //if the characters weren't really hex characters
					{
						throw new IllegalArgumentException("Invalid escape sequence " + escapeSequence + " at index " + i + " in character sequence \"" + charSequence
								+ "\".");
					}
				}
				else
				//if there is no room for an escape sequence at the end of the string
				{
					throw new IllegalArgumentException("Invalid escape sequence " + charSequence.subSequence(i + 1, charSequenceLength) + " at index " + i
							+ " in character sequence \"" + charSequence + "\".");
				}
			}
			else
			//if this is not an escaped character
			{
				if(c > UTF8.MAX_ENCODED_BYTE_COUNT1) //if this character is larger than the largest UTF-8 encoding for a single byte, the character sequence was not encoded correctly
				{
					throw new IllegalArgumentException("Invalid encoded character " + UnicodeCharacter.getCodePointString(c) + " at index " + i
							+ " in character sequence \"" + charSequence + "\".");
				}
				b = (byte)c; //add this character to the result with no change
			}
			decodedBytes[byteArrayIndex++] = b; //add the byte to the buffer and keep going
		}
		return new String(decodedBytes, 0, byteArrayIndex, UTF_8_CHARSET); //consider the bytes as a series of UTF-8 encoded characters.
	}

	/**
	 * Determines the first index of the given character.
	 * @param charSequence The character sequence to check.
	 * @param character The character to search for.
	 * @return The index of the first occurrence of the given character, or -1 if the character was not found.
	 */
	public static int indexOf(final CharSequence charSequence, final char character)
	{
		return indexOf(charSequence, character, 0); //search from the beginning
	}

	/**
	 * Determines the first index of the given character.
	 * @param charSequence The character sequence to check.
	 * @param character The character to search for.
	 * @return The index of the first occurrence of the given character, or the length of the character sequence if the character was not found.
	 */
	public static int indexOfLength(final CharSequence charSequence, final char character)
	{
		final int index = indexOf(charSequence, character); //perform the search
		return index >= 0 ? index : charSequence.length(); //return the length if we're out of characters
	}

	/**
	 * Determines the first index of the given character. If the character sequence is a {@link String}, this method delegates to {@link String#indexOf(int, int)}
	 * .
	 * @param charSequence The character sequence to check.
	 * @param character The character to search for.
	 * @param index The first index to examine.
	 * @return The index of the first occurrence of the given character, or -1 if the character was not found.
	 */
	public static int indexOf(final CharSequence charSequence, final char character, final int index)
	{
		if(charSequence instanceof String) //if the character sequence is a string
		{
			return ((String)charSequence).indexOf(character, index); //delegate to the String version, which is much more efficient
		}
		final int length = charSequence.length();
		for(int i = index; i < length; ++i) //look at each character
		{
			if(charSequence.charAt(i) == character) //if this character matches
			{
				return i; //return the matching index
			}
		}
		return -1; //show that we couldn't find the character
	}

	/**
	 * Determines the first index of the given character. If the character sequence is a {@link String}, this method delegates to {@link String#indexOf(int, int)}
	 * .
	 * @param charSequence The character sequence to check.
	 * @param character The character to search for.
	 * @param index The first index to examine.
	 * @return The index of the first occurrence of the given character, or the length of the character sequence if the character was not found.
	 */
	public static int indexOfLength(final CharSequence charSequence, final char character, int index)
	{
		index = indexOf(charSequence, character, index); //perform the search
		return index >= 0 ? index : charSequence.length(); //return the length if we're out of characters		
	}

	/**
	 * Searches a character sequence and returns the first index of any character of the given characters, starting at the beginning.
	 * @param charSequence The character sequence to be searched.
	 * @param characters The characters to check.
	 * @return The index of the first occurrence of one of the supplied characters, or -1 if none were found.
	 */
	public static int indexOf(final CharSequence charSequence, final Characters characters)
	{
		return indexOf(charSequence, characters, 0); //look of the characters, starting at the beginning of the string
	}

	/**
	 * Searches a character sequence and returns the first index of any character of the given characters, starting at the beginning.
	 * @param charSequence The character sequence to be searched.
	 * @param characters The characters to check.
	 * @return The index of the first occurrence of one of the supplied characters, or the length of the character sequence if none were found.
	 */
	public static int indexOfLength(final CharSequence charSequence, Characters characters)
	{
		final int index = indexOf(charSequence, characters); //perform the search
		return index >= 0 ? index : charSequence.length(); //return the length if we're out of characters
	}

	/**
	 * Searches a character sequence and returns the first index of any character of the given characters, starting at the given index.
	 * @param charSequence The character sequence to be searched.
	 * @param characters The characters to check.
	 * @param index The index to search from.
	 * @return The index of the first occurrence of one of the supplied characters, or -1 if none were found.
	 */
	public static int indexOf(final CharSequence charSequence, final Characters characters, final int index)
	{
		for(int i = index; i < charSequence.length(); ++i) //look at each character in the sequence
		{
			if(characters.contains(charSequence.charAt(i))) //if this character is in our character string
			{
				return i; //return the index we're at
			}
		}
		return -1; //if we make it to here, we didn't find any of the characters
	}

	/**
	 * Searches a character sequence and returns the first index of any character of the given characters, starting at the given index.
	 * @param charSequence The character sequence to be searched.
	 * @param characters The characters to check.
	 * @param index The index to search from.
	 * @return The index of the first occurrence of one of the supplied characters, or the length of the character sequence if none were found.
	 */
	public static int indexOfLength(final CharSequence charSequence, final Characters characters, int index)
	{
		index = indexOf(charSequence, characters, index); //perform the search
		return index >= 0 ? index : charSequence.length(); //return the length if we're out of characters
	}

	/**
	 * Determines the last index of the given character.
	 * @param charSequence The character sequence to check.
	 * @param character The character to search for.
	 * @return The index of the last occurrence of the given character, or -1 if the character was not found.
	 */
	public static int lastIndexOf(final CharSequence charSequence, final char character)
	{
		return lastIndexOf(charSequence, character, charSequence.length() - 1); //search from the end
	}

	/**
	 * Determines the first index of the given character. If the character sequence is a {@link String}, this method delegates to
	 * {@link String#lastIndexOf(int, int)}.
	 * @param charSequence The character sequence to check.
	 * @param character The character to search for.
	 * @param index The last index to examine; if greater than or equal to the length of this character sequence, it has the same effect as if it were equal to
	 *          one less than the length of this character sequence, and the entire character sequence may be searched; if negative, it has the same effect as if
	 *          it were -1, and -1 is returned.
	 * @return The index of the last occurrence of the given character, or -1 if the character was not found.
	 */
	public static int lastIndexOf(final CharSequence charSequence, final char character, int index) //TODO add support for supplementary code points here and throughout
	{
		if(charSequence instanceof String) //if the character sequence is a string
		{
			return ((String)charSequence).lastIndexOf(character, index); //delegate to the String version, which is much more efficient
		}
		final int length = charSequence.length();
		if(index >= length) //adjust the length as per the String.lastIndexOf() API
		{
			index = length - 1;
		}
		for(int i = index; i >= 0; --i) //look at each character
		{
			if(charSequence.charAt(i) == character) //if this character matches
			{
				return i; //return the matching index
			}
		}
		return -1; //show that we couldn't find the character
	}

	/**
	 * Searches a character sequence and returns the last index of any character of the given characters.
	 * @param charSequence The character sequence to be searched.
	 * @param characters The characters to check.
	 * @return The index of the last occurrence of one of the supplied characters, or -1 if none were found.
	 */
	public static int lastIndexOf(final CharSequence charSequence, final Characters characters)
	{
		return lastIndexOf(charSequence, characters, charSequence.length() - 1); //look of the characters, starting at the end of the string
	}

	/**
	 * Searches a character sequence and returns the last index of any character of the given characters, starting at the given index.
	 * @param charSequence The character sequence to be searched.
	 * @param characters The characters to check.
	 * @param index The last index to examine; if greater than or equal to the length of this character sequence, it has the same effect as if it were equal to
	 *          one less than the length of this character sequence, and the entire character sequence may be searched; if negative, it has the same effect as if
	 *          it were -1, and -1 is returned.
	 * @return The index of the last occurrence of one of the supplied characters, or -1 if none were found.
	 */
	public static int lastIndexOf(final CharSequence charSequence, final Characters characters, int index)
	{
		final int length = charSequence.length();
		if(index >= length) //adjust the length as per the String.lastIndexOf() API
		{
			index = length - 1;
		}
		for(int i = index; i >= 0; --i) //look at each character in the sequence
		{
			if(characters.contains(charSequence.charAt(i))) //if this character is in our character string
			{
				return i; //return the index we're at
			}
		}
		return -1; //if we make it to here, we didn't find any of the characters
	}

	/**
	 * Searches a character sequence and returns the first index of any character <em>not</em> in the specified characters, starting from the beginning.
	 * @param charSequence The character sequence to be searched.
	 * @param notCharacters The characters to check.
	 * @return The index of the first occurrence of one of the supplied characters, or -1 if none were found.
	 */
	public static int notCharIndexOf(final CharSequence charSequence, final Characters notCharacters)
	{
		return notCharIndexOf(charSequence, notCharacters, 0); //start looking from the beginning
	}

	/**
	 * Searches a character sequence and returns the first index of any character <em>not</em> in the specified characters, starting at the given index.
	 * @param charSequence The character sequence to be searched.
	 * @param notCharacters The characters to check.
	 * @param index The last index to examine; if greater than or equal to the length of this character sequence, it has the same effect as if it were equal to
	 *          one less than the length of this character sequence, and the entire character sequence may be searched; if negative, it has the same effect as if
	 *          it were -1, and -1 is returned.
	 * @return The index of the first occurrence of one of the supplied characters, or -1 if none were found.
	 */
	public static int notCharIndexOf(final CharSequence charSequence, final Characters notCharacters, int index)
	{
		final int length = charSequence.length();
		if(index >= length) //adjust the length as per the String.lastIndexOf() API
		{
			index = length - 1;
		}
		for(int i = index; i < charSequence.length(); ++i) //look at each character in the sequence
		{
			if(!notCharacters.contains(charSequence.charAt(i))) //if this character is not in our character string
				return i; //return the index we're at
		}
		return -1; //if we make it to here, we didn't find any characters which weren't in our character string
	}

	/**
	 * Searches a character sequence and returns the last index of any character <em>not</em> in the specified characters, starting at the last index.
	 * @param charSequence The character sequence to be searched.
	 * @param notCharacters The characters to check.
	 * @return The index of the last occurrence of one of the supplied characters, or -1 if none were found.
	 */
	public static int notCharLastIndexOf(final CharSequence charSequence, final Characters notCharacters)
	{
		return notCharLastIndexOf(charSequence, notCharacters, charSequence.length() - 1); //start searching from the end
	}

	/**
	 * Searches a character sequence and returns the last index of any character <em>not</em> in the specified characters, starting at the given index.
	 * @param charSequence The character sequence to be searched.
	 * @param notCharacters The characters to check.
	 * @param index The last index to examine; if greater than or equal to the length of this character sequence, it has the same effect as if it were equal to
	 *          one less than the length of this character sequence, and the entire character sequence may be searched; if negative, it has the same effect as if
	 *          it were -1, and -1 is returned.
	 * @return The index of the last occurrence of one of the supplied characters, or -1 if none were found.
	 */
	public static int notCharLastIndexOf(final CharSequence charSequence, final Characters notCharacters, int index)
	{
		final int length = charSequence.length();
		if(index >= length) //adjust the length as per the String.lastIndexOf() API
		{
			index = length - 1;
		}
		for(int i = index; i >= 0; --i) //look at each character in the sequence , looking from right to left
		{
			if(!notCharacters.contains(charSequence.charAt(i))) //if this character is not in our character string
				return i; //return the index we're at
		}
		return -1; //if we make it to here, we didn't find any characters which weren't in our character string
	}

	/**
	 * Determines if the character sequence consists of nothing but the following character.
	 * @param charSequence The character sequence to examine.
	 * @param c The character that could make up the entire sequence.
	 * @return <code>true</code> if there are no other characters but the specified character, <code>false</code> if there are other characters or if the string
	 *         is the empty string.
	 */
	public final static boolean isAll(final CharSequence charSequence, final char c)
	{
		if(charSequence.length() == 0) //if this is an empty string
			return false; //there are no characters to check
		for(int i = charSequence.length() - 1; i >= 0; --i) //look at each character in the string
		{
			if(charSequence.charAt(i) != c) //if this isn't the specified character
				return false; //show that the string contains other characters besides the one specified
		}
		return true; //if we make it to here, there weren't any characters other than the one specified
	}

	/**
	 * Determines if the character sequence consists of nothing but the given characters.
	 * @param charSequence The character sequence to examine.
	 * @param characters The characters that could make up the entire string, in any order.
	 * @return <code>true</code> if there are no other characters but the specified characters, <code>false</code> if there are other characters or if the
	 *         character sequence is empty.
	 */
	public final static boolean isAllChars(final CharSequence charSequence, final Characters characters)
	{
		if(charSequence.length() == 0) //if this is an empty string
			return false; //there are no characters to check
		for(int i = charSequence.length() - 1; i >= 0; --i) //look at each character in the string
		{
			if(!characters.contains(charSequence.charAt(i))) //if this character isn't in the string
				return false; //show that the string contains other characters besides the ones specified
		}
		return true; //if we make it to here, there weren't any characters other than the ones specified
	}

	/**
	 * Determines whether a character sequence is capitalized. A character sequence is capitalized if it contains any characters and the first character is
	 * uppercase.
	 * @param charSequence The character sequence to examine.
	 * @return <code>true</code> if the character sequence is capitalized.
	 */
	public final static boolean isCapitalized(final CharSequence charSequence)
	{
		return charSequence.length() > 0 && Character.isUpperCase(charSequence.charAt(0)); //determine if the first character is capitalized
	}

	/**
	 * Determines whether a character sequence contains only Unicode digits.
	 * @param charSequence The character sequence to examine.
	 * @return <code>true</code> if all the characters in the sequence are digits.
	 */
	public final static boolean isDigits(final CharSequence charSequence)
	{
		if(charSequence.length() == 0) //if this is an empty string
			return false; //there are no characters to check
		for(int i = charSequence.length() - 1; i >= 0; --i) //look at each letter in the string
		{
			if(!Character.isDigit(charSequence.charAt(i))) //if this isn't a digit
				return false; //show that the string doesn't contain only digits
		}
		return true; //if we make it to here, there weren't any non-digits in the string
	}

	/**
	 * Determines whether a character sequence contains only the digits '0'-'9'.
	 * @param charSequence The character sequence to examine.
	 * @return <code>true</code> if all the characters in the sequence are ISO_LATIN_1 digits.
	 */
	public final static boolean isLatinDigits(final CharSequence charSequence)
	{
		if(charSequence.length() == 0) //if this is an empty string
			return false; //there are no characters to check
		for(int i = charSequence.length() - 1; i >= 0; --i) //look at each letter in the string
		{
			if(!isLatinDigit(charSequence.charAt(i))) //if this isn't a Latin digit
				return false; //show that the string doesn't contain only latin digits
		}
		return true; //if we make it to here, there weren't any non-latin-digits in the string
	}

	/**
	 * Determines whether a character sequence contains only Unicode letters.
	 * @param charSequence The character sequence to examine.
	 * @return <code>true</code> if all the characters in the sequence are letters.
	 */
	public final static boolean isLetters(final CharSequence charSequence)
	{
		if(charSequence.length() == 0) //if this is an empty string
			return false; //there are no characters to check
		for(int i = charSequence.length() - 1; i >= 0; --i) //look at each letter in the string
		{
			if(!Character.isLetter(charSequence.charAt(i))) //if this isn't a letter
				return false; //show that the string doesn't contain only letters
		}
		return true; //if we make it to here, there weren't any non-letters in the string
	}

	/**
	 * Determines whether a character sequence contains only Unicode letters and digits.
	 * @param charSequence The character sequence to examine.
	 * @return <code>true</code> if all the characters in the sequence are letters and digits.
	 */
	public final static boolean isLettersDigits(final CharSequence charSequence)
	{
		if(charSequence.length() == 0) //if this is an empty string
			return false; //there are no characters to check
		for(int i = charSequence.length() - 1; i >= 0; --i) //look at each letter in the string
		{
			final char character = charSequence.charAt(i); //get this character
			if(!Character.isLetter(character) && !Character.isDigit(character)) //if this is not a letter or a digit
				return false; //show that the string contains non-letter or non-digit characters
		}
		return true; //if we make it to here, there weren't any non-letters or non-digits in the string
	}

	/**
	 * Determines whether a character sequence contains only Unicode letters, digits, and the supplied extra characters.
	 * @param charSequence The character sequence to examine.
	 * @param characters Extra characters to allow.
	 * @return <code>true</code> if all the characters in the sequence are letters, digits, and/or allowed characters.
	 */
	public final static boolean isLettersDigitsCharacters(final CharSequence charSequence, final String characters)
	{
		if(charSequence.length() == 0) //if this is an empty string
			return false; //there are no characters to check
		for(int i = charSequence.length() - 1; i >= 0; --i) //look at each letter in the string
		{
			final char character = charSequence.charAt(i); //get this character
			if(!Character.isLetter(character) && !Character.isDigit(character) && !contains(characters, character)) //if this is not a letter or a digit, and it's not in our extra character list
				return false; //show that the string contains something in none of our lists 
		}
		return true; //if we make it to here, there weren't any non-letters or non-digits in the string
	}

	/**
	 * Determines whether a character sequence contains only numbers and decimals or commas.
	 * @param charSequence The character sequence to examine.
	 * @return <code>true</code> if all the characters represent a number.
	 */
	public final static boolean isNumber(final CharSequence charSequence) //TODO use a regex, and verify format
	{
		if(charSequence.length() == 0) //if this is an empty string
			return false; //there are no characters to check
		for(int i = charSequence.length() - 1; i >= 0; --i) //look at each letter in the string
		{
			final char c = charSequence.charAt(i); //get this character
			if(!Character.isDigit(c) && c != '.' && c != ',') //if this isn't a digit, a decimal, or a comma
				return false; //show that the string doesn't contain a number
		}
		return true; //if we make it to here, this is a number
	}

	/**
	 * Determines whether a character sequeence contains only Roman numerals.
	 * @param charSequence The character sequence to examine.
	 * @return <code>true</code> if all the characters in the sequence are roman numerals.
	 */
	public final static boolean isRomanNumerals(final CharSequence charSequence)
	{
		if(charSequence.length() == 0) //if this is an empty string
			return false; //there are no characters to check
		for(int i = charSequence.length() - 1; i >= 0; --i) //look at each character in the string
		{
			if(!isRomanNumeral(charSequence.charAt(i))) //if this isn't a roman numberal
				return false; //show that the string doesn't contain only roman numberals
		}
		return true; //if we make it to here, there weren't any characters in the string that were not roman numerals
	}

	/**
	 * Determines whether all the letters in a character sequence are capital letters.
	 * @param charSequence The character sequence to examine.
	 * @return <code>true</code> if all the letters in the sequence are capitalized.
	 */
	public final static boolean isUpperCase(final CharSequence charSequence)
	{
		if(charSequence.length() == 0) //if this is an empty string
			return false; //there are no characters to check
		for(int i = charSequence.length() - 1; i >= 0; --i) //look at each letter in the string
		{
			final char character = charSequence.charAt(i); //get this character
			if(Character.isLetter(character) && !Character.isUpperCase(character)) //if this is a letter that is not uppercase
				return false; //show that the string contains non-uppercase characters
		}
		return true; //if we make it to here, there weren't any non-uppercase characters in the string
	}

	/**
	 * Concatenates the given character sequences with no delimiter between them.
	 * @param charSequences The character sequences to be concatenated.
	 * @return The string containing the concatenated character sequences.
	 * @throws NullPointerException if the given character sequences is <code>null</code>.
	 */
	public static CharSequence join(final CharSequence... charSequences)
	{
		return join(UNDEFINED_CHAR, charSequences); //join with no delimiter
	}

	/**
	 * Concatenates the given character sequences, separated by the given delimiter.
	 * @param delimiter The delimiter to be placed between each character sequence, or {@link Characters#UNDEFINED_CHAR} if no delimiter should be placed between
	 *          the character sequences.
	 * @param charSequences The character sequences to be concatenated.
	 * @return The string containing the concatenated character sequences.
	 * @throws NullPointerException if the given character sequences is <code>null</code>.
	 */
	public static CharSequence join(final char delimiter, final CharSequence... charSequences)
	{
		final int length = charSequences.length; //find out how many character sequences there are
		if(length > 1) //if there are more than one character sequence
		{
			CharSequence nonEmptyCharSequence = null; //see if we can short-circuit the process if there is only one non-empty character sequence
			for(final CharSequence charSequence : charSequences)
			{
				if(charSequence.length() > 0) //if this character sequence has characters
				{
					if(nonEmptyCharSequence != null) //if we already found another character sequence with characters (i.e. there are at least two)
					{
						nonEmptyCharSequence = null; //don't try anymore
						break; //stop searching
					}
					nonEmptyCharSequence = charSequence; //keep track of the first (and so far, only) character sequence with characters
				}
			}
			if(nonEmptyCharSequence != null) //if there was one and only one character sequence with characters
			{
				return nonEmptyCharSequence; //there's no use joining---just return the character sequence
			}
			return StringBuilders.append(new StringBuilder(), delimiter, charSequences); //join the character strings using a string builder
		}
		else if(length == 1) //if there is only one character sequence
		{
			return charSequences[0]; //return the one character sequence
		}
		else
		//if there are no character sequences
		{
			return ""; //return the empty string
		}
	}

	/**
	 * Splits a characters sequence into subsequences based upon the given delimiter. Subsequences will be returned between delimiters even if they are empty, and
	 * a subsequence will be returned after the last delimiter, even if there are no remaining characters. In other words, the number of character subsequences
	 * returned is <var>delimiterCount</var>+1.
	 * @param charSequence The character sequence to split.
	 * @param delimiter The delimiter to use for splitting.
	 * @return An array of character subsequences between the delimiters.
	 * @deprecated
	 */
	public static CharSequence[] split(final CharSequence charSequence, final char delimiter) //TODO convert to using regular expressions
	{
		final int length = charSequence.length(); //get the length of the character sequence
		if(length > 0) //if there are any characters
		{
			final int delimiterCount = count(charSequence, delimiter); //count the number of delimiters
			if(delimiterCount > 0) //if there is at least one delimiter
			{
				//count the delimiters; this should be faster than creating a list and dynamically adding subsequences
				final CharSequence[] subSequences = new CharSequence[delimiterCount + 1]; //there will always be one more character sequence than delimiter
				int start = 0; //start searching at the beginning
				int delimiterIndex; //we'll keep track of where we find the delimiter each time
				int i = 0; //keep track of the subsequence index
				do
				{
					assert start < charSequence.length() : "Delmiter counting and splitting logic out of synchronization.";
					delimiterIndex = indexOf(charSequence, delimiter, start); //find the index of the next delimiter
					final int end = delimiterIndex >= 0 ? delimiterIndex : length; //if we didn't find a delimiter, just use the rest of the character sequence
					subSequences[i] = charSequence.subSequence(start, end); //create a subsequence between delimiters
					start = end + 1; //start looking at the position after the delimiter
					++i; //go to the next position for storing subsequences
				}
				while(i < subSequences.length); //keep looking until we've found the correct number of delimiters
				return subSequences; //return the array of subsequences
			}
		}
		return new CharSequence[] { charSequence }; //return an array cotaining the character sequence itself if there are no characters or no delimiters
	}

	/**
	 * Determines if the character sequence starts with the given character.
	 * @param charSequence The character sequence to examine.
	 * @param character The character to compare.
	 * @return <code>true</code> if the first character of the character sequence matches that of the given string.
	 */
	public static boolean startsWith(final CharSequence charSequence, final char character)
	{
		//see if the character sequence has at least one character, and the first character matches our character
		return charSequence.length() > 0 && charSequence.charAt(0) == character;
	}

	/**
	 * Determines if the character sequence starts with the given string.
	 * @param charSequence The character sequence to examine.
	 * @param string The string to compare.
	 * @return <code>true</code> if the first characters of the character sequence match those of the given string.
	 * @throws NullPointerException if the given string is <code>null</code>.
	 */
	public static boolean startsWith(final CharSequence charSequence, final CharSequence string) //TODO refactor startsWith() and endsWith() into a generic method
	{
		return startsWith(charSequence, 0, string);
	}

	/**
	 * Determines if the character sequence starts with the given string, starting at the given index.
	 * @param charSequence The character sequence to examine.
	 * @param index The index at which to search.
	 * @param string The string to compare.
	 * @return <code>true</code> if the first characters of the character sequence match those of the given string.
	 * @throws NullPointerException if the given string is <code>null</code>.
	 */
	public static boolean startsWith(final CharSequence charSequence, final int index, final CharSequence string) //TODO refactor startsWith() and endsWith() into a generic method
	{
		if(charSequence.length() - index < string.length()) //if the substring is too long
		{
			return false; //the substring is too big to start the character sequence
		}
		for(int i = string.length() - 1; i >= 0; --i) //look at each character of the string
		{
			if(string.charAt(i) != charSequence.charAt(index + i)) //if these characters don't match in the same position
			{
				return false; //the string doesn't match
			}
		}
		return true; //the character sequence starts with the string
	}

	/**
	 * Determines which if any of the given strings the character sequence starts with.
	 * @param charSequence The character sequence to examine.
	 * @param strings The string to compare.
	 * @return The string beginning the character sequence, or <code>null</code> if none of the strings start the character sequence.
	 * @throws NullPointerException if the collection of strings of any of the strings is <code>null</code>.
	 */
	public static <S extends CharSequence> S getStartsWith(final CharSequence charSequence, final Collection<S> strings)
	{
		return getStartsWith(charSequence, 0, strings);
	}

	/**
	 * Determines which if any of the given strings the character sequence starts with, starting at the given index.
	 * @param charSequence The character sequence to examine.
	 * @param index The index at which to search.
	 * @param strings The string to compare.
	 * @return The string beginning the character sequence, or <code>null</code> if none of the strings start the character sequence.
	 * @throws NullPointerException if the collection of strings of any of the strings is <code>null</code>.
	 */
	public static <S extends CharSequence> S getStartsWith(final CharSequence charSequence, final int index, final Collection<S> strings)
	{
		for(final S string : strings)
		{
			if(startsWith(charSequence, index, string))
			{
				return string;
			}
		}
		return null;
	}

	/**
	 * Determines if the character sequence starts with one of the given characters.
	 * @param charSequence The character sequence to examine.
	 * @param characters The characters to compare.
	 * @return <code>true</code> if the first character of the character sequence matches one of those in the given string.
	 */
	public static boolean startsWithChar(final CharSequence charSequence, final Characters characters)
	{
		//see if the character sequence has at least one character, and the first character matches our character
		return charSequence.length() > 0 && characters.contains(charSequence.charAt(0));
	}

	/**
	 * Returns a character array containing the characters from the given character sequence.
	 * @param charSequence The character sequence from which to retrieve characters.
	 * @return A character array containing the characters from the given character sequence.
	 * @throws NullPointerException if the given character sequence is <code>null</code>.
	 */
	public static char[] toCharArray(final CharSequence charSequence)
	{
		if(charSequence instanceof String) //if this is a String, don't invent the wheel
		{
			return ((String)charSequence).toCharArray();
		}
		final int length = charSequence.length();
		final char[] chars = new char[length]; //create a new character array of the correct length
		for(int i = length - 1; i >= 0; --i) //populate the character array
		{
			chars[i] = charSequence.charAt(i);
		}
		return chars;
	}

	/**
	 * Truncates the end of the string beginning at the first occurrence of the given character. If the character sequence does not contain the truncate
	 * character, no action takes place.
	 * @param charSequence The character sequence to check.
	 * @param truncateChar The character indicating the part of the sequence to trim.
	 * @return A new character sequence with the specified character and following characters removed.
	 */
	public static CharSequence truncateAtFirst(final CharSequence charSequence, final char truncateChar)
	{
		final int index = indexOf(charSequence, truncateChar); //find the first occurrence of the truncate character
		return index >= 0 ? charSequence.subSequence(0, index) : charSequence; //truncate the character sequence if we can		
	}

	/**
	 * Truncates the end of the string beginning at the last occurrence of the given character. If the character sequence does not contain the truncate character,
	 * no action takes place.
	 * @param charSequence The character sequence to check.
	 * @param truncateChar The character indicating the part of the sequence to trim.
	 * @return A new character sequence with the last of the specified character and following characters removed.
	 */
	public static CharSequence truncateAtLast(final CharSequence charSequence, final char truncateChar)
	{
		final int index = lastIndexOf(charSequence, truncateChar); //find the last occurrence of the truncate character
		return index >= 0 ? charSequence.subSequence(0, index) : charSequence; //truncate the character sequence if we can		
	}

	/**
	 * Determines if the given character sequence is composed of the single given character. This method allows comparison of a character string with a character
	 * without creating a string for the character, for example.
	 * @param charSequence The character sequence to compare.
	 * @param character The character to compare with the character sequence.
	 * @return <code>true</code> if the character sequence is composed of one character and that character matches the given character.
	 */
	public static boolean equals(final CharSequence charSequence, final char character)
	{
		return charSequence.length() == 1 && charSequence.charAt(0) == character; //see if the character sequence has only one character, the given character
	}

	/**
	 * Compares the characters in one character sequence with characters in another character sequence.
	 * @param charSequence1 The character sequence to compare.
	 * @param charSequence2 The character sequence to compare with.
	 * @return <code>true</code> if the characters in the first character sequence equal the characters in the second character sequence.
	 */
	public static boolean equals(final CharSequence charSequence1, final CharSequence charSequence2)
	{
		if(charSequence1 == charSequence2) //identity always implies equality
		{
			return true;
		}
		return equals(charSequence1, charSequence2, 0);
	}

	/**
	 * Compares the characters in one character sequence with characters in another character sequence, starting at the given location to the end of the second
	 * character sequence.
	 * @param charSequence1 The character sequence to compare.
	 * @param charSequence2 The character sequence to compare with.
	 * @param start The starting location in the second character sequence, inclusive.
	 * @return <code>true</code> if the characters in the first character sequence equal the indicated characters in the second character sequence.
	 * @throws StringIndexOutOfBoundsException if <code>start</code> is negative or greater than the length of the second character sequence.
	 */
	public static boolean equals(final CharSequence charSequence1, final CharSequence charSequence2, final int start)
	{
		return equals(charSequence1, charSequence2, start, charSequence2.length());
	}

	/**
	 * Compares the characters in one character sequence with characters in another character sequence. If the given end of the second character sequence (the
	 * character sequence to which the first is being compared) is past the end, it is adjusted to be equal to the end of the second character sequence.
	 * @param charSequence1 The character sequence to compare.
	 * @param charSequence2 The character sequence to compare with.
	 * @param start The starting location in the second character sequence, inclusive.
	 * @param end The ending location in the second character sequence, exclusive.
	 * @return <code>true</code> if the characters in the first character sequence equal the indicated characters in the second character sequence.
	 * @throws StringIndexOutOfBoundsException if <code>start</code> or <code>end</code> is negative or greater than <code>length()</code>, or <code>start</code>
	 *           is greater than <code>end</code>, with the exception that if <code>end</code> is greater than the length of the second character sequence it will
	 *           be adjusted to equal the end.
	 */
	public static boolean equals(final CharSequence charSequence1, final CharSequence charSequence2, final int start, final int end)
	{
		return equals(charSequence1, 0, charSequence1.length(), charSequence2, start, end);
	}

	/**
	 * Compares characters in one character sequence with characters in another character sequence. If the given end of the second character sequence (the
	 * character sequence to which the first is being compared) is past the end, it is adjusted to be equal to the end of the second character sequence.
	 * @param charSequence1 The character sequence to compare.
	 * @param start1 The starting location in the first character sequence, inclusive.
	 * @param end1 The ending location in the first character sequence, exclusive.
	 * @param charSequence2 The character sequence to compare with.
	 * @param start2 The starting location in the second character sequence, inclusive.
	 * @param end2 The ending location in the second character sequence, exclusive.
	 * @return <code>true</code> if the indicated characters in the first character sequence equal the indicated characters in the second character sequence.
	 * @throws StringIndexOutOfBoundsException if <code>start</code> or <code>end</code> is negative or greater than <code>length()</code>, or <code>start</code>
	 *           is greater than <code>end</code>, with the exception that if <code>end2</code> is greater than the length of the second character sequence it
	 *           will be adjusted to equal the end.
	 */
	public static boolean equals(final CharSequence charSequence1, final int start1, final int end1, final CharSequence charSequence2, final int start2, int end2)
	{
		checkBounds(charSequence1, start1, end1);
		final int length2 = charSequence2.length();
		if(end2 > length2)
		{
			end2 = length2;
		}
		checkBounds(charSequence2, start2, end2);
		if((end2 - start2) != (end1 - start1)) //if the counts differ
		{
			return false;
		}
		for(int i1 = start1, i2 = start2; i1 < end1; ++i1, ++i2) //look at each character; we only need to check one end position because we already made sure the counts are the same
		{
			if(charSequence1.charAt(i1) != charSequence2.charAt(i2)) //if these characters don't match
			{
				return false;
			}
		}
		return true; //everything matches		
	}

	/**
	 * Turns an empty character sequence into <code>null</code>.
	 * @param charSequence The character sequence to examine, or <code>null</code>.
	 * @return The given character sequence, or <code>null</code> if the given character sequence has no characters or no character sequence was given.
	 */
	public static <CS extends CharSequence> CS nullify(final CS charSequence)
	{
		return charSequence != null && charSequence.length() > 0 ? charSequence : null;
	}
}
