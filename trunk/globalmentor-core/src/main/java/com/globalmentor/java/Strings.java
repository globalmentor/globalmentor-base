/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.io.*;
import java.net.URI;
import java.nio.charset.Charset;

import static com.globalmentor.io.Charsets.*;
import com.globalmentor.io.IO;
import static com.globalmentor.java.Characters.*;
import com.globalmentor.text.*;

/**
 * Various text manipulating functions. These methods work on {@link String} objects, which are immutable heavyweight objects that must be recreated with every
 * modification. Many of these methods should therefore be avoided in favor of their corresponding {@link StringBuilders} methods, which operate on
 * {@link StringBuilder} objects.
 * @see StringBuilders
 * @author Garret Wilson
 */
public class Strings
{

	private Strings()
	{
	}

	/** A shared empty array of strings. */
	public final static String[] NO_STRINGS = new String[0];

	//TODO move most of the methods that reference CharSequences to that class
	//TODO convert StringBuffer code to StringBuilder

	/**
	 * Convenience method to create a string from characters using varargs.
	 * @param chars The characters that should make up the string.
	 * @return A string created from the given characters.
	 * @throws NullPointerException if the given chars is <code>null</code>.
	 */
	public final static String stringOf(final char... chars)
	{
		if(chars.length == 0)
		{
			return ""; //return the shared empty string
		}
		return new String(chars);
	}

	/**
	 * Creates an array of strings from the given string.
	 * @param string The string to include in the array, or <code>null</code> if the array should be empty.
	 * @return A non-<code>null</code> array containing the string, or empty if the string is <code>null</code>.
	 */
	public static String[] createArray(final String string)
	{
		return string != null ? new String[] { string } : NO_STRINGS; //return an array containing the string, or an empty array if the string is null
	}

	/**
	 * Concatenates the string representations of the objects in the array. Null objects are represented by the string "null".
	 * @param objects The array of objects (such as strings) to be concatenated.
	 * @return A concatenation of string representations of all objects in the array.
	 * @see Object#toString()
	 */
	public static String append(final Object[] objects)
	{
		return StringBuilders.append(new StringBuilder(), objects).toString(); //append the objects to a string builder and return the string
	}

	/**
	 * Concatenates the string representations of the objects in the array, separated by the given separator character. Null objects are represented by the string
	 * "null".
	 * @param objects The array of objects (such as strings) to be concatenated.
	 * @param separator The separator character to be inserted between the object strings.
	 * @return A concatenation of string representations of all objects in the array, separated by the separator character.
	 * @see Object#toString()
	 */
	public static String concat(final Object[] objects, final char separator)
	{
		return TextFormatter.formatList(new StringBuilder(), separator, objects).toString(); //append the objects to a string builder and return the string
	}

	/**
	 * Concatenates the string representations of the objects in the array, separated by the given separator character. Null objects are represented by the string
	 * "null".
	 * @param objects The array of objects (such as strings) to be concatenated.
	 * @param separator The separator character to be inserted between the object strings.
	 * @param ignoreObject The object to ignore, or <code>null</code> if no objects should be ignored.
	 * @return A concatenation of string representations of all objects in the array, separated by the separator character.
	 * @see Object#toString()
	 */
	public static String concat(final Object[] objects, final char separator, final Object ignoreObject)
	{
		return StringBuilders.append(new StringBuilder(), objects, separator, ignoreObject).toString(); //append the objects to a string builder and return the string
	}

	/**
	 * Concatenates the string representations of the objects in the array, separated by the given separator string. Null objects are represented by the string
	 * "null".
	 * @param objects The array of objects (such as strings) to be concatenated.
	 * @param separator The separator string to be inserted between the object strings, or <code>null</code> if no separator should be used.
	 * @return A concatenation of string representations of all objects in the array, separated by the separator.
	 * @see Object#toString()
	 */
	public static String concat(final Object[] objects, final String separator)
	{
		return TextFormatter.formatList(new StringBuilder(), separator, objects).toString(); //append the objects to a string builder and return the string
	}

	/**
	 * Concatenates the string representations of the objects in the array, separated by the given separator character. Null objects are represented by the string
	 * "null".
	 * @param objects The objects (such as strings) to be concatenated.
	 * @param separator The separator string to be inserted between the object strings, or <code>null</code> if no separator should be used.
	 * @return A concatenation of string representations of all given objects, separated by the separator.
	 * @see Object#toString()
	 */
	public static String concat(final Iterable<?> objects, final char separator)
	{
		return StringBuilders.append(new StringBuilder(), objects, separator).toString(); //append the objects to a string builder and return the string
	}

	/**
	 * Concatenates the string representations of the objects in the array, separated by the given separator string. Null objects are represented by the string
	 * "null".
	 * @param objects The objects (such as strings) to be concatenated.
	 * @param separator The separator string to be inserted between the object strings, or <code>null</code> if no separator should be used.
	 * @return A concatenation of string representations of all given objects, separated by the separator.
	 * @see Object#toString()
	 */
	public static String concat(final Iterable<?> objects, final String separator)
	{
		return TextFormatter.formatList(new StringBuilder(), separator, objects).toString(); //append the objects to a string builder and return the string
	}

	/**
	 * Compares two strings to make sure that the strings are equal without regard to case, or that the strings are both set to <code>null</code>. If the first
	 * string is not <code>null</code>, it is compared to the second using the first string's {@link String#equalsIgnoreCase(String)} method. This is a
	 * convenience method to compare two strings using the {@link String#equalsIgnoreCase(String)} method when it's not known if one of the strings is
	 * <code>null</code>.
	 * @param string1 The first string to compare.
	 * @param string2 The second string to compare.
	 * @return <code>true</code> if the strings are equal according to the first string's {@link String#equalsIgnoreCase(String)} method or if both strings are
	 *         <code>null</code>.
	 * @see String#equalsIgnoreCase(String)
	 */
	public final static boolean equalsIgnoreCase(final String string1, final String string2)
	{
		//if the first string isn't null, compare it to the second; otherwise, see if the second string is null as well
		return string1 != null ? string1.equalsIgnoreCase(string2) : string2 == null;
	}

	/**
	 * Creates an array of bytes of the specified length and stores the bytes of the string, in UTF-8, in the array postpended with a zero byte. If the string is
	 * too long for the given length, it is truncated.
	 * @param string The string to store in bytes.
	 * @param length The length of bytes to return.
	 * @exception UnsupportedEncodingException Thrown if the given encoding is not supported.
	 */
	public static byte[] getASCIIZBytes(final String string, final int length) throws UnsupportedEncodingException
	{
		return getASCIIZBytes(string, length, CharacterEncoding.UTF_8); //return the bytes, encoded using UTF-8
	}

	/**
	 * Creates an array of bytes of the specified length and stores the bytes of the string, in the array postpended with a zero byte. If the string is too long
	 * for the given length, it is truncated.
	 * @param string The string to store in bytes.
	 * @param length The length of bytes to return.
	 * @param encoding The encoding to use in storing the string bytes.
	 * @exception UnsupportedEncodingException Thrown if the given encoding is not supported.
	 */
	public static byte[] getASCIIZBytes(String string, final int length, final String encoding) throws UnsupportedEncodingException
	{
		final byte[] stringBytes = string.getBytes(encoding); //get the bytes of the string
		final byte[] asciizBytes = new byte[length]; //create a byte array to return
		final int copyLength = Math.min(string.length(), length - 1); //find out how many bytes to copy TODO fix; this assumes UTF-8
		System.arraycopy(stringBytes, 0, asciizBytes, 0, copyLength); //copy the string bytes to the outgoing array
		asciizBytes[copyLength] = 0; //add a zero to the end of the string
		return asciizBytes; //return the byte array we constructed
	}

	/**
	 * Creates an input stream from which to read the given string.
	 * @param string The string for which an input stream should be created.
	 * @param encoding The encoding to use to turn the string into bytes.
	 * @return An input stream of the string bytes.
	 * @exception UnsupportedEncodingException Thrown if the given encoding is not supported.
	 */
	public static InputStream getInputStream(final String string, final String encoding) throws UnsupportedEncodingException
	{
		return new ByteArrayInputStream(string.getBytes(encoding)); //return an input stream to the bytes of the string, encoded using the given encoding
	}

	/**
	 * Returns the string or <code>null</code> if the string is the empty string ("").
	 * @param string The string to examine.
	 * @return <code>string</code> if the length of the string is greater than zero, otherwise <code>null</code>.
	 */
	public static String getNonEmptyString(final String string)
	{
		return string.length() > 0 ? string : null; //return the string if it has a positive length
	}

	/**
	 * Finds the index of a particular character, ignoring case.
	 * @param string The string to search.
	 * @param c The character to search for.
	 * @return The index of the character ignoring case, or -1 if the character could not be found.
	 */
	static public int indexOfIgnoreCase(final String string, final char c)
	{
		return indexOfIgnoreCase(string, c, 0); //attempt to search from the beginning
	}

	/**
	 * Finds the index of a particular character, ignoring case, from a given index.
	 * @param string The string to search.
	 * @param c The character to search for.
	 * @param fromIndex The index at which to begin searching
	 * @return The index of the character ignoring case, or -1 if the character could not be found.
	 */
	static public int indexOfIgnoreCase(final String string, final char c, final int fromIndex)
	{
		return string.toUpperCase().indexOf(Character.toUpperCase(c), fromIndex); //convert the string and character to uppercase and search
	}

	/**
	 * Finds the index of a particular substring, ignoring case.
	 * @param string The string to search.
	 * @param substring The string to search for.
	 * @return The index of the substring ignoring case, or -1 if the substring could not be found.
	 */
	static public int indexOfIgnoreCase(final String string, final String substring)
	{
		return indexOfIgnoreCase(string, substring, 0); //attempt to search from the beginning
	}

	/**
	 * Finds the index of a particular substring, ignoring case, from a given index.
	 * @param string The string to search.
	 * @param substring The string to search for.
	 * @param fromIndex The index at which to begin searching
	 * @return The index of the substring ignoring case, or -1 if the substring could not be found.
	 */
	static public int indexOfIgnoreCase(final String string, final String substring, final int fromIndex)
	{
		return string.toUpperCase().indexOf(substring.toUpperCase(), fromIndex); //convert the strings to uppercase and search
	}

	/**
	 * Inserts a string at a specified index.
	 * @param inString the String into which the information will be inserted.
	 * @param index The information will be inserted before the character that appears at this index.
	 * @param insertString The string to insert.
	 * @return A new string with the specified information inserted at the specified location.
	 */
	static public String insert(final String inString, final int index, final String insertString)
	{
		return inString.substring(0, index) + insertString + inString.substring(index); //return a string with the specified string inserted at the specified location
	}

	/**
	 * Inserts a character at a specified index.
	 * @param inString the String into which the information will be inserted.
	 * @param index The information will be inserted before the character that appears at this index.
	 * @param insertChar The character to insert.
	 * @return A new string with the specified information inserted at the specified location.
	 */
	static public String insert(final String inString, final int index, final char insertChar)
	{
		return inString.substring(0, index) + insertChar + inString.substring(index); //return a string with the specified character inserted at the specified location
	}

	/**
	 * Searches a string in reverse and returns the last index of a substring without case sensitivity, starting from <code>fromIndex</code>.
	 * @param string The string to be searched.
	 * @param substring The substring for which to search.
	 * @param fromIndex The index from which to search.
	 * @return The index of the last occurrence of the substring at or less than the given index, or -1 if none was found.
	 */
	static public int lastIndexOfIgnoreCase(final String string, final String substring, final int fromIndex)
	{
		return string.toUpperCase().lastIndexOf(substring.toUpperCase(), fromIndex); //search without regard to case
	}

	/**
	 * Determines whether the string starts with the given prefix, ignoring case.
	 * @param string The string to search.
	 * @param prefix The starting string to check for.
	 * @return <code>true</code> if the string starts with the given prefix, ignoring case.
	 */
	static public boolean startsWithIgnoreCase(final String string, final String prefix)
	{
		return string.toUpperCase().startsWith(prefix.toUpperCase()); //convert the strings to uppercase and check the prefix TODO use a more efficient method that doesn't include creating new strings
	}

	/**
	 * Returns the index of the given numbered (one-based) token.
	 * @param inString The string to search.
	 * @param tokenNumber The number (one-based) of the token to find.
	 * @param delimiters The characters to use for delimiters.
	 * @return The index of the specified token number, or -1 if that token was not found.
	 */
	static public int tokenIndex(final String inString, int tokenNumber, final Characters delimiters)
	{
		int i = 0; //start at the beginning of the string
		while(true)
		{
			i = CharSequences.notCharIndexOf(inString, delimiters, i); //find the next token
			if(i == -1) //if there is no other token
				break; //exit, because there are no more tokens left
			tokenNumber--; //show that we've found another token
			if(tokenNumber == 0) //if we've found all the tokens we needed to
				break; //leave, because i now has the position of that token
			else
			//if there are still more tokens to find
			{
				i = CharSequences.charIndexOf(inString, delimiters, i); //starting at our current position, find the next delimiter character
				if(i == -1) //if there is no delimiter after this token (i.e. this is the last token)
					break; //exit, because there are no more tokens left
			}
		}
		return i; //return the index of the token, or -1 if this token doesn't exist
	}

	/**
	 * Returns the index right after the given numbered (one-based) token.
	 * @param inString The string to search.
	 * @param tokenNumber The number (one-based) of the token to find.
	 * @param delimiters The characters to use for delimiters.
	 * @return The index of one character past the last character of the specified token number, or -1 if that token was not found.
	 */
	static public int tokenEndIndex(final String inString, final int tokenNumber, final Characters delimiters)
	{
		int i = tokenIndex(inString, tokenNumber, delimiters); //find the beginning of the specified token
		if(i != -1) //if we found the beginning of the specified token
		{
			i = CharSequences.charIndexOf(inString, delimiters, i); //find the character right after the token
			if(i == -1) //if there are no more delimiters after this token
				i = inString.length(); //we know that this token goes to the end of the string
		}
		return i; //return the index of one character past the token, or -1 if this token doesn't exist
	}

	/**
	 * Returns the specified numbered (one-based) token in the specified string, separated by delimiters.
	 * @param inString The string to search.
	 * @param tokenNumber The number (one-based) of the token to find.
	 * @param delimiters The characters to use for delimiters.
	 * @return The specified numbered (one-based) token in the specified string, separated by delimiters, or "" if that token was not found.
	 */
	static public String stringToken(final String inString, final int tokenNumber, final Characters delimiters)
	{
		String token = ""; //assume we couldn't find the specified token
		int beginIndex = tokenIndex(inString, tokenNumber, delimiters); //find the beginning of the specified token
		if(beginIndex != -1) //if we found the beginning of the specified token
		{
			int endIndex = tokenEndIndex(inString, tokenNumber, delimiters); //find the end of the specified token
			token = inString.substring(beginIndex, endIndex); //get the token
		}
		return token; //return the token
	}

	/**
	 * Returns the index of the given numbered (one-based) word.
	 * @param inString The string to search.
	 * @param tokenNumber The number (one-based) of the token to find.
	 * @return The index of the specified word, or -1 if that word was not found.
	 */
	static public int wordIndex(final String inString, final int wordNumber)
	{
		return tokenIndex(inString, wordNumber, WORD_DELIMITER_CHARACTERS); //return the index of the word (a word is a token surrounded by word delimiters)
	}

	/**
	 * Returns the index right after the given numbered (one-based) word.
	 * @param inString The string to search.
	 * @param wordNumber The number (one-based) of the word to find.
	 * @return The index of one character past the last character of the specified word number, or -1 if that word was not found.
	 */
	static public int wordEndIndex(final String inString, final int wordNumber)
	{
		return tokenEndIndex(inString, wordNumber, WORD_DELIMITER_CHARACTERS); //return the ending index of the word (a word is a token surrounded by word delimiters)
	}

	/**
	 * Returns the specified numbered (one-based) word in the specified string.
	 * @param inString The string to search.
	 * @param wordNumber The number (one-based) of the word to find.
	 * @return The specified numbered (one-based) word in the specified string, or "" if that word was not found.
	 */
	static public String stringWord(final String inString, final int wordNumber)
	{
		return stringToken(inString, wordNumber, WORD_DELIMITER_CHARACTERS); //return the word (a word is a token surrounded by word delimiters)
	}

	/**
	 * Returns the beginning of the word at index. If the character at index is whitespace, the beginning of the previous word will be returned.
	 * @param inString The string with the word.
	 * @param index The index of the character in a word.
	 * @return The index of the beginning character of the word.
	 */
	static public int getWordBeginning(final String inString, final int index) //TODO del this function
	{
		int i;
		for(i = index; i > 0 && !isWhitespace(inString.charAt(i - 1)); --i)
			; //start at index and look back
		return i; //return the index we found
	}

	/**
	 * Returns the end of the word at index. If the character at index is whitespace, the end of the next word will be returned.
	 * @param inString The string with the word.
	 * @param index The index of the character in a word.
	 * @return The index of the ending character of the word.
	 */
	static public int getWordEnd(final String inString, final int index) //TODO del this function
	{
		int i;
		for(i = index; i < inString.length() - 1 && !isWhitespace(inString.charAt(i + 1)); ++i)
			; //start at index and look forward
		return i; //return the index we found
	}

	/**
	 * Parses the string and finds any hyperlinks, to which it then creates and inserts the appropriate HTML tags.
	 * @param inString The string in which to find hyperlinks.
	 * @return A string with HTML hyperlink tags embedded.
	 */
	static public String makeHTMLHyperlinks(final String inString)
	{
		String outString = inString; //this is the string we'll process
		int fromIndex = 0; //we'll start looking for links at the beginning of the string
		while(fromIndex < outString.length()) //keep looking until we run out of characters
		{
			int checkIndex = CharSequences.charIndexOf(outString, new Characters('.', '@'), fromIndex); //see if we can find any of the hyperlink characters TODO use a constant
			if(checkIndex != -1) //if we found one of them
			{
				int wordBegin = getWordBeginning(outString, checkIndex); //find the beginning of this word
				int originalWordEnd = getWordEnd(outString, checkIndex); //find the end of this word
				int newWordEnd = originalWordEnd; //we'll ignore ending punctuation marks
				while(newWordEnd > checkIndex) //start looking for punctuation at the end of the word
				{
					if(isPunctuation(outString.charAt(newWordEnd))) //if this is a punctuation mark
						newWordEnd--; //back up a letter to specify the word end
					else
						//if this is not a punctuation mark
						break; //we now know that the word doesn't end in a punctuation mark
				}
				if(checkIndex != wordBegin && checkIndex != newWordEnd) //if the hyperlink character is not at the beginning or ending character of the word
				{
					String HREF = outString.substring(wordBegin, newWordEnd + 1); //get the location to jump to
					if(HREF.indexOf("..") == -1) //make sure there are not two periods in a row
					{
						final String protocolString = HREF.indexOf('@') != -1 ? "mailto:" : "http://"; //if the location has the '@' character, it's an e-mail address; otherwise, it's an HTTP URL
						if(!((HREF.length() > 3 && HREF.substring(0, 4).toLowerCase().equals("http")) || (HREF.length() > 5 && HREF.substring(0, 6).toLowerCase()
								.equals("mailto")))) //if the hyperlink doesn't have a protocol at the beginning already TODO fix for ftp
							HREF = protocolString + HREF; //add the protocol to the beginning of the address for them (in our link, not in the normal text
						final String tagPrefix = "<A HREF=\"" + HREF + "\">"; //create the tag prefix
						final String tagPostfix = "</A>"; //create the tag postfix
						outString = insert(outString, newWordEnd + 1, tagPostfix); //insert the tag postfix first (after the last letter of the word), so it won't mess up our word beginning index, yet
						outString = insert(outString, wordBegin, tagPrefix); //insert the tag prefix
						fromIndex = originalWordEnd + tagPrefix.length() + tagPostfix.length() + 1; //we'll start looking for other hyperlinks one character after the original end of the word, compensating for the characters we added
						continue; //go back and start checking from our new starting index
					}
				}
				//TODO del				else	//if this hyperlink character came at the beginning or ending of the word
				fromIndex = originalWordEnd + 1; //start looking again at the character following this word
				//TODO del					fromIndex=checkIndex+1;	//start looking again at the character following this hyperlink character
			}
			else
				//if we didn't find any more hyperlink characters
				break; //we've found all we could
		}
		return outString; //return the resulting string
	}

	/**
	 * Creates a string containing a range of characters.
	 * @param firstChar The character to start with.
	 * @param lastChar The last character to include, which should have a value higher than or equal to <code>firstChar</code>.
	 * @return A string containing a range of characters including the first and last characters provided.
	 */
	public static String createString(final char firstChar, final char lastChar)
	{
		final StringBuilder stringBuilder = new StringBuilder(lastChar - firstChar + 1); //create a string buffer with enough room to hold the characters
		for(char c = firstChar; c <= lastChar; stringBuilder.append(c++))
			; //append the entire range of characters to the string buffer
		return stringBuilder.toString(); //return the string we constructed
	}

	/**
	 * Creates a string with a given repetition of characters.
	 * @param ch The character to be in the string.
	 * @param count The number of repetitions of the character.
	 * @return A string with count repetitions of ch.
	 */
	public static String createString(final char ch, final int count)
	{
		return StringBuilders.append(new StringBuilder(), ch, count).toString(); //append the characters to a new string builder and return the string version of the result
	}

	/**
	 * Ensures that the given string is the correct length by adding or deleting characters to or from the end.
	 * @param inString The string to process.
	 * @param len The requested length.
	 * @param ch The character to be added to the string, if needed.
	 * @return A string with the correct length.
	 */
	public static String makeStringLength(final String inString, final int len, final char ch) //TODO rename to forceLength
	{
		return makeStringLength(inString, len, ch, -1);
	}

	/**
	 * Ensures that the given string is the correct length by adding or deleting characters to or from the requested position.
	 * @param inString The string to process.
	 * @param len The requested length.
	 * @param ch The character to be added to the string, if needed.
	 * @param pos The position at which to insert or delete characters, or -1 if the end should be used.
	 * @return A string with the correct length.
	 */
	public static String makeStringLength(final String inString, final int len, final char ch, int pos) //TODO rename to forceLength
	{
		final int originalLength = inString.length(); //get the length of the original string
		if(originalLength == len) //if the string is already the correct length
		{
			return inString; //return the string untouched
		}
		else
		//if the string isn't the correct length already
		{
			return StringBuilders.appendForceLength(new StringBuilder(), inString, len, ch, pos).toString(); //append the needed characters to a string builder and return the resulting string
		}
	}

	/**
	 * Removes a character at the specified index.
	 * @param inString the String from which the information will be removed.
	 * @param index The index of the information to remove.
	 * @return A new string with the specified information removed from the specified location.
	 */
	static public String remove(final String inString, final int index)
	{
		return remove(inString, index, 1); //remove one character from the specified location
	}

	/**
	 * Removes several characters at the specified index.
	 * @param inString the String from which the information will be removed.
	 * @param index The index of the information to remove.
	 * @param len The number of characters to remove.
	 * @return A new string with the specified information removed from the specified location.
	 */
	static public String remove(final String inString, final int index, final int len) //TODO now maybe just call replace() with "" for the replacement text
	{
		//TODO what if index is zero? will this still work?
		return inString.substring(0, index) + inString.substring(index + len); //return a string with the specified information removed
	}

	/**
	 * Removes all characters that come after and including the first occurrence of a character in the given delimiter string. If the character does not exist in
	 * the string, the original string will be returned.
	 * @param string The string to check.
	 * @param delimiters The characters that indicate removal should occur.
	 * @return The string with the first matching character and everything after it removed, or the original string if no characters were in the supplied set of
	 *         delimiters.
	 */
	public static String removeAfterFirstChar(String string, final Characters delimiters)
	{
		for(int i = 0; i < string.length(); ++i) //look at each character in the string
		{
			if(delimiters.contains(string.charAt(i))) //if this character is one of our delimiters
				return string.substring(0, i); //return all the characters before this character
		}
		return string; //return the original string if we couldn't find a match
	}

	/**
	 * Removes all characters that come before the last occurrence of the given' character. If the character does not exist in the string, the original string
	 * will be returned.
	 */
	public static String removeBeforeLast(String string, final char c)
	{
		final int lastIndex = string.lastIndexOf(c); //get the last index of the character
		if(lastIndex >= 0) //if the character exists in the string
			string = string.substring(lastIndex + 1); //throw away everything up to and including the character
		return string; //return the string which may or may not have information removed
	}

	/**
	 * Removes the given substring, matched without case sensitivity, and everything following the string.
	 * @param string The string that may contain the substring.
	 * @param substring The string to match without case sensitivity.
	 * @return The string with the substring and everything following it removed, or the original string if no changes were made.
	 */
	public static String removeLengthIgnoreCase(final String string, final String substring) //TODO probably rename truncateIgnoreCase
	{
		final int index = indexOfIgnoreCase(string, substring); //see if the substring appears in the text
		//if the substring is present, remove it and everything following
		return index >= 0 ? string.substring(0, index) : string;
	}

	/**
	 * Trims the last side of the string beginning at the first occurrence of removeChar. If removeChar does not exist in the string, no information is removed.
	 * @param inString the String from which the information will be removed.
	 * @param removeChar The character to remove from the string.
	 * @return A new string with its beginning removed.
	 */
	static public String trimFirstChar(final String inString, final char removeChar)
	{
		return trimFirstChar(inString, removeChar, 1); //trim on the first occurrence of the character
	}

	/**
	 * Trims the left side of the string beginning at the specified occurrence of removeChar from the beginning. If removeChar does not exist in the string the
	 * required number of times, the string will be trimmed at the last occurrence. information is removed.
	 * @param inString the String from which the information will be removed.
	 * @param removeChar The character to remove from the string.
	 * @param occurrence The number of occurrences of the remove character before information should be removed.
	 * @return A new string with its end removed.
	 */
	static public String trimFirstChar(final String inString, final char removeChar, int occurrence)
	{
		int occurrenceIndex = -1; //we'll start looking at the beginning
		for(int i = occurrenceIndex + 1; i < inString.length(); ++i) //look at each character, starting at the end
		{
			if(inString.charAt(i) == removeChar) //if this is the character to remove
			{
				occurrenceIndex = i; //show where the last occurrence took place
				if((--occurrence) == 0) //decrement occurrence; if we've used up all occurrences
				{
					break; //stop searching for a place to trim
				}
			}
		}
		return inString.substring(occurrenceIndex + 1); //remove everything past but not including the last occurrence of the remove character	}
	}

	/**
	 * Trims the right side of the string beginning at the last occurrence of removeChar. If removeChar does not exist in the string, no information is removed.
	 * @param inString the String from which the information will be removed.
	 * @param removeChar The character to remove from the string.
	 * @return A new string with its end removed.
	 */
	static public String trimLastChar(final String inString, final char removeChar)
	{
		return trimLastChar(inString, removeChar, 1); //trim on the first occurrence of the character
	}

	/**
	 * Trims the right side of the string beginning at the specified occurrence of removeChar from the end. If removeChar does not exist in the string the
	 * required number of times, the string will be trimmed at the last occurrence. information is removed.
	 * @param inString the String from which the information will be removed.
	 * @param removeChar The character to remove from the string.
	 * @param occurrence The number of occurrences of the remove character before information should be removed.
	 * @return A new string with its end removed.
	 */
	static public String trimLastChar(final String inString, final char removeChar, int occurrence)
	{
		int occurrenceIndex = inString.length() + 1; //we'll start looking at the end
		for(int i = occurrenceIndex - 1; i >= 0; --i) //look at each character, starting at the end
		{
			if(inString.charAt(i) == removeChar) //if this is the character to remove
			{
				occurrenceIndex = i; //show where the last occurrence took place
				if((--occurrence) != 0) //decrement occurrence; if we've used up all occurrences
				{
					break; //stop searching for a place to trim
				}
			}
		}
		return inString.substring(0, occurrenceIndex); //remove everything up to but not including the last occurrence of the remove character
	}

	/**
	 * Removes several characters at the specified index and replaces them with the given string.
	 * @param inString the String from which the information will be removed.
	 * @param index The index of the information to remove.
	 * @param len The number of characters to remove.
	 * @param replaceString The string of characters to put in the place of the removed characters.
	 * @return A new string with the specified information removed from the specified location.
	 */
	static public String replace(final String inString, final int index, final int len, final String replaceString)
	{
		return inString.substring(0, index) + replaceString + inString.substring(index + len); //return a string with the specified information removed
	}

	/**
	 * Replaces every occurrence of a specified character with a string.
	 * @param inString the String from in the information will be replaced.
	 * @param replaceChar The character to replace.
	 * @param withString The string that will replace replaceChar.
	 * @return A new string with the specified information replaced.
	 */
	static public String replace(final String inString, final char replaceChar, final String withString) //TODO this can be made more efficient with string buffers
	{
		String outString = inString; //this is the string we'll process
		int i = 0; //start at the beginning of the string
		while(i < outString.length()) //keep going until we reach the end of the string
		{
			if(outString.charAt(i) == replaceChar) //if we have found a character to replace
			{
				outString = remove(outString, i); //remove this character from the string, which will mean that i is now at the next character
				outString = insert(outString, i, withString); //insert the string at the same index, putting i at the beginning of the inserted string
				i += withString.length(); //skip to the character just after the string we just inserted
			}
			else
				//if we're not at a character to replace
				++i; //go to the next character in the string
		}
		return outString; //return the processed string
	}

	/**
	 * Replaces every occurrence of a specified string with another string.
	 * @param inString the String from in the information will be replaced.
	 * @param replaceString The string to replace.
	 * @param withString The string that will replace replaceString.
	 * @return A new string with the specified information replaced.
	 */
	public static String replace(final String inString, final String replaceString, final String withString)
	{
		final StringBuilder stringBuilder = new StringBuilder(inString); //create a string builder from the string
		StringBuilders.replace(stringBuilder, replaceString, withString); //replace the contents of the string builder
		return stringBuilder.toString(); //return the string in which we replaced the charactersz
	}

	/**
	 * Replaces any of several matching characters with a particular character.
	 * @param inString The string in which characters should be replaced.
	 * @param matchCharacters The string containing characters to be matched; every character that matches one of these characters will be replaced with the
	 *          replacement character.
	 * @param replacementChar The character to replace any matched character.
	 * @return A string with the appropriate characters replaced by the replacement character.
	 */
	public static String replace(final String inString, final Characters matchCharacters, final char replacementChar)
	{
		final StringBuilder outStringBuilder = new StringBuilder(inString); //the output string will be identical in length to the input string, because we're replacing characters with characters
		//replace the characters in the string builder; if there were actually any replacements made
		if(StringBuilders.replace(outStringBuilder, matchCharacters, replacementChar) > 0)
		{
			return outStringBuilder.toString(); //return the new string
		}
		else
		//if no replacements were made
		{
			return inString; //just return the original string, which should be faster than converting the string buffer to a string
		}
	}

	/**
	 * Replaces any of several matching characters with a particular string.
	 * @param inString The string in which characters should be replaced.
	 * @param matchCharacters The string containing characters to be matched; every character that matches one of these characters will be replaced with the
	 *          replacement string.
	 * @param replacementString The string to replace any matched character.
	 * @return A string with the appropriate characters replaced by the replacement string.
	 */
	public static String replace(final String inString, final Characters matchCharacters, final String replacementString)
	{
		final StringBuilder outStringBuilder = new StringBuilder(inString); //the output string will be identical in length to the input string, because we're replacing characters with characters
		//replace the characters in the string builder; if there were actually any replacements made
		if(StringBuilders.replace(outStringBuilder, matchCharacters, replacementString) > 0)
		{
			return outStringBuilder.toString(); //return the new string
		}
		else
		//if no replacements were made
		{
			return inString; //just return the original string, which should be faster than converting the string buffer to a string
		}
	}

	/**
	 * Replaces each matching character with the corresponding replacement string.
	 * @param string The string in which the replacements will be made.
	 * @param matchChars An array of characters to be replaced.
	 * @param replacementStrings An array of strings to replace the characters appearing at the same indexes as those in <var>matchChars</var>.
	 * @return The string with replacements made, which may be the original string if no replacements were made.
	 */
	public static String replace(final String string, final char[] matchChars, final String[] replacementStrings)
	{
		//first see if this string contains one of the match characters
		//we assume that most strings will not contain a match character, so we won't have to do any replacements
		//letting the string do the per-character search is much faster than using String.charAt()
		for(final char matchChar : matchChars) //for each character to match
		{
			if(string.indexOf(matchChar) >= 0) //if the string contains this match character
			{
				final StringBuilder stringBuilder = new StringBuilder(string); //create a new string buffer with the given text
				StringBuilders.replace(stringBuilder, matchChars, replacementStrings); //do the replacement on the buffer
				return stringBuilder.toString(); //convert the results to a string and return it
			}
		}
		return string; //if there are no matching match characters in the string, there's nothing to replace, so just return the original string
	}

	/**
	 * Truncates the string, if needed, to ensure that the string is no longer than the provided length.
	 * @param string The string to truncate.
	 * @param maxLength The maximum length of the string; if the string is longer than the given length, it will be truncated.
	 * @return The string, if the string was shorter than or equal to the maximum length; otherwise, the first <code>maxLength</code> characters of the string.
	 */
	public static String truncate(final String string, final int maxLength)
	{
		return string.length() < maxLength ? string : string.substring(0, maxLength); //if the string is too long, use only the first maxLength characters
	}

	/**
	 * Removes everything after and including the first occurrence of one of the given characters.
	 * @param string The string that may contain one or more of the characters.
	 * @param delimiters The characters that will cause
	 * @return The string with the first occuring character and everything after it removed, or the original string if no changes were made.
	 */
	public static String truncateChar(final String string, final Characters delimiters)
	{
		final int index = CharSequences.charIndexOf(string, delimiters); //find the first occurrence of one of the characters
		//if one of the characters is present, remove it and everything following
		return index >= 0 ? string.substring(0, index) : string;
	}

	/**
	 * Collapses every run of any number of collapseChars to a single replacement string.
	 * @param inString the String in which the information will be collapsed.
	 * @param collapseChars The characters to be removed from the string.
	 * @param replaceString The string which will replace the collapseChars.
	 * @return A new string with the specified information collapsed.
	 */
	static public String collapseEveryChar(final String inString, final Characters collapseChars, final String replaceString)
	{
		if(CharSequences.charIndexOf(inString, collapseChars) >= 0) //first search the string to see if we would replace something; if so
		{
			final StringBuffer stringBuffer = new StringBuffer(inString); //create a new string buffer from the string
			StringBuffers.collapse(stringBuffer, collapseChars, replaceString); //collapse the characters
			return stringBuffer.toString(); //convert the string buffer back to a string and return it
		}
		else
			//if there are no characters to collapse
			return inString; //return the original string
	}

	/**
	 * Trims the specified delimiters from the beginning and end of the string.
	 * @param inString The string to be processed.
	 * @param delimiters The string containing delimiter characters.
	 */
	static public String trim(final String inString, final Characters delimiters) //TODO call the StringBuffer version---or maybe not---this may be more efficient
	{
		int beginIndex, endIndex;
		final int length = inString.length(); //get the length of the original string
		for(beginIndex = 0; beginIndex < length && delimiters.contains(inString.charAt(beginIndex)); ++beginIndex)
			; //find the first non-delimiter in the string
		for(endIndex = length; endIndex > beginIndex && delimiters.contains(inString.charAt(endIndex - 1)); --endIndex)
			; //find the last non-delimiter in the string
		if(beginIndex > 0 || endIndex < length) //if there is something to trim
			return inString.substring(beginIndex, endIndex); //return the substring minus the beginning and ending delimiters
		else
			//if there is nothing to trim
			return inString; //return the original string
	}

	/**
	 * Trims whitespace, including the Unicode no-break space character 0x00A0, from the beginning and end of the string.
	 * @param inString The string to be processed.
	 */
	static public String trimWhitespaceNoBreak(final String inString) //TODO update with our new Unicode 4.x constants
	{
		final int length = inString.length(); //get the string's length
		int beginIndex, endIndex;
		//find the first non-whitespace character in the string
		for(beginIndex = 0; beginIndex < length; ++beginIndex) //look at each character
		{
			final char c = inString.charAt(beginIndex); //get the character at this index
			if(!Character.isWhitespace(c) && c != NO_BREAK_SPACE_CHAR) //if this is not whitespace or a non-breaking space
				break; //stop looking for non-whitespace
		}
		//find the last non-whitespace character in the string
		for(endIndex = length; endIndex > beginIndex; --endIndex)
		{
			final char c = inString.charAt(endIndex - 1); //get the character at the previous index
			if(!Character.isWhitespace(c) && c != NO_BREAK_SPACE_CHAR) //if this is not whitespace or a non-breaking space
				break; //stop looking for non-whitespace
		}
		if(beginIndex > 0 || endIndex < length) //if there is something to trim
			return inString.substring(beginIndex, endIndex); //return the substring minus the beginning and ending delimiters
		else
			//if there is nothing to trim
			return inString; //return the original string
	}

	/**
	 * Trims whitespace, including the Unicode no-break space character 0x00A0, from the beginning of the string.
	 * @param inString The string to be processed.
	 */
	static public String trimWhitespaceNoBreakBeginning(final String inString)
	{
		final int length = inString.length(); //get the string's length
		int beginIndex;
		//find the first non-whitespace character in the string
		for(beginIndex = 0; beginIndex < length; ++beginIndex) //look at each character
		{
			final char c = inString.charAt(beginIndex); //get the character at this index
			if(!Character.isWhitespace(c) && c != NO_BREAK_SPACE_CHAR) //if this is not whitespace or a non-breaking space
				break; //stop looking for non-whitespace
		}
		if(beginIndex > 0) //if there is something to trim
			return inString.substring(beginIndex); //return the substring minus the beginning
		else
			//if there is nothing to trim
			return inString; //return the original string
	}

	/**
	 * Trims whitespace, including the Unicode no-break space character 0x00A0, from the beginning of the string.
	 * @param inString The string to be processed.
	 */
	static public String trimWhitespaceNoBreakEnd(final String inString)
	{
		final int length = inString.length(); //get the string's length
		int endIndex;
		//find the last non-whitespace character in the string
		for(endIndex = length; endIndex > 0; --endIndex)
		{
			final char c = inString.charAt(endIndex - 1); //get the character at the previous index
			if(!Character.isWhitespace(c) && c != NO_BREAK_SPACE_CHAR) //if this is not whitespace or a non-breaking space
				break; //stop looking for non-whitespace
		}
		if(endIndex < length) //if there is something to trim
			return inString.substring(0, endIndex); //return the substring minus the ending delimiters
		else
			//if there is nothing to trim
			return inString; //return the original string
	}

	/**
	 * If the input string begins with the specifid string, trims that string.
	 * @param inString The string to be processed.
	 * @param beginString The string to be trimmed, if it appears at the beginning of inString.
	 * @return The string, trimmed if needed.
	 */
	static public String trimBeginning(final String inString, final String beginString)
	{
		return inString.startsWith(beginString) ? //if the string begins with beginString
		inString.substring(beginString.length())
				: //trim the string
				inString; //if the string doesn't begin with beginString, return the string itself
	}

	/**
	 * If the input string ends with the specifid string, trims that string.
	 * @param inString The string to be processed.
	 * @param beginString The string to be trimmed, if it appears at the end of inString.
	 * @return The string, trimmed if needed.
	 */
	static public String trimEnd(final String inString, final String endString)
	{
		return inString.endsWith(endString) ? //if the string ends with beginString
		inString.substring(0, inString.length() - endString.length())
				: //trim the string
				inString; //if the string doesn't end with endString, return the string itself
	}

	/**
	 * Performs word-wrapping on the input string by strategically inserting ends-of-line, ensuring that each line is no longer than the specified length.
	 * @param inString The string to be wrapped.
	 * @param wrapLength The maximum length of any line.
	 * @return The string wrapped with the given characters inserted.
	 */
	static public String wrap(final String inString, final int wrapLength)
	{
		return wrap(inString, wrapLength, UNDEFINED_CHAR, '\n'); //wrap the string using newlines
	}

	/**
	 * Performs word-wrapping on the input string by strategically inserting ends-of-line, ensuring that each line is no longer than the specified length.
	 * @param inString The string to be wrapped.
	 * @param wrapLength The maximum length of any line.
	 * @param padChar The character to use to pad each line, or <code>UNDEFINED_CHAR</code> if lines should not be padded.
	 * @param eolChar The string to insert at the end of each line, or <code>UNDEFINED_CHAR</code> if ends of lines should not be marked.
	 * @return The string wrapped with the given characters inserted.
	 */
	static public String wrap(final String inString, final int wrapLength, final char padChar, final char eolChar)
	{
		//TODO del Log.trace("inside wrap() with string: "+inString+", length: "+wrapLength);	//TODO del
		final StringBuffer outStringBuffer = new StringBuffer(inString.length()); //create a new string buffer that's at least as long as the input string
		int lineBeginIndex = 0; //this will keep track of the start of each line
		while(lineBeginIndex < inString.length()) //while we haven't reached the end of the input string
		{
			//TODO del Log.trace("Loop iteration, lineBeginIndex: "+lineBeginIndex);	//TODO del
			int lineEndIndex = lineBeginIndex + wrapLength; //we'll assume that we can't find any character on which to break, which will mean we'll have to force a break at the maximum length of the line
			if(lineEndIndex > inString.length()) //if we went past the end of the string
				lineEndIndex = inString.length(); //there's no need to wrap -- the end of the string is shorting than our wrapping length
			else
			//if there are characters that need wrapped
			{
				for(int i = lineEndIndex - 1; i >= lineBeginIndex; --i) //look at each character from the maximum end of the line to the beginning
				{
					if(isWordWrap(inString.charAt(i))) //if we can wrap on this character
					{
						lineEndIndex = i + 1; //we'll wrap right after this character
						break; //stop searching for more wrapping characters on this line
					}
				}
			}
			outStringBuffer.append(inString.substring(lineBeginIndex, lineEndIndex)); //add this line to our string buffer
			if(lineEndIndex < inString.length()) //if we're not at the end of the input string (i.e. don't add a newline return a the end of the string)
			{
				if(padChar != UNDEFINED_CHAR) //if we have a pad character
				{
					for(int padCount = wrapLength - (lineEndIndex - lineBeginIndex); padCount > 0; --padCount) //add the correct number of pad characters
					{
						outStringBuffer.append(padChar); //add the pad character
					}
				}
				if(eolChar != UNDEFINED_CHAR) //if we have a character to mark the end of the line
					outStringBuffer.append(eolChar); //add an end-of-line character
			}
			lineBeginIndex = lineEndIndex; //start looking at the next line
		}
		return outStringBuffer.toString(); //return the string from the we constructed
	}

	/**
	 * Writes an object to a string using the given I/O support, converting bytes to a string using the {@value CharacterEncoding#UTF_8} encoding.
	 * @param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	 * @param object The object to write to a string.
	 * @param io The I/O support for writing the object.
	 * @throws IOException if there is an error writing the data.
	 * @throws UnsupportedEncodingException if the {@value CharacterEncoding#UTF_8} character encoding is not supported.
	 */
	public static <T> String write(final URI baseURI, final T object, final IO<T> io) throws IOException
	{
		return write(baseURI, object, io, UTF_8_CHARSET); //write and convert to a string using UTF_8
	}

	/**
	 * Writes an object to a string using the given I/O support.
	 * @param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	 * @param object The object to write to a string.
	 * @param io The I/O support for writing the object.
	 * @param charset The encoding with which to interpret the written bytes.
	 * @throws IOException if there is an error writing the data.
	 * @throws UnsupportedEncodingException if the named character encoding is not supported.
	 */
	public static <T> String write(final URI baseURI, final T object, final IO<T> io, final Charset charset) throws IOException
	{
		final ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream(); //create a new byte array output stream
		try
		{
			io.write(byteArrayOutputStream, baseURI, object); //write the object, determining the base URI from the file
		}
		finally
		{
			byteArrayOutputStream.close(); //always close the output stream
		}
		return byteArrayOutputStream.toString(charset.name()); //convert the byte array to a string using the given encoding
	}

}
