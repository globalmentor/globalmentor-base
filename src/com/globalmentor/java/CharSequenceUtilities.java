package com.globalmentor.java;

import java.io.UnsupportedEncodingException;

import static com.garretwilson.text.Characters.*;
import static com.garretwilson.text.CharacterEncoding.*;
import static com.globalmentor.java.CharacterUtilities.*;

import com.garretwilson.util.ArrayUtilities;

/**Various text manipulating functions. These methods work on
	objects that implement the <code>CharacterSequence</code> interface.
	To avoid creation of new strings, some of these methods should
	be avoided in favor of their corresponding <code>StringBufferUtilities</code>
	methods, which operate on <code>StringBuffer</code> objects.
@see StringBufferUtilities
@author Garret Wilson
*/
public class CharSequenceUtilities
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
	public static boolean containsLetter(final CharSequence charSequence) //G***maybe change this to indexOfLetterOrDigit
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
	*/
	public static boolean containsLetterOrDigit(final CharSequence charSequence) //G***maybe change this to indexOfLetterOrDigit
	{
		for(int i=charSequence.length()-1; i>=0; --i) //look at each character in the string
		{
			if(Character.isLetterOrDigit(charSequence.charAt(i)))  //if this is a letter or digit
				return true;  //we found a letter or digit
		}
		return false; //we found no letters or digits
	}

	/**Determines if a character sequence contains whitespace.
	@param charSequence The character sequence to be searched.
	@return <code>true</code> if the given character sequence contains whitespace.
	@see Characters#WHITESPACE_CHARS
	*/
	public static boolean containsWhitespace(final CharSequence charSequence)
	{
		return containsChar(charSequence, WHITESPACE_CHARS);	//see if the character sequence contains whitespace
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

	/**Counts the number of occurences of any one of given characters in a character sequence.
	@param charSequence The character sequence to examine.
	@param characters The scharacter to count.
	@return The number of occurences of the characters in the character sequence.
	*/
/*TODO fix
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
*/

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
	Characters are assumed to be valid unless specified otherwise.
	@param charSequence The data to escape.
	@param validCharacters The characters that should not be escaped and all others should be escaped, or <code>null</code> if characters should not be matched against valid characters.
	@param invalidCharacters The characters that, if they appear, should be escaped, or <code>null</code> if characters should not be matched against invalid characters.
	@param escapeChar The character to prefix the hex representation.
	@param length The number of characters to use for the hex representation.
	@return A string containing the escaped data.
	@exception IllegalArgumentException if neither valid nor invalid characters are given.
	*/
	public static String escapeHex(final CharSequence charSequence, final String validCharacters, final String invalidCharacters, final char escapeChar, final int length)	//TODO del if not needed; more specialized processing is needed for URIs, such as UTF-8 conversion
	{
		try
		{
			final byte[] charBytes=charSequence.toString().getBytes(UTF_8);	//get the UTF-8 bytes of the string
			final StringBuilder stringBuilder=new StringBuilder(charBytes.length+16);	//create a new string builder to hold the result, reserving some extra characters
			for(byte charByte:charBytes)	//look at each character's byte in the sequence
			{
				final char c=(char)charByte;	//look at this UTF-8 byte as a character
				final boolean encode=c==escapeChar	//always encode the escape character
						|| (validCharacters!=null && validCharacters.indexOf(c)<0)	//encode if there is a list of valid characters and this character is not one of them
						|| (invalidCharacters!=null && invalidCharacters.indexOf(c)>=0);	//encode if there is a list of invalid characters and this character is one of them
				if(encode)	//if this a character to escape
				{
						//append the escape character, along with a two-digit representation of the character value
					stringBuilder.append(escapeChar).append(IntegerUtilities.toHexString(c, length).toUpperCase());
				}
				else	//if this is not a character to escape
				{
					stringBuilder.append(c);	//add this character to the result without escaping it
				}	
			}
			return stringBuilder.toString();	//return the result we constructed
		}
		catch(final UnsupportedEncodingException unsupportedEncodingException)	//the JVM should always know how to convert a string to UTF-8
		{
			throw new AssertionError(unsupportedEncodingException);
		}
	}

	/**Decodes the escaped characters in the character iterator by
		converting the hex value after each occurrence of the escape
		character to the corresponding Unicode character. 
	@param charSequence The data to unescape.
	@param escapeChar The character that prefixes the hex representation.
	@param length The number of characters used for the hex representation.
	@return A string containing the unescaped data.
	*/
	public static String unescapeHex(final CharSequence charSequence, final char escapeChar, final int length)	//TODO del if not needed; more specialized processing is needed for URIs, such as UTF-8 conversion
	{
		final StringBuilder stringBuilder=new StringBuilder();	//create a new string builder to hold the result
		for(int i=0; i<charSequence.length(); ++i)	//look at each character in the sequence
		{
			final char c=charSequence.charAt(i);	//get a reference to this character
				//if this is the beginning of an escaped character, and there's room for enough hex characters after it (the last test is lenient, throwing no exception if the escape character doesn't actually encode anything) 
			if(c==escapeChar && i<charSequence.length()-length)
			{
				try
				{
						//convert the next two hex characters to a single character value and add it to the string buffer
					stringBuilder.append((char)Integer.parseInt(charSequence.subSequence(i+1, i+length+1).toString(), 16));
					i+=length;	//skip the escape sequence (we'll go to the last character, and we'll be advanced one character when we go back to the start of the loop)
				}
				catch(NumberFormatException numberFormatException)	//if the characters weren't really hex characters
				{
					stringBuilder.append(c);	//we'll assume this wasn't an escape character after all, and add it normally (this is really lenient; this method could be written to throw an exception)
				}
			} 
			else	//if this is not an escaped character
			{
				stringBuilder.append(c);	//add this character to the result with no change
			}	
		}
		return stringBuilder.toString();	//return the result we constructed
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
	
	/**Searches a character sequence and returns the first index of any character in the specified array, starting at the beginning.
	@param charSequence The character sequence to be searched.
	@param characters The characters to check.
	@return The index of the first occurrence of one of the supplied characters, or -1 if none were found.
	*/
	public static int indexOf(final CharSequence charSequence, final char[] characters)
	{
		return indexOf(charSequence, characters, 0);	//look of the characters, starting at the beginning of the string
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
			if(ArrayUtilities.contains(characters, charSequence.charAt(i)))	//if this character is in our character string
				return i;	//return the index we're at
		}
		return -1;	//if we make it to here, we didn't find any of the characters
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
	@param <code>true</code> if there are no other characters but the specified
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
	@param <code>true</code> if there are no other characters but the specified
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
	public final static boolean isNumber(final CharSequence charSequence) //G***use a regex, and verify format
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

	/**Splits a characters sequence into subsequences based upon the given delimiter.
	Subsequences will be returned between delimiters even if they are empty, and
		a subsequence will be returned after the last delimiter, even if there are no
		remaining characters. In other words, the number of character subsequences returned
		is <var>delimiterCount</var>+1.
	@param charSequence The character sequence to split.
	@param delimiter The delimiter to use for splitting.
	@return An array of character subsequences between the delimiters.
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

	/**Returns a string builder with the given character sequence content.
	If the given character sequence is a string builder, it will be returned; otherwise, a setring builder will be created.
	@param charSequence The character sequence containing the content for a string builder.
	@return given The character sequence, if it is a string builder, or a new string builder created from the character sequence.
	*/
/*TODO del; this may be dangerous if a calling method doesn't realize the character sequence will be modified when this is used to change a passed character sequence
	public static StringBuilder toStringBuilder(final CharSequence charSequence)
	{
		return charSequence instanceof StringBuilder ? (StringBuilder)charSequence : new StringBuilder(charSequence);	//only create a new string builder if we need to
	}
*/

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
