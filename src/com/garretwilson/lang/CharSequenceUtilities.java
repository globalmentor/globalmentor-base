package com.garretwilson.lang;

import java.io.UnsupportedEncodingException;

import static com.garretwilson.lang.CharacterUtilities.*;
import com.garretwilson.text.CharacterEncodingConstants;
import static com.garretwilson.text.CharacterConstants.*;

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
		in the specified string, starting at <code>fromIndex</code>.
	@param charSequence The character sequence to be searched.
	@param charString The string of characters to check.
	@param fromIndex The index to search from
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
		character, starting from <code>fromIndex</code>.
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
		character in the specified string, starting from <code>fromIndex</code>.
	@param charSequence The character sequence to be searched.
	@param charString The string of characters to check.
	@param fromIndex The index to search from
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
	static public boolean containsLetterOrDigit(final CharSequence charSequence) //G***maybe change this to indexOfLetterOrDigit
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
	@see CharacterConstants#WHITESPACE_CHARS
	*/
	public static boolean containsWhitespace(final CharSequence charSequence)
	{
		return containsChar(charSequence, WHITESPACE_CHARS);	//see if the character sequence contains whitespace
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

	/**Escapes the indicated characters in the character iterator
		using the supplied escape character.
	Every matching character is converted to its Unicode hex equivalent
		and prefixed with the given escape character.
	<p>As an example, the URI encoding rules in
		<a href="http://www.ietf.org/rfc/rfc2396.txt">RFC 2396</a>,
		"Uniform Resource Identifiers (URI): Generic Syntax" would use
		<code>escapeHex(charSequence, ";/?:@&=+$,", '%', 2);</code>.</p>
	@param charSequence The data to escape.
	@param encodeCharacters The characters that, if they appear, should be escaped.
	@param escapeChar The character to prefix the hex representation.
	@param length The number of characters to use for the hex representation.
	@return A string containing the escaped data.
	*/
	public static String escapeHex(final CharSequence charSequence, final String encodeCharacters, final char escapeChar, final int length)
	{
		final StringBuffer stringBuffer=new StringBuffer();	//create a new string buffer to hold the result
		for(int i=0; i<charSequence.length(); ++i)	//look at each character in the sequence
		{
			final char c=charSequence.charAt(i);	//get a reference to this character
			if(c==escapeChar || encodeCharacters.indexOf(c)>=0)	//if this a character to escape
			{
					//append the escape character, along with a two-digit representation of the character value
				stringBuffer.append(escapeChar).append(IntegerUtilities.toHexString(c, length).toUpperCase());
			}
			else	//if this is not a character to escape
			{
				stringBuffer.append(c);	//add this character to the result without escaping it
			}	
		}
		return stringBuffer.toString();	//return the result we constructed
	}

	/**Decodes the escaped characters in the character iterator by
		converting the hex value after each occurrence of the escape
		character to the corresponding Unicode character. 
	<p>For example, to decode a URI according to the URI encoding rules
		in <a href="http://www.ietf.org/rfc/rfc2396.txt">RFC 2396</a>,
		"Uniform Resource Identifiers (URI): Generic Syntax", one would
		use <code>unescapeHex(charSequence, '%', 2)</code>.</p>
	@param charSequence The data to unescape.
	@param escapeChar The character that prefixes the hex representation.
	@param length The number of characters used for the hex representation.
	@return A string containing the unescaped data.
	*/
	public static String unescapeHex(final CharSequence charSequence, final char escapeChar, final int length)
	{
		final StringBuffer stringBuffer=new StringBuffer();	//create a new string buffer to hold the result
		for(int i=0; i<charSequence.length(); ++i)	//look at each character in the sequence
		{
			final char c=charSequence.charAt(i);	//get a reference to this character
				//if this is the beginning of an escaped character, and there's room for enough hex characters after it (the last test is lenient, throwing no exception if the escape character doesn't actually encode anything) 
			if(c==escapeChar && i<charSequence.length()-length)	
			{
				try
				{
						//convert the next two hex characters to a single character value and add it to the string buffer
					stringBuffer.append((char)Integer.parseInt(charSequence.subSequence(i+1, i+length+1).toString(), 16));
					i+=length;	//skip the escape sequence (we'll go to the last character, and we'll be advanced one character when we go back to the start of the loop)
				}
				catch(NumberFormatException numberFormatException)	//if the characters weren't really hex characters
				{
					stringBuffer.append(c);	//we'll assume this wasn't an escape character after all, and add it normally (this is really lenient; this method could be written to throw an exception)
				}
			} 
			else	//if this is not an escaped character
			{
				stringBuffer.append(c);	//add this character to the result with no change
			}	
		}
		return stringBuffer.toString();	//return the result we constructed
	}

	/**Determines the first index of the given character.
	@param charSequence The character sequence to check.
	@param character The character to search for.
	@return The index of the first occurrence of the given character, or -1 if
		the character was not found.
	*/
	public static int indexOf(final CharSequence charSequence, final char character)
	{
		final int length=charSequence.length();
		for(int i=0; i<length; ++i)	//look at each character
		{
			if(charSequence.charAt(i)==character)	//if this character matches
			{
				return i;	//return the matching index
			}
		}
		return -1;	//show that we couldn't find the character
	}

	/**Searches a character sequence and returns the first index of any character
		<em>not</em> in the specified string, starting from the beginning.
	@param charSequence The character sequence to be searched.
	@param notCharString The string of characters to check.
	@return The index of the first occurrence of one of the supplied characters,
		or -1 if none were found.
	*/
	static public int notCharIndexOf(final CharSequence charSequence, final String notCharString)
	{
		return notCharIndexOf(charSequence, notCharString, 0);  //start looking from the beginning
	}

	/**Searches a character sequence and returns the first index of any character
		<em>not</em> in the specified string, starting at <code>fromIndex</code>.
	@param charSequence  The character sequence to be searched.
	@param notCharString The string of characters to check.
	@param fromIndex The index to search from
	@return The index of the first occurrence of one of the supplied characters,
		or -1 if none were found.
	*/
	static public int notCharIndexOf(final CharSequence charSequence, final String notCharString, final int fromIndex)
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
	static public int notCharLastIndexOf(final CharSequence charSequence, final String notCharString)
	{
		return notCharLastIndexOf(charSequence, notCharString, charSequence.length()-1);  //start searching from the end
	}

	/**Searches a character sequence and returns the last index of any character
		<em>not</em> in the specified string, starting at <code>fromIndex</code>.
	@param charSequence The character sequence to be searched.
	@param notCharString The string of characters to check.
	@param fromIndex The index to search from
	@return The index of the last occurrence of one of the supplied characters,
		or -1 if none were found.
	*/
	static public int notCharLastIndexOf(final CharSequence charSequence, final String notCharString, final int fromIndex)
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

	/**Determines if the character sequence starts with the given character.
	@param charSequence The character sequence to examine.
	@param character The character to compare.
	@return <code>true</code> if the first character of the character sequence
		matches that of the given string.
	*/
	public static boolean startsWith(final CharSequence charSequence, final char character)
	{
			//see if the character sequence has at least one character, and the first character matches our character
		return charSequence.length()>0 && charSequence.charAt(0)==character;
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

}
