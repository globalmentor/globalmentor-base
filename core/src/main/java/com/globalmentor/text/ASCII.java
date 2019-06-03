/*
 * Copyright © 2012-2017 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.text;

import static com.globalmentor.java.CharSequences.*;
import static java.lang.Math.*;

import com.globalmentor.java.Characters;

/**
 * Constants and utilities for working with US_ASCII.
 * @author Garret Wilson
 */
public class ASCII {

	/** The value of the first uppercase letter. */
	public static final short UPPERCASE_LETTER_FIRST = 'A';
	/** The value of the last uppercase letter. */
	public static final short UPPERCASE_LETTER_LAST = 'Z';

	/** The value of the first lowercase letter. */
	public static final short LOWERCASE_LETTER_FIRST = 'a';
	/** The value of the last lowercase letter. */
	public static final short LOWERCASE_LETTER_LAST = 'z';

	/** The difference in value between lowercase (higher values) and uppercase (lower values) ASCII letters. */
	public static final short LOWERCASE_UPPERCASE_LETTER_DIFFERENCE = LOWERCASE_LETTER_FIRST - UPPERCASE_LETTER_FIRST;

	/** The highest ASCII code point value that is a control character. */
	public static final char MAX_CONTROL_VALUE = 31;

	/** The highest ASCII code point value. */
	public static final char MAX_VALUE = 127;

	/** The ASCII digit characters <code>'0'</code> – <code>'9'</code>. */
	public static final Characters DIGIT_CHARACTERS = Characters.ofRange('0', '9');

	/**
	 * The ASCII hexadecimal digit characters <code>'0'</code> – <code>'9'</code>, <code>'a'</code> – <code>'f'</code>, and <code>'A'</code> – <code>'F'</code>.
	 * @apiNote Note that this definition includes both lowercase and uppercase letters.
	 */
	public static final Characters HEX_CHARACTERS = DIGIT_CHARACTERS.addRange('a', 'f').addRange('A', 'F');

	/**
	 * Indicates whether a given character is within the ASCII range.
	 * @param c The character to test.
	 * @return <code>true</code> if the given character is not greater than {@value #MAX_VALUE}.
	 * @see #MAX_VALUE
	 */
	public static boolean isASCII(final char c) {
		return c <= MAX_VALUE;
	}

	/**
	 * Indicates whether a given character is within the ASCII range.
	 * @param c The character to test.
	 * @return <code>true</code> if the given character is not negative and not greater than {@value #MAX_VALUE}.
	 * @see #MAX_VALUE
	 */
	public static boolean isASCII(final int c) {
		return c >= 0 && c <= MAX_VALUE;
	}

	/**
	 * Determines whether a given character sequence contains only ASCII characters.
	 * @param charSequence The character sequence to test.
	 * @return <code>true</code> if no character(s) in the character sequence are outside the ASCII range.
	 * @see #isASCII(char)
	 */
	public static boolean isASCII(final CharSequence charSequence) {
		for(int i = charSequence.length() - 1; i >= 0; --i) {
			if(!isASCII(charSequence.charAt(i))) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Indicates whether a given character is within the ASCII control range.
	 * @param c The character to test.
	 * @return <code>true</code> if the given character is not greater than {@value #MAX_CONTROL_VALUE}.
	 * @see #MAX_CONTROL_VALUE
	 */
	public static boolean isASCIIControl(final char c) {
		return c <= MAX_CONTROL_VALUE;
	}

	/**
	 * Indicates whether a given character is within the ASCII control range.
	 * @param c The character to test.
	 * @return <code>true</code> if the given character is not negative and not greater than {@value #MAX_VALUE}.
	 * @see #MAX_CONTROL_VALUE
	 */
	public static boolean isASCIIControl(final int c) {
		return c >= 0 && c <= MAX_CONTROL_VALUE;
	}

	/**
	 * Determines whether a given character sequence contains only non-control ASCII characters.
	 * @param charSequence The character sequence to test.
	 * @return <code>true</code> if no character(s) in the character sequence are outside the ASCII range of non-control characters.
	 * @see #isASCII(char)
	 * @see #isASCIIControl(char)
	 */
	public static boolean isASCIINonControl(final CharSequence charSequence) {
		for(int i = charSequence.length() - 1; i >= 0; --i) {
			final char c = charSequence.charAt(i);
			if(!isASCII(c) || isASCIIControl(c)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Determines whether the given character is an ASCII lowercase character.
	 * @apiNote This method is typically used for formal language token manipulation, not general human language text processing.
	 * @param c The character to check.
	 * @return Whether the given character is an ASCII lowercase character.
	 */
	public static boolean isLowerCase(final char c) {
		return c >= LOWERCASE_LETTER_FIRST && c <= LOWERCASE_LETTER_LAST;
	}

	/**
	 * Determines whether the given character is an ASCII uppercase character.
	 * @apiNote This method is typically used for formal language token manipulation, not general human language text processing.
	 * @param c The character to check.
	 * @return Whether the given character is an ASCII uppercase character.
	 */
	public static boolean isUpperCase(final char c) {
		return c >= UPPERCASE_LETTER_FIRST && c <= UPPERCASE_LETTER_LAST;
	}

	/**
	 * Searches the given character sequence for one of the given tokens, separated by the given delimiters, without regard to ASCII case.
	 * @param charSequence The character sequence to search.
	 * @param delimiters The delimiters to skip.
	 * @param tokens The tokens for which to check.
	 * @return The <code>true</code> if one of the given tokens was found.
	 * @throws NullPointerException if the given character sequence, delimiters, and/or tokens is <code>null</code>.
	 */
	public static boolean containsTokenIgnoreCase(final CharSequence charSequence, final Characters delimiters, final CharSequence... tokens) {
		return getTokenIgnoreCase(charSequence, delimiters, tokens) != null;
	}

	/**
	 * Searches the given character sequence for one of the given tokens, separated by the given delimiters, without regard to ASCII case.
	 * @param charSequence The character sequence to search.
	 * @param delimiters The delimiters to skip.
	 * @param tokens The tokens for which to check.
	 * @return The token that was found, or <code>null</code> if no token was found.
	 * @throws NullPointerException if the given character sequence, delimiters, and/or tokens is <code>null</code>.
	 */
	public static CharSequence getTokenIgnoreCase(final CharSequence charSequence, final Characters delimiters, final CharSequence... tokens) {
		final int length = charSequence.length();
		for(int i = 0; i < length; ++i) { //look through the sequence
			if(delimiters.contains(charSequence.charAt(i))) { //skip delimiters
				continue;
			}
			for(final CharSequence token : tokens) { //look at each token
				if(equalsIgnoreCase(token, charSequence, i, i + token.length())) { //if this token equals the characters starting at the current position
					return token;
				}
			}
		}
		return null;
	}

	/**
	 * Searches the given character sequence for the given tokens, separated by the given delimiters, without regard to ASCII case, and removes all occurrences
	 * that are found. The original character sequence is not modified.
	 * @param charSequence The character sequence to search.
	 * @param delimiters The delimiters to skip.
	 * @param tokens The tokens for which to check.
	 * @return A character sequence with the tokens removed.
	 * @throws NullPointerException if the given character sequence, delimiters, and/or tokens is <code>null</code>.
	 */
	public static CharSequence removeTokensIgnoreCase(final CharSequence charSequence, final Characters delimiters, final CharSequence... tokens) {
		final int end = charSequence.length();
		for(int i = 0; i < end; ++i) { //look through the sequence
			if(delimiters.contains(charSequence.charAt(i))) { //skip delimiters
				continue;
			}
			for(final CharSequence token : tokens) { //look at each token
				final int tokenEnd = min(i + token.length(), end); //find where the end of the token would be
				if(equalsIgnoreCase(token, charSequence, i, tokenEnd) && (tokenEnd == end || delimiters.contains(charSequence.charAt(tokenEnd)))) { //if this token equals the characters starting at the current position and ends with a delimiter or the end of the sequence
					return removeTokensIgnoreCase(new StringBuilder(charSequence), i, end, delimiters, tokens); //put the characters into a string builder and remove tokens from there
				}
			}
		}
		return charSequence; //the original character sequence did not need to be modified
	}

	/**
	 * Searches the given character sequence for the given tokens, separated by the given delimiters, without regard to ASCII case, and removes all occurrences
	 * that are found.
	 * @param stringBuilder The character sequence to search.
	 * @param delimiters The delimiters to skip.
	 * @param tokens The tokens for which to check.
	 * @return The string builder with the tokens removed.
	 * @throws NullPointerException if the given string builder, delimiters, and/or tokens is <code>null</code>.
	 */
	public static StringBuilder removeTokensIgnoreCase(final StringBuilder stringBuilder, final Characters delimiters, final CharSequence... tokens) {
		return removeTokensIgnoreCase(stringBuilder, 0, stringBuilder.length(), delimiters, tokens);
	}

	/**
	 * Searches the given character sequence for the given tokens, separated by the given delimiters, without regard to ASCII case, and removes all occurrences
	 * that are found.
	 * @param stringBuilder The character sequence to search.
	 * @param start The starting location for searching, inclusive.
	 * @param end The ending location for searching, exclusive.
	 * @param delimiters The delimiters to skip.
	 * @param tokens The tokens for which to check.
	 * @return The string builder with the tokens removed.
	 * @throws NullPointerException if the given string builder, delimiters, and/or tokens is <code>null</code>.
	 */
	public static StringBuilder removeTokensIgnoreCase(final StringBuilder stringBuilder, final int start, final int end, final Characters delimiters,
			final CharSequence... tokens) {
		for(int i = start; i < end; ++i) { //look through the sequence
			if(delimiters.contains(stringBuilder.charAt(i))) { //skip delimiters
				continue;
			}
			for(final CharSequence token : tokens) { //look at each token
				final int tokenEnd = min(i + token.length(), end); //find where the end of the token would be
				if(equalsIgnoreCase(token, stringBuilder, i, tokenEnd) && (tokenEnd == end || delimiters.contains(stringBuilder.charAt(tokenEnd)))) { //if this token equals the characters starting at the current position and ends with a delimiter or the end of the sequence
					stringBuilder.delete(i, tokenEnd); //remove the token; no need to re-adjust the index, as the next character is a delimiter or the end
				}
			}
		}
		return stringBuilder;
	}

	/**
	 * Compares the characters in one character sequence with characters in another character sequence without regard to ASCII case.
	 * @apiNote This method is typically used for formal language token manipulation, not general human language text processing.
	 * @param charSequence1 The character sequence to compare.
	 * @param charSequence2 The character sequence to compare with.
	 * @return <code>true</code> if the characters in the first character sequence equal the characters in the second character sequence.
	 */
	public static boolean equalsIgnoreCase(final CharSequence charSequence1, final CharSequence charSequence2) {
		if(charSequence1 == charSequence2) { //identity always implies equality
			return true;
		}
		return equalsIgnoreCase(charSequence1, charSequence2, 0);
	}

	/**
	 * Compares the characters in one character sequence with characters in another character sequence, starting at the given location to the end of the second
	 * character sequence, without regard to ASCII case
	 * @apiNote This method is typically used for formal language token manipulation, not general human language text processing.
	 * @param charSequence1 The character sequence to compare.
	 * @param charSequence2 The character sequence to compare with.
	 * @param start The starting location in the second character sequence, inclusive.
	 * @return <code>true</code> if the characters in the first character sequence equal the indicated characters in the second character sequence.
	 * @throws StringIndexOutOfBoundsException if <code>start</code> is negative or greater than the length of the second character sequence.
	 */
	public static boolean equalsIgnoreCase(final CharSequence charSequence1, final CharSequence charSequence2, final int start) {
		return equalsIgnoreCase(charSequence1, charSequence2, start, charSequence2.length());
	}

	/**
	 * Compares the characters in one character sequence with characters in another character sequence, without regard to ASCII case. If the given end of the
	 * second character sequence (the character sequence to which the first is being compared) is past the end, it is adjusted to be equal to the end of the
	 * second character sequence.
	 * @apiNote This method is typically used for formal language token manipulation, not general human language text processing.
	 * @param charSequence1 The character sequence to compare.
	 * @param charSequence2 The character sequence to compare with.
	 * @param start The starting location in the second character sequence, inclusive.
	 * @param end The ending location in the second character sequence, exclusive.
	 * @return <code>true</code> if the characters in the first character sequence equal the indicated characters in the second character sequence.
	 * @throws StringIndexOutOfBoundsException if <code>start</code> or <code>end</code> is negative or greater than <code>length()</code>, or <code>start</code>
	 *           is greater than <code>end</code>, with the exception that if <code>end</code> is greater than the length of the second character sequence it will
	 *           be adjusted to equal the end.
	 */
	public static boolean equalsIgnoreCase(final CharSequence charSequence1, final CharSequence charSequence2, final int start, final int end) {
		return equalsIgnoreCase(charSequence1, 0, charSequence1.length(), charSequence2, start, end);
	}

	/**
	 * Compares characters in one character sequence with characters in another character sequence, without regard to ASCII case. If the given end of the second
	 * character sequence (the character sequence to which the first is being compared) is past the end, it is adjusted to be equal to the end of the second
	 * character sequence.
	 * @apiNote This method is typically used for formal language token manipulation, not general human language text processing.
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
	public static boolean equalsIgnoreCase(final CharSequence charSequence1, final int start1, final int end1, final CharSequence charSequence2, final int start2,
			int end2) {
		checkBounds(charSequence1, start1, end1);
		final int length2 = charSequence2.length();
		if(end2 > length2) { //adjust the second character sequence's end if needed
			end2 = length2;
		}
		checkBounds(charSequence2, start2, end2);
		if((end2 - start2) != (end1 - start1)) { //if the counts differ
			return false;
		}
		for(int i1 = start1, i2 = start2; i1 < end1; ++i1, ++i2) { //look at each character; we only need to check one end position because we already made sure the counts are the same
			final char c1 = charSequence1.charAt(i1);
			final char c2 = charSequence1.charAt(i2);
			if(c1 == c2) { //if the characters match
				continue;
			} else if(c1 >= UPPERCASE_LETTER_FIRST && c1 <= UPPERCASE_LETTER_LAST) { //if the first character is uppercase
				if(c1 + LOWERCASE_UPPERCASE_LETTER_DIFFERENCE == c2) { //if the characters match after converting the first to lowercase
					continue;
				}
			} else if(c1 >= LOWERCASE_LETTER_FIRST && c1 <= LOWERCASE_LETTER_LAST) { //if the first character is lowercase
				if(c1 - LOWERCASE_UPPERCASE_LETTER_DIFFERENCE == c2) { //if the characters match after converting the first to uppercase
					continue;
				}
			}
			return false; //these characters don't match
		}
		return true; //everything matches		
	}

	/**
	 * Converts a character to lowercase if it is an ASCII uppercase letter.
	 * @apiNote This method is typically used for formal language token manipulation, not general human language text processing.
	 * @param c The character to convert.
	 * @return The character, guaranteed <em>not</em> to be an ASCII uppercase letter.
	 */
	public static char toLowerCase(final char c) {
		return c >= UPPERCASE_LETTER_FIRST && c <= UPPERCASE_LETTER_LAST ? (char)(c + LOWERCASE_UPPERCASE_LETTER_DIFFERENCE) : c;
	}

	/**
	 * Converts a character sequence to lowercase, converting only ASCII letters. The input character sequence will not be modified.
	 * @apiNote This method is typically used for formal language token manipulation, not general human language text processing.
	 * @param charSequence The character sequence containing the text to convert.
	 * @return A character sequence containing the converted text.
	 */
	public static CharSequence toLowerCase(final CharSequence charSequence) {
		final int end = charSequence.length();
		for(int i = 0; i < end; ++i) { //look at each character
			final char c = charSequence.charAt(i);
			if(c >= UPPERCASE_LETTER_FIRST && c <= UPPERCASE_LETTER_LAST) { //if this character is uppercase
				return toLowerCase(new StringBuilder(charSequence), i, end); //create a new string builder and do the conversion in-place
			}
		}
		return charSequence; //the original character sequence did not need to be modified
	}

	/**
	 * Converts a string builder to lowercase, converting only ASCII letters.
	 * @apiNote This method is typically used for formal language token manipulation, not general human language text processing.
	 * @param stringBuilder The string builder containing the text to convert.
	 * @return The string builder after the conversions.
	 */
	public static StringBuilder toLowerCase(final StringBuilder stringBuilder) {
		return toLowerCase(stringBuilder, 0, stringBuilder.length());
	}

	/**
	 * Converts a string builder to lowercase, converting only ASCII letters.
	 * @apiNote This method is typically used for formal language token manipulation, not general human language text processing.
	 * @param stringBuilder The string builder containing the text to convert.
	 * @param start The starting position, inclusive.
	 * @param end The ending position, exclusive.
	 * @return The string builder after the conversions.
	 */
	public static StringBuilder toLowerCase(final StringBuilder stringBuilder, final int start, final int end) {
		for(int i = start; i < end; ++i) { //look at each character
			final char c = stringBuilder.charAt(i);
			if(c >= UPPERCASE_LETTER_FIRST && c <= UPPERCASE_LETTER_LAST) { //if this character is uppercase
				stringBuilder.setCharAt(i, (char)(c + LOWERCASE_UPPERCASE_LETTER_DIFFERENCE)); //convert the character to lowercase
			}
		}
		return stringBuilder;
	}

	/**
	 * Converts a character to uppercase if it is an ASCII lowercase letter.
	 * @apiNote This method is typically used for formal language token manipulation, not general human language text processing.
	 * @param c The character to convert.
	 * @return The character, guaranteed <em>not</em> to be an ASCII lowercase letter.
	 */
	public static char toUpperCase(final char c) {
		return c >= LOWERCASE_LETTER_FIRST && c <= LOWERCASE_LETTER_LAST ? (char)(c - LOWERCASE_UPPERCASE_LETTER_DIFFERENCE) : c;
	}

	/**
	 * Converts a character sequence to uppercase, converting only ASCII letters. The input character sequence will not be modified.
	 * @apiNote This method is typically used for formal language token manipulation, not general human language text processing.
	 * @param charSequence The character sequence containing the text to convert.
	 * @return A character sequence containing the converted text.
	 */
	public static CharSequence toUpperCase(final CharSequence charSequence) {
		final int end = charSequence.length();
		for(int i = 0; i < end; ++i) { //look at each character
			final char c = charSequence.charAt(i);
			if(c >= LOWERCASE_LETTER_FIRST && c <= LOWERCASE_LETTER_LAST) { //if this character is lowercase
				return toUpperCase(new StringBuilder(charSequence), i, end); //create a new string builder and do the conversion in-place
			}
		}
		return charSequence; //the original character sequence did not need to be modified
	}

	/**
	 * Converts a string builder to uppercase, converting only ASCII letters.
	 * @apiNote This method is typically used for formal language token manipulation, not general human language text processing.
	 * @param stringBuilder The string builder containing the text to convert.
	 * @return The string builder after the conversions.
	 */
	public static StringBuilder toUpperCase(final StringBuilder stringBuilder) {
		return toUpperCase(stringBuilder, 0, stringBuilder.length());
	}

	/**
	 * Converts a string builder to uppercase, converting only ASCII letters.
	 * @apiNote This method is typically used for formal language token manipulation, not general human language text processing.
	 * @param stringBuilder The string builder containing the text to convert.
	 * @param start The starting position, inclusive.
	 * @param end The ending position, exclusive.
	 * @return The string builder after the conversions.
	 */
	public static StringBuilder toUpperCase(final StringBuilder stringBuilder, final int start, final int end) {
		for(int i = start; i < end; ++i) { //look at each character
			final char c = stringBuilder.charAt(i);
			if(c >= LOWERCASE_LETTER_FIRST && c <= LOWERCASE_LETTER_LAST) { //if this character is lowercase
				stringBuilder.setCharAt(i, (char)(c - LOWERCASE_UPPERCASE_LETTER_DIFFERENCE)); //convert the character to uppercase
			}
		}
		return stringBuilder;
	}

}
