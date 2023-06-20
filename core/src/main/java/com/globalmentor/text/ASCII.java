/*
 * Copyright © 2012-2017 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.text;

import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Conditions.*;
import static java.lang.Math.*;

import javax.annotation.*;

import com.globalmentor.java.Characters;

/**
 * Constants and utilities for working with US_ASCII.
 * @author Garret Wilson
 */
public class ASCII {

	/** The value of the first lowercase letter. */
	public static final char LOWERCASE_LETTER_FIRST = 'a';
	/** The value of the last lowercase letter. */
	public static final char LOWERCASE_LETTER_LAST = 'z';

	/** The value of the first uppercase letter. */
	public static final char UPPERCASE_LETTER_FIRST = 'A';
	/** The value of the last uppercase letter. */
	public static final char UPPERCASE_LETTER_LAST = 'Z';

	/**
	 * The difference in value between lowercase (higher values) and uppercase (lower values) ASCII letters. An uppercase ASCII letter can be converted to a
	 * lowercase letter by <em>adding</em> this value.
	 */
	public static final char LOWERCASE_UPPERCASE_LETTER_DIFFERENCE = LOWERCASE_LETTER_FIRST - UPPERCASE_LETTER_FIRST;

	/** The highest ASCII code point value that is a control character. */
	public static final char MAX_CONTROL_VALUE = 31;

	/** The highest ASCII code point value. */
	public static final char MAX_VALUE = 127;

	/** The value of the first digit. */
	public static final char DIGIT_FIRST = '0';
	/** The value of the last digit. */
	public static final char DIGIT_LAST = '9';

	/** The ASCII digit characters <code>'0'</code> – <code>'9'</code>. */
	public static final Characters DIGIT_CHARACTERS = Characters.ofRange(DIGIT_FIRST, DIGIT_LAST);

	/** The value of the first lowercase hexadecimal letter digit. */
	public static final char LOWERCASE_HEX_LETTER_DIGIT_FIRST = 'a';
	/** The value of the last lowercase hexadecimal letter digit. */
	public static final char LOWERCASE_HEX_LETTER_DIGIT_LAST = 'f';

	/** The value of the first uppercase hexadecimal letter digit. */
	public static final char UPPERCASE_HEX_LETTER_DIGIT_FIRST = 'A';
	/** The value of the last uppercase hexadecimal letter digit. */
	public static final char UPPERCASE_HEX_LETTER_DIGIT_LAST = 'F';

	/**
	 * The ASCII hexadecimal digit characters <code>'0'</code> – <code>'9'</code>, <code>'a'</code> – <code>'f'</code>, and <code>'A'</code> – <code>'F'</code>.
	 * @apiNote Note that this definition includes both lowercase and uppercase letters.
	 */
	public static final Characters HEX_CHARACTERS = DIGIT_CHARACTERS.addRange(LOWERCASE_HEX_LETTER_DIGIT_FIRST, LOWERCASE_HEX_LETTER_DIGIT_LAST)
			.addRange(UPPERCASE_HEX_LETTER_DIGIT_FIRST, UPPERCASE_HEX_LETTER_DIGIT_LAST);

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
	 * Compares two characters without regard to ASCII case.
	 * @apiNote This method is typically used for formal language token manipulation, not general human language text processing.
	 * @param c1 The character to compare.
	 * @param c2 The character to compare with.
	 * @return <code>true</code> if the characters are equal without regard to ASCII case.
	 */
	public static boolean equalsIgnoreCase(final char c1, final char c2) {
		if(c1 == c2) { //if the characters match
			return true;
		} else if(c1 >= UPPERCASE_LETTER_FIRST && c1 <= UPPERCASE_LETTER_LAST) { //if the first character is uppercase
			if(c1 + LOWERCASE_UPPERCASE_LETTER_DIFFERENCE == c2) { //if the characters match after converting the first to lowercase
				return true;
			}
		} else if(c1 >= LOWERCASE_LETTER_FIRST && c1 <= LOWERCASE_LETTER_LAST) { //if the first character is lowercase
			if(c1 - LOWERCASE_UPPERCASE_LETTER_DIFFERENCE == c2) { //if the characters match after converting the first to uppercase
				return true;
			}
		}
		return false;
	}

	/**
	 * Compares the characters in one character sequence with characters in another character sequence without regard to ASCII case.
	 * @apiNote This method is typically used for formal language token manipulation, not general human language text processing.
	 * @apiNote This API follows {@link Object#equals(Object)} and {@link String#equalsIgnoreCase(String)} in allowing either or both arguments to be
	 *          <code>null</code>.
	 * @param charSequence1 The character sequence to compare.
	 * @param charSequence2 The character sequence to compare with.
	 * @return <code>true</code> if the characters in the first character sequence equal the characters in the second character sequence.
	 */
	public static boolean equalsIgnoreCase(@Nullable final CharSequence charSequence1, @Nullable final CharSequence charSequence2) {
		if(charSequence1 == charSequence2) { //identity always implies equality
			return true;
		}
		if(charSequence1 == null || charSequence2 == null) { //if one is null but not the other
			return false;
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
	public static boolean equalsIgnoreCase(@Nonnull final CharSequence charSequence1, @Nonnull final CharSequence charSequence2, final int start) {
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
	public static boolean equalsIgnoreCase(@Nonnull final CharSequence charSequence1, @Nonnull final CharSequence charSequence2, final int start, final int end) {
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
	public static boolean equalsIgnoreCase(@Nonnull final CharSequence charSequence1, final int start1, final int end1, @Nonnull final CharSequence charSequence2,
			final int start2, int end2) {
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
			final char c2 = charSequence2.charAt(i2);
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
	 * Produces a hash code of the characters in the given sequence, without regard to ASCII case. The returned value meets the semantics of
	 * {@link Object#hashCode()} for string equality without regard to ASCII case.
	 * @apiNote This method is intended to be a semantically appropriate and more efficient way to produce a hash code than converting two strings to lowercase
	 *          for comparison. Moreover various {@link CharSequence} implementations may produce different hash codes, making it impossible to compare the hash
	 *          code of a {@link String} or a {@link StringBuilder}, for example (the latter of which may be produced by {@link #toLowerCase(CharSequence)}).
	 * @implSpec This implementation uses the same algorithm used by the OpenJDK 17 for strings known to contain only ISO-8859-1 characters, except that it
	 *           normalizes ASCII characters to lowercase.
	 * @implNote This method is intended for sequences of ASCII characters. If any non-ASCII characters are present, this method will not fail, but the resulting
	 *           hash code may not be as robust as retrieving a hash code on a {@link String} containing the same characters.
	 * @param charSequence The sequence of characters for which an ASCII case-insensitive hash should be produce.
	 * @return The ASCII case-insensitive hash of the characters in the sequence.
	 */
	public static int hashCodeIgnoreCase(@Nonnull final CharSequence charSequence) {
		int hash = 0;
		final int length = charSequence.length();
		for(int index = 0; index < length; index++) {
			final char c = charSequence.charAt(index);
			final char cIgnoreCase = c >= UPPERCASE_LETTER_FIRST && c <= UPPERCASE_LETTER_LAST ? (char)(c + LOWERCASE_UPPERCASE_LETTER_DIFFERENCE) : c;
			hash = 31 * hash + (cIgnoreCase & 0xff);
		}
		return hash;
	}

	/**
	 * Determines the first index of the given character, without regard to ASCII case.
	 * @apiNote This method is typically used for formal language token manipulation, not general human language text processing.
	 * @param charSequence The character sequence to check.
	 * @param character The character to search for.
	 * @return The index of the first occurrence of the given character without regard to ASCII case, or -1 if the character was not found.
	 */
	public static int indexOfIgnoreCase(@Nonnull final CharSequence charSequence, final char character) {
		return indexOfIgnoreCase(charSequence, character, 0); //search from the beginning
	}

	/**
	 * Determines the first index of the given character, without regard to ASCII case.
	 * @param charSequence The character sequence to check.
	 * @param character The character to search for.
	 * @param index The first index to examine.
	 * @return The index of the first occurrence of the given character without regard to ASCII case, or -1 if the character was not found.
	 */
	public static int indexOfIgnoreCase(final CharSequence charSequence, final char character, final int index) {
		final char otherCaseCharacter = isUpperCase(character) ? toLowerCase(character) : toUpperCase(character);
		if(character == otherCaseCharacter) { //if there is no difference in case to check (e.g. we are not searching for an ASCII letter)
			return indexOf(charSequence, character); //short-circuit and jut do a normal search
		}
		final int length = charSequence.length();
		for(int i = index; i < length; ++i) {
			final char current = charSequence.charAt(i);
			if(current == character || current == otherCaseCharacter) {
				return i;
			}
		}
		return -1;
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
	 * Converts a character sequence to a lowercase string, converting only ASCII letters.
	 * @apiNote This is a convenience method for returning the result in string form. If you do not need a string, you should use
	 *          {@link #toLowerCase(CharSequence)}.
	 * @implSpec This method delegates to {@link #toLowerCase(CharSequence)} followed by {@link CharSequence#toString()}.
	 * @param charSequence The character sequence containing the text to convert.
	 * @return A string containing the converted text.
	 */
	public static String toLowerCaseString(final CharSequence charSequence) {
		return toLowerCase(charSequence).toString();
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

	/**
	 * Converts a character sequence to an uppercase string, converting only ASCII letters.
	 * @apiNote This is a convenience method for returning the result in string form. If you do not need a string, you should use
	 *          {@link #toUpperCase(CharSequence)}.
	 * @implSpec This method delegates to {@link #toUpperCase(CharSequence)} followed by {@link CharSequence#toString()}.
	 * @param charSequence The character sequence containing the text to convert.
	 * @return A string containing the converted text.
	 */
	public static String toUpperCaseString(final CharSequence charSequence) {
		return toUpperCase(charSequence).toString();
	}

	/**
	 * Returns the value represented by the given ASCII digit. For example the value of character <code>'5'</code> is the integer <code>5</code>.
	 * @param c The ASCII digit character.
	 * @return The value the character represents.
	 * @throws IllegalArgumentException if the given character is not an ASCII digit character.
	 * @see #DIGIT_CHARACTERS
	 */
	public static int valueOfDigit(final char c) {
		checkArgument(DIGIT_CHARACTERS.contains(c), "Character `%s` is not an ASCII digit.", c);
		return c - DIGIT_FIRST;
	}

	/**
	 * Find the value of the hexadecimal digit, without regard to case. For example the value of character <code>'5'</code> is the integer <code>5</code>, and the
	 * value of character <code>'c'</code> is the integer <code>12</code>.
	 * @param c The hexadecimal digit character.
	 * @return The value the character represents.
	 * @throws IllegalArgumentException if the given character is not a hexadecimal digit character.
	 * @see #valueOfDigit(char)
	 */
	public static int valueOfHexDigit(final char c) {
		if(DIGIT_CHARACTERS.contains(c)) {
			return valueOfDigit(c);
		}
		final char normalizedCharacter = toLowerCase(c); //normalize input, which we expect to be a letter now
		//because HEX_CHARACTERS contains two ranges, it's more efficient to explicitly check the normalized character
		checkArgument(normalizedCharacter >= LOWERCASE_HEX_LETTER_DIGIT_FIRST && normalizedCharacter <= LOWERCASE_HEX_LETTER_DIGIT_LAST,
				"Character `%s` is not an ASCII digit.", c);
		return normalizedCharacter - LOWERCASE_HEX_LETTER_DIGIT_FIRST + 10;
	}

}
