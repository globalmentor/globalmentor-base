/*
 * Copyright © 2003-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import com.garretwilson.text.CharacterEncoding;
import com.garretwilson.text.RomanNumeralUtilities;

/**Various utilities and constants for interacting with characters.
In most cases, names of constants are derived from Unicode names.
@author Garret Wilson
*/
public class Characters
{
	/**The character with Unicode code point zero.*/
	public final static char NULL_CHAR=0x0000;
	/**A backspace.*/
	public final static char BACKSPACE_CHAR=0x0008;
	/**A horizontal tab.*/
	public final static char HORIZONTAL_TABULATION_CHAR=0x0009;
	/**A line feed (LF).*/
	public final static char LINE_FEED_CHAR=0x000A;
	/**A vertical tab.*/
	public final static char VERTICAL_TABULATION_CHAR=0x000B;
	/**A form feed (FF).*/
	public final static char FORM_FEED_CHAR=0x000C;
	/**A carriage return.*/
	public final static char CARRIAGE_RETURN_CHAR=0x000D;
	/**The information separator four character.*/
	public final static char INFORMATION_SEPARATOR_FOUR_CHAR=0x001C;
	/**The information separator three character.*/
	public final static char INFORMATION_SEPARATOR_THREE_CHAR=0x001D;
	/**The information separator two character.*/
	public final static char INFORMATION_SEPARATOR_TWO_CHAR=0x001E;
	/**The information separator one character.*/
	public final static char INFORMATION_SEPARATOR_ONE_CHAR=0x001F;
	/**A unit separator character.*/
	public final static char UNIT_SEPARATOR_CHAR=0x001F;
	/**A space character.*/
	public final static char SPACE_CHAR=0x0020;
	/**A quotation mark character.*/
	public final static char QUOTATION_MARK_CHAR=0x0022;
	/**The percent sign.*/
	public final static char PERCENT_SIGN_CHAR=0x0025;
	/**An apostrophe character.*/
	public final static char APOSTROPHE_CHAR=0x0027;
	/**A plus sign character.*/
	public final static char PLUS_SIGN_CHAR=0x002B;
	/**A comma character.*/
	public final static char COMMA_CHAR=0x002C;
	/**A hyphen or minus character.*/
	public final static char HYPHEN_MINUS_CHAR=0x002D;
	/**A colon character.*/
	public final static char COLON_CHAR=0x003A;
	/**A semicolon character.*/
	public final static char SEMICOLON_CHAR=0x003B;
	/**A less-than sign character (003C;LESS-THAN SIGN;Sm;0;ON;;;;;Y;;;;;).*/
	public final static char LESS_THAN_CHAR=0x003C;
	/**An equals sign character (003D;EQUALS SIGN;Sm;0;ON;;;;;N;;;;;).*/
	public final static char EQUALS_SIGN_CHAR=0x003D;
	/**A greater-than sign character (003E;GREATER-THAN SIGN;Sm;0;ON;;;;;Y;;;;;).*/
	public final static char GREATER_THAN_CHAR=0x003E;
	/**A question mark character (003F;QUESTION MARK;Po;0;ON;;;;;N;;;;;).*/
	public final static char QUESTION_MARK_CHAR=0x003F;
	/**A grave accent character.*/
	public final static char GRAVE_ACCENT_CHAR=0x0060;
	/**A tilde character (007E;TILDE;Sm;0;ON;;;;;N;;;;;).*/
	public final static char TILDE_CHAR=0x007E;	
	/**A next line (NEL) control character.*/
	public final static char NEXT_LINE_CHAR=0x0085;
	/**A start of string control character.*/
	public final static char START_OF_STRING_CHAR=0x0098;
	/**A string terminator control character.*/
	public final static char STRING_TERMINATOR_CHAR=0x009C;
	/**Unicode no-break space (NBSP).*/
	public final static char NO_BREAK_SPACE_CHAR=0x00A0;
	/**The copyright symbol.*/
	public final static char COPYRIGHT_SIGN=0x00A9;
	/**A left-pointing guillemet character.*/
	public final static char LEFT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR=0x00AB;
	/**The pilcrow or paragraph sign.
	@see #PARAGRAPH_SIGN_CHAR
	*/
	public final static char PILCROW_SIGN_CHAR=0x00B6;
	/**The paragraph sign.
	@see #PILCROW_SIGN_CHAR
	*/
	public final static char PARAGRAPH_SIGN_CHAR=PILCROW_SIGN_CHAR;
	/**A middle dot character.*/
	public final static char MIDDLE_DOT_CHAR=0x00B7;
	/**A right-pointing guillemet character.*/
	public final static char RIGHT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR=0x00BB;
	/**An uppercase oe ligature.*/
	public final static char LATIN_CAPITAL_LIGATURE_OE_CHAR=0x0152;
	/**A lowercase oe ligature.*/
	public final static char LATIN_SMALL_LIGATURE_OE_CHAR=0x0153;
	/**A Y umlaut.*/
	public final static char LATIN_CAPITAL_LETTER_Y_WITH_DIAERESIS_CHAR=0x0178;
	/**A zero-width space (ZWSP) that may expand during justification.*/
	public final static char ZERO_WIDTH_SPACE_CHAR=0x200B;
	/**A zero-width non-joiner (200C;ZERO WIDTH NON-JOINER;Cf;0;BN;;;;;N;;;;;).*/
	public final static char ZERO_WIDTH_NON_JOINER_CHAR=0x200C;
	/**A zero-width joiner (200D;ZERO WIDTH JOINER;Cf;0;BN;;;;;N;;;;;).*/
	public final static char ZERO_WIDTH_JOINER_CHAR=0x200D;
	/**A left-to-right mark (200E;LEFT-TO-RIGHT MARK;Cf;0;L;;;;;N;;;;;).*/
	public final static char LEFT_TO_RIGHT_MARK_CHAR=0x200E;
	/**A right-to-right mark (200F;RIGHT-TO-LEFT MARK;Cf;0;R;;;;;N;;;;;).*/
	public final static char RIGHT_TO_LEFT_MARK_CHAR=0x200F;
	/**A zero-width non-breaking space&mdash;word joiner (WJ).*/
	public final static char WORD_JOINER_CHAR=0x2060;
	/**A left single quote.*/
	public final static char LEFT_SINGLE_QUOTATION_MARK_CHAR=0x2018;
	/**A right single quote.*/
	public final static char RIGHT_SINGLE_QUOTATION_MARK_CHAR=0x2019;
	/**A single low-9 quotation mark.*/
	public final static char SINGLE_LOW_9_QUOTATION_MARK_CHAR=0x201A;
/*TODO fix and add to FORMAT_CHARS 
	202A;LEFT-TO-RIGHT EMBEDDING;Cf;0;LRE;;;;;N;;;;;
	202B;RIGHT-TO-LEFT EMBEDDING;Cf;0;RLE;;;;;N;;;;;
	202C;POP DIRECTIONAL FORMATTING;Cf;0;PDF;;;;;N;;;;;
	202D;LEFT-TO-RIGHT OVERRIDE;Cf;0;LRO;;;;;N;;;;;
	202E;RIGHT-TO-LEFT OVERRIDE;Cf;0;RLO;;;;;N;;;;;
206A;INHIBIT SYMMETRIC SWAPPING;Cf;0;BN;;;;;N;;;;;
206B;ACTIVATE SYMMETRIC SWAPPING;Cf;0;BN;;;;;N;;;;;
206C;INHIBIT ARABIC FORM SHAPING;Cf;0;BN;;;;;N;;;;;
206D;ACTIVATE ARABIC FORM SHAPING;Cf;0;BN;;;;;N;;;;;
206E;NATIONAL DIGIT SHAPES;Cf;0;BN;;;;;N;;;;;
206F;NOMINAL DIGIT SHAPES;Cf;0;BN;;;;;N;;;;;
FFF9;INTERLINEAR ANNOTATION ANCHOR;Cf;0;BN;;;;;N;;;;;
FFFA;INTERLINEAR ANNOTATION SEPARATOR;Cf;0;BN;;;;;N;;;;;
FFFB;INTERLINEAR ANNOTATION TERMINATOR;Cf;0;BN;;;;;N;;;;;
*/
	/**A single high-reversed-9 quotation mark.*/
	public final static char SINGLE_HIGH_REVERSED_9_QUOTATION_MARK_CHAR=0x201B;
	/**A left double quote.*/
	public final static char LEFT_DOUBLE_QUOTATION_MARK_CHAR=0x201C;
	/**A right double quote.*/
	public final static char RIGHT_DOUBLE_QUOTATION_MARK_CHAR=0x201D;
	/**A double low-9 quotation mark.*/
	public final static char DOUBLE_LOW_9_QUOTATION_MARK_CHAR=0x201E;
	/**A double high-reversed-9 quotation mark.*/
	public final static char DOUBLE_HIGH_REVERSED_9_QUOTATION_MARK_CHAR=0x201F;
	/**Unicode en dash character.*/
	public final static char EN_DASH_CHAR=0x2013;
	/**Unicode em dash character.*/
	public final static char EM_DASH_CHAR=0x2014;
	/**Unicode bullet character.*/
	public final static char BULLET_CHAR=0x2022;
	/**Unicode horizontal ellipsis.*/
	public final static char HORIZONTAL_ELLIPSIS_CHAR=0x2026;
	/**A paragraph separator character.*/
	public final static char PARAGRAPH_SEPARATOR_CHAR=0x2029;
	/**A left-pointing single guillemet character.*/
	public final static char SINGLE_LEFT_POINTING_ANGLE_QUOTATION_MARK_CHAR=0x2039;
	/**A right-pointing single guillemet character.*/
	public final static char SINGLE_RIGHT_POINTING_ANGLE_QUOTATION_MARK_CHAR=0x203A;
	/**Trademark character.*/
	public final static char TRADE_MARK_SIGN_CHAR=0x2122;
	/**Infinity symbol (221E;INFINITY;Sm;0;ON;;;;;N;;;;;).*/
	public final static char INFINITY_CHAR=0x2122;
	/**A left-pointing angle bracket character (2329;LEFT-POINTING ANGLE BRACKET;Ps;0;ON;3008;;;;Y;BRA;;;;).*/
	public final static char LEFT_POINTING_ANGLE_BRACKET=0x2329;
	/**A right-pointing angle bracket character (232A;RIGHT-POINTING ANGLE BRACKET;Pe;0;ON;3009;;;;Y;KET;;;;).*/
	public final static char RIGHT_POINTING_ANGLE_BRACKET=0x232A;
	/**A reversed double prime quotation mark.*/
	public final static char REVERSED_DOUBLE_PRIME_QUOTATION_MARK_CHAR=0x301D;
	/**A double prime quotation mark.*/
	public final static char DOUBLE_PRIME_QUOTATION_MARK_CHAR=0x301E;
	/**A low double prime quotation mark.*/
	public final static char LOW_DOUBLE_PRIME_QUOTATION_MARK_CHAR=0x301F;
	/**A full width quotation mark.*/
	public final static char FULLWIDTH_QUOTATION_MARK_CHAR=0xFF02;
	/**A zero-width no-breaking space (ZWNBSP)&mdash;the Byte Order Mark (BOM) (FEFF;ZERO WIDTH NO-BREAK SPACE;Cf;0;BN;;;;;N;BYTE ORDER MARK;;;;).
	For non-breaking purposes, deprecated in favor of <code>WORD_JOINER_CHAR</code>.
	@see #WORD_JOINER_CHAR
	*/
	public final static char ZERO_WIDTH_NO_BREAK_SPACE_CHAR=0xFEFF;
	/**The Byte Order Mark (BOM).
	@see #ZERO_WIDTH_NO_BREAK_SPACE_CHAR
	*/
	public final static char BOM_CHAR=ZERO_WIDTH_NO_BREAK_SPACE_CHAR;
	/**A character for a placeholder in text for an otherwise unspecified object.*/
	public final static char OBJECT_REPLACEMENT_CHAR=0xFFFC;
	/**Represents a character that is unknown or unrepresentable in Unicode.*/
	public final static char REPLACEMENT_CHAR=0xFFFD;
	/**An invalid, undefined Unicode character.*/
	public final static char UNDEFINED_CHAR=0xFFFF;

	/**Unicode control characters (0x0000-0x001F, 0x007F-0x09F).*/
	public final static String CONTROL_CHARS=""+
		(char)0x0000+(char)0x0001+(char)0x0002+(char)0x0003+(char)0x0004+(char)0x0005+(char)0x0006+(char)0x0007+
		(char)0x0008+(char)0x0009+(char)0x000A+(char)0x000B+(char)0x000C+(char)0x000D+(char)0x000E+(char)0x000F+
		(char)0x0010+(char)0x0011+(char)0x0012+(char)0x0013+(char)0x0014+(char)0x0015+(char)0x0016+(char)0x0017+
		(char)0x0018+(char)0x0019+(char)0x001A+(char)0x001B+(char)0x001C+(char)0x001D+(char)0x001E+(char)0x001F+
		(char)0x007F+
		(char)0x0080+(char)0x0081+(char)0x0082+(char)0x0083+(char)0x0084+(char)0x0085+(char)0x0086+(char)0x0087+
		(char)0x0088+(char)0x0089+(char)0x008A+(char)0x008B+(char)0x008C+(char)0x008D+(char)0x008E+(char)0x008F+
		(char)0x0090+(char)0x0091+(char)0x0092+(char)0x0093+(char)0x0094+(char)0x0095+(char)0x0096+(char)0x0097+
		(char)0x0098+(char)0x0099+(char)0x009A+(char)0x009B+(char)0x009C+(char)0x009D+(char)0x009E+(char)0x009F;

	/**Unicode paragraph separator characters.*/
	public final static String PARAGRAPH_SEPARATOR_CHARS=""+LINE_FEED_CHAR+CARRIAGE_RETURN_CHAR+INFORMATION_SEPARATOR_FOUR_CHAR+INFORMATION_SEPARATOR_THREE_CHAR+INFORMATION_SEPARATOR_TWO_CHAR+NEXT_LINE_CHAR+PARAGRAPH_SEPARATOR_CHAR;

	/**Unicode segment separator characters.*/
	public final static String SEGMENT_SEPARATOR_CHARS=""+HORIZONTAL_TABULATION_CHAR+VERTICAL_TABULATION_CHAR+INFORMATION_SEPARATOR_ONE_CHAR;

	/**Unicode whitespace characters.*/
	public final static String WHITESPACE_CHARS=""+HORIZONTAL_TABULATION_CHAR+LINE_FEED_CHAR+VERTICAL_TABULATION_CHAR+FORM_FEED_CHAR+CARRIAGE_RETURN_CHAR+SPACE_CHAR;
/*TODO add
			  * U0085 NEL
			  * U00A0 NBSP
			  * U1680 OGHAM SPACE MARK
			  * U180E MONGOLIAN VOWEL SEPARATOR
			  * U2000-U200A (different sorts of spaces)
			  * U2028 LSP
			  * U2029 PSP
			  * U202F NARROW NBSP
			  * U205F MEDIUM MATHEMATICAL SPACE
			  * U3000 IDEOGRAPHIC SPACE
*/
			
			
	/**Unicode formatting characters; Unicode characters marked with "Cf",
		such as <code>WORD_JOINER</code>.
	*/
	public final static String FORMAT_CHARS=""+ZERO_WIDTH_NON_JOINER_CHAR+ZERO_WIDTH_JOINER_CHAR
			+LEFT_TO_RIGHT_MARK_CHAR+RIGHT_TO_LEFT_MARK_CHAR
			+WORD_JOINER_CHAR+ZERO_WIDTH_NO_BREAK_SPACE_CHAR;

	/**Characters considered to be end-of-line markers (e.g. CR and LF).*/
	public final static String EOL_CHARS=""+CARRIAGE_RETURN_CHAR+LINE_FEED_CHAR;

	/**Characters that do not contain visible "content", and may be trimmed from ends of a string.
	These include whitespace, control characters, and formatting characters.
	*/
	public final static String TRIM_CHARS=WHITESPACE_CHARS+CONTROL_CHARS+FORMAT_CHARS;

	/**A regular expression pattern for the class of trim characters.
	@see #TRIM_CHARS
	*/
//TODO del if not needed	public final static Pattern TRIM_PATTERN=Pattern.compile("["+TRIM_CHARS+"]");

	/**Characters that delimit a list separated by trim characters, commas, and/or semicolons.
	@see #TRIM_CHARS
	*/
	public final static String LIST_DELIMITER_CHARS=TRIM_CHARS+",;";

	/**A regular expression character class pattern for the class of list delimiter characters.
	@see #LIST_DELIMITER_CHARS
	*/
//TODO del if not needed	public final static Pattern LIST_DELIMITER_CLASS_PATTERN=Pattern.compile(createCharacterClass(LIST_DELIMITER_CHARS));

	
	/**Characters that could be considered the start of a quotation.*/
	public final static String LEFT_QUOTE_CHARS=""+
			QUOTATION_MARK_CHAR+
			APOSTROPHE_CHAR+
			REVERSED_DOUBLE_PRIME_QUOTATION_MARK_CHAR+
			DOUBLE_PRIME_QUOTATION_MARK_CHAR+
			LOW_DOUBLE_PRIME_QUOTATION_MARK_CHAR+
			FULLWIDTH_QUOTATION_MARK_CHAR+
			LEFT_SINGLE_QUOTATION_MARK_CHAR+
			LEFT_DOUBLE_QUOTATION_MARK_CHAR+
			LEFT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR+
			SINGLE_LOW_9_QUOTATION_MARK_CHAR+
			DOUBLE_LOW_9_QUOTATION_MARK_CHAR+
			SINGLE_LEFT_POINTING_ANGLE_QUOTATION_MARK_CHAR;

	/**Characters that could be considered the end of a quotation.*/
	public final static String RIGHT_QUOTE_CHARS=""+
			QUOTATION_MARK_CHAR+
			APOSTROPHE_CHAR+
			REVERSED_DOUBLE_PRIME_QUOTATION_MARK_CHAR+
			DOUBLE_PRIME_QUOTATION_MARK_CHAR+
			LOW_DOUBLE_PRIME_QUOTATION_MARK_CHAR+
			FULLWIDTH_QUOTATION_MARK_CHAR+
			RIGHT_SINGLE_QUOTATION_MARK_CHAR+
			RIGHT_DOUBLE_QUOTATION_MARK_CHAR+
			RIGHT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR+
			SINGLE_HIGH_REVERSED_9_QUOTATION_MARK_CHAR+
			DOUBLE_HIGH_REVERSED_9_QUOTATION_MARK_CHAR+
			SINGLE_RIGHT_POINTING_ANGLE_QUOTATION_MARK_CHAR;

	/**Characters that start or end quotations.*/
	public final static String QUOTE_CHARS=""+
			QUOTATION_MARK_CHAR+
			APOSTROPHE_CHAR+
			REVERSED_DOUBLE_PRIME_QUOTATION_MARK_CHAR+
			DOUBLE_PRIME_QUOTATION_MARK_CHAR+
			LOW_DOUBLE_PRIME_QUOTATION_MARK_CHAR+
			FULLWIDTH_QUOTATION_MARK_CHAR+
			LEFT_SINGLE_QUOTATION_MARK_CHAR+
			RIGHT_SINGLE_QUOTATION_MARK_CHAR+
			LEFT_DOUBLE_QUOTATION_MARK_CHAR+
			RIGHT_DOUBLE_QUOTATION_MARK_CHAR+
			LEFT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR+
			RIGHT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR+
			SINGLE_LOW_9_QUOTATION_MARK_CHAR+
			SINGLE_HIGH_REVERSED_9_QUOTATION_MARK_CHAR+
			DOUBLE_LOW_9_QUOTATION_MARK_CHAR+
			DOUBLE_HIGH_REVERSED_9_QUOTATION_MARK_CHAR+
			SINGLE_LEFT_POINTING_ANGLE_QUOTATION_MARK_CHAR+
			SINGLE_RIGHT_POINTING_ANGLE_QUOTATION_MARK_CHAR;

	/**Characters used to punctuate phrases and sentences.*/
	public final static String PHRASE_PUNCTUATION_CHARS=".,:;?!";	//TODO use constants here

	/**Punctuation that expects a character to follow at some point.*/
	public final static String DEPENDENT_PUNCTUATION_CHARS=""+
		  COLON_CHAR+';'+COMMA_CHAR+HYPHEN_MINUS_CHAR+EM_DASH_CHAR+EN_DASH_CHAR;	//TODO use a constant

	/**Left punctuation used to group characters.*/
	public final static String LEFT_GROUP_PUNCTUATION_CHARS="([{<"; //TODO use constants

	/**Right punctuation used to group characters.*/
	public final static String RIGHT_GROUP_PUNCTUATION_CHARS=")]}>"; //TODO use constants

	/**Punctuation used to group characters.*/
	public final static String GROUP_PUNCTUATION_CHARS=LEFT_GROUP_PUNCTUATION_CHARS+RIGHT_GROUP_PUNCTUATION_CHARS;

	/**Characters used to punctuate phrases and sentences, as well as general
		punctuation such as quotes.*/
	public final static String PUNCTUATION_CHARS=
		  PHRASE_PUNCTUATION_CHARS+
			GROUP_PUNCTUATION_CHARS+
			QUOTE_CHARS+
			HYPHEN_MINUS_CHAR+EM_DASH_CHAR+EN_DASH_CHAR;

	/**Characters that separate words.*/
	public final static String WORD_DELIMITER_CHARS=WHITESPACE_CHARS+PUNCTUATION_CHARS;	//TODO this needs fixed

	/**A regular expression pattern for the class of word delimiter characters.
	@see #WORD_DELIMITER_CHARS
	*/
//TODO fix; these characters must be escaped, or this Pattern.toString() will run into an endless loop!	public final static Pattern WORD_DELIMITER_PATTERN=Pattern.compile("["+WORD_DELIMITER_CHARS+"]");

	/**Characters that allow words to wrap.*/
	public final static String WORD_WRAP_CHARS=WHITESPACE_CHARS+"-/";	//TODO use constants

	/**Sees if the specified character is in one of the specified ranges.
	@param c The character to check.
	@param ranges An array of character pair arrays, <em>in order</em>, the first of each pair specifying the bottom inclusive character of a range, the second of which specifying the top inclusive character of the range.
	@return <code>true</code> if the character is in one of the ranges, else <code>false</code>.
	*/
	public static boolean isCharInRange(final char c, final char[][] ranges)	//TODO improve code by doing a binary search
	{
		final int rangeCount=ranges.length;	//find out how many ranges there are
		for(int i=0; i<rangeCount; ++i)	//look at each range
		{
			final char[] range=ranges[i];	//get this range
			if(c<range[0])	//if the character is lower than the lower bound, it's not in this range---and we've already checked previous ranges
			{
				return false;	//we've ran out of ranges that might work
			}
			else if(c<=range[1])	//if the character is greater than or equal to the lower bound, see if the character is lower than or equal to the upper bound
			{
				return true;	//the character is within range
			}
		}
		return false;	//if we get here, this means our character is higher than any of the characters, meaning the character is higher than all the ranges
	}

	/**Determines whether a character is in the ASCII character range (0x00-0x80).
	@param c The character to examine.
	@return <code>true</code> if the character is an ASCII character.
	*/
	public static boolean isASCII(final char c)
	{
		return c>=0 && c<=0x80; //see if this character is between 0 and 128, inclusive
	}

	/**Determines whether a character is one of the digits '0'-'9'.
	@param c The character to examine.
	@return <code>true</code> if the character is an ISO_LATIN_1 digit.
	*/
	public final static boolean isLatinDigit(final char c)
	{
		return c>='0' && c<='9';  //see if the character falls in the range of the latin digits
	}

	/**Specifies whether or not a given character is a punctuation mark.
	@param c Character to analyze.
	@return <code>true</code> if the character is punctuation.
	*/
	public static boolean isPunctuation(final char c)
	{
		return PUNCTUATION_CHARS.indexOf(c)>=0;	//return true if we can find the character in the string of punctuation characters TODO update the list of punctuation characters
	}

	/**Determines whether a character is a Roman numeral.
	@param c The character to examine.
	@return <code>true</code> if the character is a Roman numeral.
	*/
	public static boolean isRomanNumeral(final char c)
	{
		return RomanNumeralUtilities.getValue(c)>=0;  //see if the character returns a valid Roman numeral value
	}

	/**Specifies whether or not a given character is whitespace.
	@param c Character to analyze.
	@return <code>true</code> if the character is whitespace.
	*/
	public static boolean isWhitespace(final char c)
	{
		return WHITESPACE_CHARS.indexOf(c)>=0;	//return true if we can find the character in the string of whitespace characters
	}

	/**Specifies whether or not a given character is a word delimiter, such as
		whitespace or punctuation.
	@param c Character to analyze.
	@return <code>true</code> if the character allows word wrapping.
	*/
	public static boolean isWordDelimiter(final char c)
	{
		return WORD_DELIMITER_CHARS.indexOf(c)>=0;	//return true if we can find the character in the string of word delimiter characters
	}

	/**Specifies whether or not a given character allows a word wrap.
	@param c Character to analyze.
	@return <code>true</code> if the character allows word wrapping.
	*/
	public static boolean isWordWrap(final char c)
	{
		return WORD_WRAP_CHARS.indexOf(c)>=0;	//return true if we can find the character in the string of word wrap characters
	}

	/**Converts an array of characters to an array of bytes, using the UTF-8
	character encoding.
	@param characters The characters to convert to bytes.
	@return An array of bytes representing the given characters in the UTF-8
		encoding.
	*/
	public static byte[] toByteArray(final char[] characters)
	{
		try
		{
			return toByteArray(characters, CharacterEncoding.UTF_8);	//convert the characters using UTF-8
		}
		catch(UnsupportedEncodingException unsupportedEncodingException)	//all JVMs should support UTF-8
		{
			throw new AssertionError(unsupportedEncodingException);
		}
	}

	/**Converts an array of characters to an array of bytes, using the given
	character encoding.
	@param characters The characters to convert to bytes.
	@param encoding The encoding to use when converting characters to bytes.
	@return An array of bytes representing the given characters in the specified
		encoding.
	@exception UnsupportedEncodingException if the given encoding is not supported.
	*/
	public static byte[] toByteArray(final char[] characters, final String encoding) throws UnsupportedEncodingException
	{
		final ByteArrayOutputStream byteArrayOutputStream=new ByteArrayOutputStream();	//create a byte array output stream
		final Writer writer=new OutputStreamWriter(byteArrayOutputStream, encoding);	//create a writer for converting characters to bytes
		try
		{
			writer.write(characters);	//write the characters to the writer in one batch (writing them individually would be extremely inefficient)
			writer.flush();	//flush everything we've written to the byte output stream
		}
		catch(IOException ioException)		//we don't expect any errors
		{
			throw new AssertionError(ioException);
		}
		return byteArrayOutputStream.toByteArray();	//return the bytes we collected from the character conversion
	}

	/**Parses a string and returns its character value.
	@param string A string expected to contain a single character.
	@return The single character contained by the string.
	@exception NullPointerException if the given string is <code>null</code>
	@exception IllegalArgumentException if the string is not composed of a single character.
	*/
	public final static Character parseCharacter(final String string)
	{
		if(string.length()!=1)	//if this string isn't composed of a single character
		{
			throw new IllegalArgumentException("The string \""+string+"\" does not represent a single character.");
		}
		return Character.valueOf(string.charAt(0));	//return the first and only character in the string
	}

}