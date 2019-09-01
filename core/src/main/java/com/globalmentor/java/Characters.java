/*
 * Copyright © 1996-2017 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static java.nio.charset.StandardCharsets.*;
import static java.util.Arrays.*;
import static java.util.Collections.*;

import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

import static com.globalmentor.java.Arrays.*;
import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Integers.*;
import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.java.StringBuilders.*;

import com.globalmentor.text.ASCII;
import com.globalmentor.text.RomanNumerals;

/**
 * An immutable set of characters that supports various searching and other functions. This essentially provides an efficient yet immutable array with
 * object-oriented functionality.
 * 
 * <p>
 * This class is similar to {@link String}, except that it discards duplicate characters. Furthermore, this class allows no Unicode surrogates; the characters
 * contained are interpreted as complete Unicode code points. This also makes comparison more efficient. As this class is similar to an ordered set than a list,
 * it doesn't implement {@link CharSequence} in order to prevent signature conflicts; and provides {@link #size()} to count its contents instead of a "length"
 * property.
 * </p>
 * <p>
 * This class also provides static utilities and constants for interacting with characters in general.
 * </p>
 * <p>
 * In most cases, names of constants are derived from Unicode names.
 * </p>
 * @author Garret Wilson
 * @see <a href="http://unicode.org/unicode/standard/reports/tr13/tr13-5.html">Unicode Newline Guidelines</a>
 */
public final class Characters {

	/** A shared instance of an empty array of characters. */
	public static final char[] EMPTY_ARRAY = new char[0];

	/** The shared instance of no characters. */
	public static final Characters NONE = new Characters(EMPTY_ARRAY); //must be defined before the predefined instances below

	/** The character with Unicode code point zero. */
	public static final char NULL_CHAR = 0x0000;
	/** A backspace. */
	public static final char BACKSPACE_CHAR = 0x0008;
	/** A horizontal tab (0009;&lt;control&gt;;Cc;0;S;;;;;N;CHARACTER TABULATION;;;;). */
	public static final char CHARACTER_TABULATION_CHAR = 0x0009;
	/** A line feed (LF). */
	public static final char LINE_FEED_CHAR = 0x000A;
	/** A vertical tab (000B;&lt;control&gt;;Cc;0;S;;;;;N;LINE TABULATION;;;;). */
	public static final char LINE_TABULATION_CHAR = 0x000B;
	/** A form feed (FF). */
	public static final char FORM_FEED_CHAR = 0x000C;
	/** A carriage return. */
	public static final char CARRIAGE_RETURN_CHAR = 0x000D;
	/** Data Link Escape control character (0010;&lt;control&gt;;Cc;0;BN;;;;;N;DATA LINK ESCAPE;;;;). */
	public static final char DATA_LINK_ESCAPE_CHAR = 0x0010;
	/** The information separator four character. */
	public static final char INFORMATION_SEPARATOR_FOUR_CHAR = 0x001C;
	/** The information separator three character. */
	public static final char INFORMATION_SEPARATOR_THREE_CHAR = 0x001D;
	/** The information separator two character. */
	public static final char INFORMATION_SEPARATOR_TWO_CHAR = 0x001E;
	/** The information separator one character. */
	public static final char INFORMATION_SEPARATOR_ONE_CHAR = 0x001F;
	/** A unit separator character. */
	public static final char UNIT_SEPARATOR_CHAR = 0x001F;
	/** A space character. */
	public static final char SPACE_CHAR = 0x0020;
	/** A quotation mark character. */
	public static final char QUOTATION_MARK_CHAR = 0x0022;
	/** The percent sign. */
	public static final char PERCENT_SIGN_CHAR = 0x0025;
	/** An apostrophe character. */
	public static final char APOSTROPHE_CHAR = 0x0027;
	/** A plus sign character. */
	public static final char PLUS_SIGN_CHAR = 0x002B;
	/** A comma character. */
	public static final char COMMA_CHAR = 0x002C;
	/** A hyphen or minus character. */
	public static final char HYPHEN_MINUS_CHAR = 0x002D;
	/** A solidus or slash character (002F;SOLIDUS;Po;0;CS;;;;;N;SLASH;;;;). */
	public static final char SOLIDUS_CHAR = 0x002F;
	/** A colon character. */
	public static final char COLON_CHAR = 0x003A;
	/** A semicolon character. */
	public static final char SEMICOLON_CHAR = 0x003B;
	/** A less-than sign character (003C;LESS-THAN SIGN;Sm;0;ON;;;;;Y;;;;;). */
	public static final char LESS_THAN_CHAR = 0x003C;
	/** An equals sign character (003D;EQUALS SIGN;Sm;0;ON;;;;;N;;;;;). */
	public static final char EQUALS_SIGN_CHAR = 0x003D;
	/** A greater-than sign character (003E;GREATER-THAN SIGN;Sm;0;ON;;;;;Y;;;;;). */
	public static final char GREATER_THAN_CHAR = 0x003E;
	/** A question mark character (003F;QUESTION MARK;Po;0;ON;;;;;N;;;;;). */
	public static final char QUESTION_MARK_CHAR = 0x003F;
	/** A grave accent character. */
	public static final char GRAVE_ACCENT_CHAR = 0x0060;
	/** A tilde character (007E;TILDE;Sm;0;ON;;;;;N;;;;;). */
	public static final char TILDE_CHAR = 0x007E;
	/** A next line (NEL) control character. */
	public static final char NEXT_LINE_CHAR = 0x0085;
	/** A start of string control character. */
	public static final char START_OF_STRING_CHAR = 0x0098;
	/** A string terminator control character. */
	public static final char STRING_TERMINATOR_CHAR = 0x009C;
	/** Unicode no-break space (NBSP). */
	public static final char NO_BREAK_SPACE_CHAR = 0x00A0;
	/** The copyright symbol. */
	public static final char COPYRIGHT_SIGN = 0x00A9;
	/** A left-pointing guillemet character. */
	public static final char LEFT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR = 0x00AB;
	/**
	 * The pilcrow or paragraph sign.
	 * @see #PARAGRAPH_SIGN_CHAR
	 */
	public static final char PILCROW_SIGN_CHAR = 0x00B6;
	/**
	 * The paragraph sign.
	 * @see #PILCROW_SIGN_CHAR
	 */
	public static final char PARAGRAPH_SIGN_CHAR = PILCROW_SIGN_CHAR;
	/** A middle dot character. */
	public static final char MIDDLE_DOT_CHAR = 0x00B7;
	/** A right-pointing guillemet character. */
	public static final char RIGHT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR = 0x00BB;
	/** An uppercase oe ligature. */
	public static final char LATIN_CAPITAL_LIGATURE_OE_CHAR = 0x0152;
	/** A lowercase oe ligature. */
	public static final char LATIN_SMALL_LIGATURE_OE_CHAR = 0x0153;
	/** A Y umlaut. */
	public static final char LATIN_CAPITAL_LETTER_Y_WITH_DIAERESIS_CHAR = 0x0178;
	/** A zero-width space (ZWSP) that may expand during justification. */
	public static final char ZERO_WIDTH_SPACE_CHAR = 0x200B;
	/** A zero-width non-joiner (200C;ZERO WIDTH NON-JOINER;Cf;0;BN;;;;;N;;;;;). */
	public static final char ZERO_WIDTH_NON_JOINER_CHAR = 0x200C;
	/** A zero-width joiner (200D;ZERO WIDTH JOINER;Cf;0;BN;;;;;N;;;;;). */
	public static final char ZERO_WIDTH_JOINER_CHAR = 0x200D;
	/** A left-to-right mark (200E;LEFT-TO-RIGHT MARK;Cf;0;L;;;;;N;;;;;). */
	public static final char LEFT_TO_RIGHT_MARK_CHAR = 0x200E;
	/** A right-to-right mark (200F;RIGHT-TO-LEFT MARK;Cf;0;R;;;;;N;;;;;). */
	public static final char RIGHT_TO_LEFT_MARK_CHAR = 0x200F;
	/** A zero-width non-breaking space—word joiner (WJ). */
	public static final char WORD_JOINER_CHAR = 0x2060;
	/** A left single quote. */
	public static final char LEFT_SINGLE_QUOTATION_MARK_CHAR = 0x2018;
	/** A right single quote. */
	public static final char RIGHT_SINGLE_QUOTATION_MARK_CHAR = 0x2019;
	/** A single low-9 quotation mark. */
	public static final char SINGLE_LOW_9_QUOTATION_MARK_CHAR = 0x201A;
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
	/** A single high-reversed-9 quotation mark. */
	public static final char SINGLE_HIGH_REVERSED_9_QUOTATION_MARK_CHAR = 0x201B;
	/** A left double quote. */
	public static final char LEFT_DOUBLE_QUOTATION_MARK_CHAR = 0x201C;
	/** A right double quote. */
	public static final char RIGHT_DOUBLE_QUOTATION_MARK_CHAR = 0x201D;
	/** A double low-9 quotation mark. */
	public static final char DOUBLE_LOW_9_QUOTATION_MARK_CHAR = 0x201E;
	/** A double high-reversed-9 quotation mark. */
	public static final char DOUBLE_HIGH_REVERSED_9_QUOTATION_MARK_CHAR = 0x201F;
	/** Unicode en dash character. */
	public static final char EN_DASH_CHAR = 0x2013;
	/** Unicode em dash character. */
	public static final char EM_DASH_CHAR = 0x2014;
	/** Unicode bullet character. */
	public static final char BULLET_CHAR = 0x2022;
	/** Unicode horizontal ellipsis. */
	public static final char HORIZONTAL_ELLIPSIS_CHAR = 0x2026;
	/** A line separator character (2028;LINE SEPARATOR;Zl;0;WS;;;;;N;;;;;). */
	public static final char LINE_SEPARATOR_CHAR = 0x2028;
	/** A paragraph separator character (2029;PARAGRAPH SEPARATOR;Zp;0;B;;;;;N;;;;;). */
	public static final char PARAGRAPH_SEPARATOR_CHAR = 0x2029;
	/** A left-pointing single guillemet character. */
	public static final char SINGLE_LEFT_POINTING_ANGLE_QUOTATION_MARK_CHAR = 0x2039;
	/** A right-pointing single guillemet character. */
	public static final char SINGLE_RIGHT_POINTING_ANGLE_QUOTATION_MARK_CHAR = 0x203A;
	/** Trademark character. */
	public static final char TRADE_MARK_SIGN_CHAR = 0x2122;
	/** Infinity symbol (221E;INFINITY;Sm;0;ON;;;;;N;;;;;). */
	public static final char INFINITY_CHAR = 0x2122;
	/** A left-pointing angle bracket character (2329;LEFT-POINTING ANGLE BRACKET;Ps;0;ON;3008;;;;Y;BRA;;;;). */
	public static final char LEFT_POINTING_ANGLE_BRACKET = 0x2329;
	/** A right-pointing angle bracket character (232A;RIGHT-POINTING ANGLE BRACKET;Pe;0;ON;3009;;;;Y;KET;;;;). */
	public static final char RIGHT_POINTING_ANGLE_BRACKET = 0x232A;

	/** The symbol for NULL (2400;SYMBOL FOR NULL;So;0;ON;;;;;N;GRAPHIC FOR NULL;;;;). */
	public static final char NULL_SYMBOL = 0x2400;
	/** The symbol for line feed (240A;SYMBOL FOR LINE FEED;So;0;ON;;;;;N;GRAPHIC FOR LINE FEED;;;;). */
	public static final char LINE_FEED_SYMBOL = 0x240A;
	/** The symbol for vertical tab (240B;SYMBOL FOR VERTICAL TABULATION;So;0;ON;;;;;N;GRAPHIC FOR VERTICAL TABULATION;;;;). */
	public static final char VERTICAL_TAB_SYMBOL = 0x240B;
	/** The symbol for form feed (240C;SYMBOL FOR FORM FEED;So;0;ON;;;;;N;GRAPHIC FOR FORM FEED;;;;). */
	public static final char FORM_FEED_SYMBOL = 0x240C;
	/** The symbol for carriage return (240D;SYMBOL FOR CARRIAGE RETURN;So;0;ON;;;;;N;GRAPHIC FOR CARRIAGE RETURN;;;;). */
	public static final char CARRIAGE_RETURN_SYMBOL = 0x240D;
	/** The symbol for end of transmission (2404;SYMBOL FOR END OF TRANSMISSION;So;0;ON;;;;;N;GRAPHIC FOR END OF TRANSMISSION;;;;). */
	public static final char END_OF_TRANSMISSION_SYMBOL = 0x2404;
	/** The symbol for space (2420;SYMBOL FOR SPACE;So;0;ON;;;;;N;GRAPHIC FOR SPACE;;;;). */
	public static final char SPACE_SYMBOL = 0x2420;
	/** The blank symbol (2422;BLANK SYMBOL;So;0;ON;;;;;N;BLANK;;;;). */
	public static final char BLANK_SYMBOL = 0x2422;

	/** A reversed double prime quotation mark. */
	public static final char REVERSED_DOUBLE_PRIME_QUOTATION_MARK_CHAR = 0x301D;
	/** A double prime quotation mark. */
	public static final char DOUBLE_PRIME_QUOTATION_MARK_CHAR = 0x301E;
	/** A low double prime quotation mark. */
	public static final char LOW_DOUBLE_PRIME_QUOTATION_MARK_CHAR = 0x301F;
	/** A full width quotation mark. */
	public static final char FULLWIDTH_QUOTATION_MARK_CHAR = 0xFF02;
	/**
	 * A zero-width no-breaking space (ZWNBSP)—the Byte Order Mark (BOM) (FEFF;ZERO WIDTH NO-BREAK SPACE;Cf;0;BN;;;;;N;BYTE ORDER MARK;;;;). For non-breaking
	 * purposes, deprecated in favor of <code>WORD_JOINER_CHAR</code>.
	 * @see #WORD_JOINER_CHAR
	 */
	public static final char ZERO_WIDTH_NO_BREAK_SPACE_CHAR = 0xFEFF;
	/**
	 * The Byte Order Mark (BOM).
	 * @see #ZERO_WIDTH_NO_BREAK_SPACE_CHAR
	 */
	public static final char BOM_CHAR = ZERO_WIDTH_NO_BREAK_SPACE_CHAR;
	/** A character for a placeholder in text for an otherwise unspecified object. */
	public static final char OBJECT_REPLACEMENT_CHAR = 0xFFFC;
	/** Represents a character that is unknown or unrepresentable in Unicode. */
	public static final char REPLACEMENT_CHAR = 0xFFFD;
	/** An invalid, undefined Unicode character which is "guaranteed not to be a Unicode character at all. */
	public static final char UNDEFINED_CHAR = 0xFFFF;

	/** Unicode control characters (0x0000-0x001F, 0x007F-0x09F). */
	public static final String CONTROL_CHARS = "" + (char)0x0000 + (char)0x0001 + (char)0x0002 + (char)0x0003 + (char)0x0004 + (char)0x0005 + (char)0x0006
			+ (char)0x0007 + (char)0x0008 + (char)0x0009 + (char)0x000A + (char)0x000B + (char)0x000C + (char)0x000D + (char)0x000E + (char)0x000F + (char)0x0010
			+ (char)0x0011 + (char)0x0012 + (char)0x0013 + (char)0x0014 + (char)0x0015 + (char)0x0016 + (char)0x0017 + (char)0x0018 + (char)0x0019 + (char)0x001A
			+ (char)0x001B + (char)0x001C + (char)0x001D + (char)0x001E + (char)0x001F + (char)0x007F + (char)0x0080 + (char)0x0081 + (char)0x0082 + (char)0x0083
			+ (char)0x0084 + (char)0x0085 + (char)0x0086 + (char)0x0087 + (char)0x0088 + (char)0x0089 + (char)0x008A + (char)0x008B + (char)0x008C + (char)0x008D
			+ (char)0x008E + (char)0x008F + (char)0x0090 + (char)0x0091 + (char)0x0092 + (char)0x0093 + (char)0x0094 + (char)0x0095 + (char)0x0096 + (char)0x0097
			+ (char)0x0098 + (char)0x0099 + (char)0x009A + (char)0x009B + (char)0x009C + (char)0x009D + (char)0x009E + (char)0x009F;

	/** Unicode paragraph separator characters. */
	public static final String PARAGRAPH_SEPARATOR_CHARS = "" + LINE_FEED_CHAR + CARRIAGE_RETURN_CHAR + INFORMATION_SEPARATOR_FOUR_CHAR
			+ INFORMATION_SEPARATOR_THREE_CHAR + INFORMATION_SEPARATOR_TWO_CHAR + NEXT_LINE_CHAR + PARAGRAPH_SEPARATOR_CHAR;

	/** Unicode segment separator characters. */
	public static final String SEGMENT_SEPARATOR_CHARS = "" + CHARACTER_TABULATION_CHAR + LINE_TABULATION_CHAR + INFORMATION_SEPARATOR_ONE_CHAR;

	/**
	 * Unicode newline characters.
	 * @see <a href="http://unicode.org/unicode/standard/reports/tr13/tr13-5.html">Unicode Newline Guidelines</a>
	 */
	public static final Characters NEWLINE_CHARACTERS = of(CARRIAGE_RETURN_CHAR, LINE_FEED_CHAR, NEXT_LINE_CHAR, LINE_SEPARATOR_CHAR, FORM_FEED_CHAR,
			LINE_SEPARATOR_CHAR, PARAGRAPH_SEPARATOR_CHAR);

	/** Unicode whitespace characters. */
	public static final Characters WHITESPACE_CHARACTERS = NEWLINE_CHARACTERS.add(CHARACTER_TABULATION_CHAR, SPACE_CHAR); //TODO finish

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

	/**
	 * Unicode formatting characters; Unicode characters marked with "Cf", such as <code>WORD_JOINER</code>.
	 */
	public static final String FORMAT_CHARS = "" + ZERO_WIDTH_NON_JOINER_CHAR + ZERO_WIDTH_JOINER_CHAR + LEFT_TO_RIGHT_MARK_CHAR + RIGHT_TO_LEFT_MARK_CHAR
			+ WORD_JOINER_CHAR + ZERO_WIDTH_NO_BREAK_SPACE_CHAR;

	/**
	 * Characters that do not contain visible "content", and may be trimmed from ends of a string. These include whitespace, control characters, and formatting
	 * characters.
	 */
	public static final Characters TRIM_CHARACTERS = WHITESPACE_CHARACTERS.add(CONTROL_CHARS).add(FORMAT_CHARS);

	/**
	 * A regular expression pattern for the class of trim characters.
	 * @see #TRIM_CHARS
	 */
	//TODO del if not needed	public static final Pattern TRIM_PATTERN=Pattern.compile("["+TRIM_CHARS+"]");

	/**
	 * Characters that delimit a list separated by trim characters, commas, and/or semicolons.
	 * @see #TRIM_CHARACTERS
	 */
	public static final String LIST_DELIMITER_CHARS = TRIM_CHARACTERS + ",;";

	/**
	 * A regular expression character class pattern for the class of list delimiter characters.
	 * @see #LIST_DELIMITER_CHARS
	 */
	//TODO del if not needed	public static final Pattern LIST_DELIMITER_CLASS_PATTERN=Pattern.compile(createCharacterClass(LIST_DELIMITER_CHARS));

	/** Characters that could be considered the start of a quotation. */
	public static final String LEFT_QUOTE_CHARS = "" + QUOTATION_MARK_CHAR + APOSTROPHE_CHAR + REVERSED_DOUBLE_PRIME_QUOTATION_MARK_CHAR
			+ DOUBLE_PRIME_QUOTATION_MARK_CHAR + LOW_DOUBLE_PRIME_QUOTATION_MARK_CHAR + FULLWIDTH_QUOTATION_MARK_CHAR + LEFT_SINGLE_QUOTATION_MARK_CHAR
			+ LEFT_DOUBLE_QUOTATION_MARK_CHAR + LEFT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR + SINGLE_LOW_9_QUOTATION_MARK_CHAR + DOUBLE_LOW_9_QUOTATION_MARK_CHAR
			+ SINGLE_LEFT_POINTING_ANGLE_QUOTATION_MARK_CHAR;

	/** Characters that could be considered the end of a quotation. */
	public static final String RIGHT_QUOTE_CHARS = "" + QUOTATION_MARK_CHAR + APOSTROPHE_CHAR + REVERSED_DOUBLE_PRIME_QUOTATION_MARK_CHAR
			+ DOUBLE_PRIME_QUOTATION_MARK_CHAR + LOW_DOUBLE_PRIME_QUOTATION_MARK_CHAR + FULLWIDTH_QUOTATION_MARK_CHAR + RIGHT_SINGLE_QUOTATION_MARK_CHAR
			+ RIGHT_DOUBLE_QUOTATION_MARK_CHAR + RIGHT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR + SINGLE_HIGH_REVERSED_9_QUOTATION_MARK_CHAR
			+ DOUBLE_HIGH_REVERSED_9_QUOTATION_MARK_CHAR + SINGLE_RIGHT_POINTING_ANGLE_QUOTATION_MARK_CHAR;

	/** Characters that start or end quotations. */
	public static final String QUOTE_CHARS = "" + QUOTATION_MARK_CHAR + APOSTROPHE_CHAR + REVERSED_DOUBLE_PRIME_QUOTATION_MARK_CHAR
			+ DOUBLE_PRIME_QUOTATION_MARK_CHAR + LOW_DOUBLE_PRIME_QUOTATION_MARK_CHAR + FULLWIDTH_QUOTATION_MARK_CHAR + LEFT_SINGLE_QUOTATION_MARK_CHAR
			+ RIGHT_SINGLE_QUOTATION_MARK_CHAR + LEFT_DOUBLE_QUOTATION_MARK_CHAR + RIGHT_DOUBLE_QUOTATION_MARK_CHAR + LEFT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR
			+ RIGHT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR + SINGLE_LOW_9_QUOTATION_MARK_CHAR + SINGLE_HIGH_REVERSED_9_QUOTATION_MARK_CHAR
			+ DOUBLE_LOW_9_QUOTATION_MARK_CHAR + DOUBLE_HIGH_REVERSED_9_QUOTATION_MARK_CHAR + SINGLE_LEFT_POINTING_ANGLE_QUOTATION_MARK_CHAR
			+ SINGLE_RIGHT_POINTING_ANGLE_QUOTATION_MARK_CHAR;

	/** Characters used to punctuate phrases and sentences. */
	public static final Characters PHRASE_PUNCTUATION_CHARACTERS = of('.', ',', ':', ';', '?', '!'); //TODO use constants here

	/** Punctuation that expects a character to follow at some point. */
	public static final Characters DEPENDENT_PUNCTUATION_CHARACTERS = of(COLON_CHAR, ';', COMMA_CHAR, HYPHEN_MINUS_CHAR, EM_DASH_CHAR, EN_DASH_CHAR); //TODO use a constant

	/** Left punctuation used to group characters. */
	public static final Characters LEFT_GROUP_PUNCTUATION_CHARACTERS = of('(', '[', '{', '<'); //TODO use constants

	/** Right punctuation used to group characters. */
	public static final Characters RIGHT_GROUP_PUNCTUATION_CHARACTERS = of(')', ']', '}', '>'); //TODO use constants

	/** Punctuation used to group characters. */
	public static final Characters GROUP_PUNCTUATION_CHARACTERS = LEFT_GROUP_PUNCTUATION_CHARACTERS.add(RIGHT_GROUP_PUNCTUATION_CHARACTERS);

	/**
	 * Characters used to punctuate phrases and sentences, as well as general punctuation such as quotes.
	 */
	public static final Characters PUNCTUATION_CHARS = PHRASE_PUNCTUATION_CHARACTERS.add(GROUP_PUNCTUATION_CHARACTERS).add(QUOTE_CHARS).add(HYPHEN_MINUS_CHAR)
			.add(EM_DASH_CHAR).add(EN_DASH_CHAR);

	/** Characters that separate words. */
	public static final Characters WORD_DELIMITER_CHARACTERS = WHITESPACE_CHARACTERS.add(PUNCTUATION_CHARS); //TODO this needs fixed

	/**
	 * A regular expression pattern for the class of word delimiter characters.
	 * @see #WORD_DELIMITER_CHARS
	 */
	//TODO fix; these characters must be escaped, or this Pattern.toString() will run into an endless loop!	public static final Pattern WORD_DELIMITER_PATTERN=Pattern.compile("["+WORD_DELIMITER_CHARS+"]");

	//Unicode categories; see http://www.unicode.org/reports/tr44/#General_Category_Values

	/** Characters in the Unicode <code>Space_Separator</code> (<code>Zs</code>) category as of Unicode 9.0.0. */
	public static final Characters SPACE_SEPARATOR_CHARACTERS = of(SPACE_CHAR, NO_BREAK_SPACE_CHAR, '\u1680').add(ofRange('\u2000', '\u200A')).add('\u202F')
			.add('\u205F').add('\u3000');

	/** Characters in the Unicode <code>Line_Separator</code> (<code>Zl</code>) category as of Unicode 9.0.0. */
	public static final Characters LINE_SEPARATOR_CHARACTERS = of(LINE_SEPARATOR_CHAR);

	/** Characters in the Unicode <code>Paragraph_Separator</code> (<code>Zp</code>) category as of Unicode 9.0.0. */
	public static final Characters PARAGRAPH_SEPARATOR_CHARACTERS = of(PARAGRAPH_SEPARATOR_CHAR);

	/** Characters considered to be end-of-line markers (e.g. CR and LF). */
	public static final Characters EOL_CHARACTERS = of(CARRIAGE_RETURN_CHAR, LINE_FEED_CHAR).add(LINE_SEPARATOR_CHARACTERS).add(PARAGRAPH_SEPARATOR_CHARACTERS);

	/**
	 * Characters in the Unicode <code>Separator</code> (<code>Z</code>) group as of Unicode 9.0.0.
	 * @see #SPACE_SEPARATOR_CHARACTERS
	 * @see #LINE_SEPARATOR_CHARACTERS
	 * @see #PARAGRAPH_SEPARATOR_CHARACTERS
	 */
	public static final Characters SEPARATOR_CHARACTERS = of(SPACE_SEPARATOR_CHARACTERS, LINE_SEPARATOR_CHARACTERS, PARAGRAPH_SEPARATOR_CHARACTERS);

	/** Characters that allow words to wrap. */
	public static final String WORD_WRAP_CHARS = WHITESPACE_CHARACTERS + "-/"; //TODO use constants

	/** The set of sorted characters, with no duplicates or surrogates. */
	private final char[] chars;

	/** The lowest character, or -1 if there are no characters. */
	private final int minChar;

	/** The highest character, or -1 if there are no characters.. */
	private final int maxChar;

	/**
	 * Characters constructor.
	 * @apiNote This method is only to be used internally with preprocessed data. The given character array is used as-is, and is expected to be sorted, with no
	 *          surrogate characters, and with duplicates removed.
	 * @param characters The characters to store.
	 * @throws NullPointerException if the given characters is <code>null</code>.
	 */
	private Characters(final char[] characters) {
		if(characters.length > 0) { //if this is not an empty array of characters
			minChar = characters[0];
			maxChar = characters[characters.length - 1];
		} else { //if there are no characters
			minChar = maxChar = -1;
		}
		this.chars = characters; //save the processed characters
	}

	/**
	 * Characters factory method. Duplicates are ignored.
	 * @param characters The characters to store.
	 * @return An instance of {@link Characters} with the given characters stored.
	 * @throws NullPointerException if the given characters is <code>null</code>.
	 * @throws IllegalArgumentException if the given characters contain Unicode surrogate characters.
	 */
	public static Characters of(final char... characters) {
		return of(characters, 0, characters.length);
	}

	/**
	 * Characters factory method. Duplicates are ignored.
	 * @param characters The characters to store.
	 * @param start The start index, inclusive.
	 * @param end The end index, exclusive.
	 * @return An instance of {@link Characters} with the given characters stored.
	 * @throws NullPointerException if the given characters is <code>null</code>.
	 * @throws IllegalArgumentException if the given characters contain Unicode surrogate characters.
	 */
	public static Characters of(char[] characters, final int start, final int end) {
		checkIndexRange(characters.length, start, end);
		final int length = end - start;
		if(length == 0) {
			return NONE;
		}
		//create a defensive copy of the input
		if(start == 0 && end == characters.length) { //if we're using the whole array
			characters = characters.clone(); //create a copy of the characters so that we can control the array completely
		} else { //if we're using part of the array
			final char[] tempCharacters = new char[length]; //create a new array
			System.arraycopy(characters, start, tempCharacters, 0, length); //copy the characters
			characters = tempCharacters; //use the copy
		}
		sort(characters); //sort the characters
		boolean duplicates = false; //start by assuming there are no duplicates
		for(int i = length - 1; i > 0 && !duplicates; --i) { //check the characters for duplicates; iterate down to the second character
			final char c = characters[i];
			checkArgument(!Character.isSurrogate(c), "Characters contain surrogate character: %s.", getLabel(c));
			if(c == characters[i - 1]) { //because the characters are now sorted, we just check for two of the same character side-by-side
				duplicates = true;
			}
		}
		final char firstChar = characters[0];
		checkArgument(!Character.isSurrogate(characters[0]), "Characters contain surrogate character: %s.", getLabel(firstChar)); //check the first character, which we skipped
		if(duplicates) { //if there are duplicates, remove them
			int lastChar = -1;
			final StringBuilder stringBuilder = new StringBuilder(characters.length);
			for(final char c : characters) {
				if(c != lastChar) { //if this is not a duplicate (the characters are in order, so duplicates will always be side-by-side)
					stringBuilder.append(c);
					lastChar = c; //indicate the last character we looked at, so we can prevent duplicates
				}
			}
			characters = toCharArray(stringBuilder); //use the characters in the string builder, which no longer contain duplicates
			//note that length is no longer valid here, but there is no point in updating it now
		}
		return new Characters(characters);
	}

	/**
	 * Creates a range of characters.
	 * @param first The first of the range, inclusive.
	 * @param last The last of the range, inclusive.
	 * @return Characters representing the indicated range.
	 * @throws IllegalArgumentException if the last character comes before the first character.
	 */
	public static Characters ofRange(final char first, final char last) {
		if(last < first) {
			throw new IllegalArgumentException("Last character in range " + getLabel(last) + " cannot come before first character " + getLabel(first) + ".");
		}
		final int length = last - first + 1;
		final char[] chars = new char[length];
		for(int i = 0; i < length; ++i) {
			chars[i] = (char)(first + i);
		}
		return new Characters(chars);
	}

	/**
	 * Characters factory method from existing {@link Characters} instances. Duplicates are ignored.
	 * @param multipleCharacters The {@link Characters} instances containing characters to store.
	 * @return An instance of {@link Characters} with the given characters stored.
	 * @throws NullPointerException if the given characters is <code>null</code>.
	 * @throws IllegalArgumentException if the given characters contain Unicode surrogate characters.
	 */
	public static Characters of(final Characters... multipleCharacters) {
		Characters characters = NONE; //start with no characters
		if(multipleCharacters.length > 0) { //if there are characters
			for(final Characters moreCharacters : multipleCharacters) { //go through add add each of the character sets
				characters = characters.add(moreCharacters);
			}
		}
		return characters;
	}

	/**
	 * Character sequence factory method. Duplicates are ignored.
	 * @param charSequence The character sequence containing characters to store.
	 * @return An instance of {@link Characters} with the characters contained on the given char sequence.
	 * @throws NullPointerException if the given character sequence is <code>null</code>.
	 * @throws IllegalArgumentException if the given character sequence contains Unicode surrogate characters.
	 */
	public static Characters from(final CharSequence charSequence) {
		return of(toCharArray(charSequence));
	}

	/** @return <code>true</code> if this object contains no characters. */
	public boolean isEmpty() {
		return chars.length == 0;
	}

	/** @return The number of characters. */
	public int size() {
		return chars.length;
	}

	/**
	 * Creates a new object with these characters and the given characters. Duplicates are ignored.
	 * @param characters The characters to add.
	 * @return A new object containing these characters and the given characters.
	 * @throws NullPointerException if the given characters is <code>null</code>.
	 * @throws IllegalArgumentException if the given characters contain Unicode surrogate characters.
	 */
	public Characters add(final Characters characters) {
		return add(characters.chars);
	}

	/**
	 * Creates a new object with these characters and the given characters. Duplicates are ignored.
	 * @param characters The characters to add.
	 * @return A new object containing these characters and the given characters.
	 * @throws NullPointerException if the given characters is <code>null</code>.
	 * @throws IllegalArgumentException if the given characters contain Unicode surrogate characters.
	 */
	public Characters add(final char... characters) {
		final int length = characters.length;
		return length > 0 ? Characters.from(toStringBuilder(length).append(characters)) //get a string builder from the characters and append the given characters to create a new object
				: this; //if nothing is being added, return these characters
	}

	/**
	 * Creates a new object with these characters and the given characters. Duplicates are ignored.
	 * @param charSequence The characters to add.
	 * @return A new object containing these characters and the given characters.
	 * @throws NullPointerException if the given character sequence <code>null</code>.
	 * @throws IllegalArgumentException if the given character sequence contains Unicode surrogate characters.
	 */
	public Characters add(final CharSequence charSequence) {
		final int length = charSequence.length();
		return length > 0 ? Characters.from(toStringBuilder(charSequence.length()).append(charSequence)) //get a string builder from the characters and append the given characters to create a new object
				: this; //if nothing is being added, return these characters
	}

	/**
	 * Adds a range of characters.
	 * @param first The first of the range, inclusive.
	 * @param last The last of the range, inclusive.
	 * @return A new object containing these characters and the given range of characters.
	 * @throws IllegalArgumentException if the last character comes before the first character.
	 */
	public Characters addRange(final char first, final char last) {
		return add(Characters.ofRange(first, last));
	}

	/**
	 * Creates a new object with these characters, with the given characters removed.
	 * @param characters The characters to remove.
	 * @return A new object containing these characters without the given characters.
	 * @throws NullPointerException if the given characters is <code>null</code>.
	 */
	public Characters remove(final Characters characters) {
		return remove(characters.chars);
	}

	/**
	 * Creates a new object with these characters, with the given characters removed.
	 * @param characters The characters to remove.
	 * @return A new object containing these characters without the given characters.
	 * @throws NullPointerException if the given characters is <code>null</code>.
	 */
	public Characters remove(final char... characters) {
		for(final char character : characters) { //see if we have any of the given characters
			if(contains(character)) { //if there is at least one character to remove
				final StringBuilder stringBuilder = new StringBuilder(chars.length); //create a new string builder with enough room for our current characters
				for(final char c : chars) { //look at all our current characters
					if(!Arrays.contains(characters, c)) { //if this is not a character to remove
						stringBuilder.append(c); //add this character to our string builder
					}
				}
				return Characters.from(stringBuilder); //return new characters from our string builder, with the requested characters removed
			}
		}
		return this; //if we didn't have any of the given characters to begin with, return the characters the way they are
	}

	/**
	 * Creates a new object with these characters, with the given characters removed.
	 * @param charSequence The characters to add.
	 * @return A new object containing these characters without the given characters.
	 * @throws NullPointerException if the given character sequence <code>null</code>.
	 */
	public Characters remove(final CharSequence charSequence) {
		for(int i = charSequence.length() - 1; i >= 0; --i) { //see if we have any of the given characters
			if(contains(charSequence.charAt(i))) { //if there is at least one character to remove
				final StringBuilder stringBuilder = new StringBuilder(chars.length); //create a new string builder with enough room for our current characters
				for(final char c : chars) { //look at all our current characters
					if(!CharSequences.contains(charSequence, c)) { //if this is not a character to remove
						stringBuilder.append(c); //add this character to our string builder
					}
				}
				return Characters.from(stringBuilder); //return new characters from our string builder, with the requested characters removed
			}
		}
		return this; //if we didn't have any of the given characters to begin with, return the characters the way they are
	}

	/**
	 * Splits a character sequence on the these characters. Runs of matching characters are removed and the interspersed tokens are returned.
	 * @implNote This method is likely more efficient than a regular expression-based approach, especially in situations in which splitting is likely to occur at
	 *           a small frequency, because the setup cost is low and individual character testing is efficient.
	 * @implSpec The current implementation does not support surrogate characters.
	 * @param charSequence The character sequence to split.
	 * @return A list of subsequences; the list may not be mutable.
	 */
	public List<String> split(final CharSequence charSequence) {
		ArrayList<String> tokens = null; //we'll only create this if we have to
		final int length = charSequence.length();
		int start = 0;
		while(start < length) {
			//skip delimiters
			while(start < length && contains(charSequence.charAt(start))) {
				++start;
			}
			if(start == length) { //if we've reached the end, stop
				break;
			}
			//gather all non-delimiters to form a token
			int end = start + 1;
			while(end < length && !contains(charSequence.charAt(end))) {
				++end;
			}
			if(tokens == null) { //if we haven't created any subsequences
				if(start == 0 && end == length) { //if the whole character sequence is non-delimiters (there were no delimiters) 
					return singletonList(charSequence.toString()); //there is only one token to return
				}
				tokens = new ArrayList<>(); //if we're really splitting the string, we'll need a new list
			}
			tokens.add(charSequence.subSequence(start, end).toString()); //add this subsequence
			start = end; //start over at the end of this subsequence
		}
		return tokens != null ? tokens : emptyList();
	}

	/** @return A string containing these characters. */
	public String toString() {
		return new String(chars);
	}

	/**
	 * Returns a string representing an array of these characters, each character represented as 'x', or if the character is a control character, the Unicode code
	 * point of this character, e.g. "U+1234". Example: "['a', 0x0020]"
	 * @implSpec This method does not treat surrogate characters specially.
	 * @return A string containing an array representation of these characters.
	 */
	public String toLabelArrayString() {
		return appendLabelArrayString(new StringBuilder(chars.length * 8), chars).toString(); //most of the time, each character will just take up five to eight characters
	}

	/**
	 * A string builder containing these characters. This implementation provides an initial capacity for 16 more characters.
	 * @return A string builder containing these characters.
	 * @see StringBuilder#append(char[])
	 */
	public StringBuilder toStringBuilder() {
		return toStringBuilder(16);
	}

	/**
	 * A string builder containing these characters, with an initial capacity with room for the specified number of extra characters.
	 * @param extraCapacity The extra initial capacity.
	 * @return A string builder containing these characters.
	 * @throws IllegalArgumentException if the given capacity is negative.
	 * @see StringBuilder#append(char[])
	 */
	public StringBuilder toStringBuilder(final int extraCapacity) {
		return new StringBuilder(chars.length + checkArgumentNotNegative(extraCapacity)).append(chars); //allow room for more characters
	}

	/**
	 * Determines whether the given character is contained in these characters.
	 * @param character The character to check.
	 * @return <code>true</code> if the character exists in these characters.
	 */
	public boolean contains(final char character) {
		if(character < minChar || character > maxChar) { //do quick bounds checking
			return false;
		}
		//TODO add a flag indicating if the range is complete and uninterrupted, so that individual characters wouldn't have to be checked
		for(final char c : chars) { //look at each character
			if(c == character) { //if we found the character
				return true;
			} else if(c > character) { //if we've gone beyond the character, the character doesn't exist, because the characters are in order
				return false;
			}
		}
		return false; //we couldn't find the character
	}

	/**
	 * Sees if the specified character is in one of the specified ranges.
	 * @param c The character to check.
	 * @param ranges An array of character pair arrays, <em>in order</em>, the first of each pair specifying the bottom inclusive character of a range, the second
	 *          of which specifying the top inclusive character of the range.
	 * @return <code>true</code> if the character is in one of the ranges, else <code>false</code>.
	 */
	public static boolean isCharInRange(final char c, final char[][] ranges) { //TODO improve code by doing a binary search
		final int rangeCount = ranges.length; //find out how many ranges there are
		for(int i = 0; i < rangeCount; ++i) { //look at each range
			final char[] range = ranges[i]; //get this range
			if(c < range[0]) { //if the character is lower than the lower bound, it's not in this range---and we've already checked previous ranges
				return false; //we've ran out of ranges that might work
			} else if(c <= range[1]) { //if the character is greater than or equal to the lower bound, see if the character is lower than or equal to the upper bound
				return true; //the character is within range
			}
		}
		return false; //if we get here, this means our character is higher than any of the characters, meaning the character is higher than all the ranges
	}

	/**
	 * Determines whether a character is in the ASCII character range (0x00-0x7F).
	 * @param c The character to examine.
	 * @return <code>true</code> if the character is an ASCII character.
	 */
	public static boolean isASCII(final char c) { //TODO reconcile with ASCII.isASCII()
		return c >= 0 && c < 0x80; //see if this character is between 0 and 128, inclusive
	}

	/**
	 * Determines whether a character is one of the digits '0'-'9'.
	 * @param c The character to examine.
	 * @return <code>true</code> if the character is an ISO_LATIN_1 digit.
	 * @deprecated Use {@link ASCII#DIGIT_CHARACTERS}.
	 */
	@Deprecated
	public static final boolean isLatinDigit(final char c) {
		return c >= '0' && c <= '9'; //see if the character falls in the range of the Latin digits
	}

	/**
	 * Specifies whether or not a given character is a punctuation mark.
	 * @param c Character to analyze.
	 * @return <code>true</code> if the character is punctuation.
	 */
	public static boolean isPunctuation(final char c) {
		return PUNCTUATION_CHARS.contains(c); //return true if we can find the character in the string of punctuation characters TODO update the list of punctuation characters
	}

	/**
	 * Determines whether a character is a Roman numeral.
	 * @param c The character to examine.
	 * @return <code>true</code> if the character is a Roman numeral.
	 */
	public static boolean isRomanNumeral(final char c) {
		return RomanNumerals.getValue(c) >= 0; //see if the character returns a valid Roman numeral value
	}

	/**
	 * Specifies whether or not a given character is whitespace.
	 * @param c Character to analyze.
	 * @return <code>true</code> if the character is whitespace.
	 */
	public static boolean isWhitespace(final char c) {
		return WHITESPACE_CHARACTERS.contains(c); //return true if we can find the character in the string of whitespace characters
	}

	/**
	 * Specifies whether or not a given character is a word delimiter, such as whitespace or punctuation.
	 * @param c Character to analyze.
	 * @return <code>true</code> if the character allows word wrapping.
	 */
	public static boolean isWordDelimiter(final char c) {
		return WORD_DELIMITER_CHARACTERS.contains(c); //return true if we can find the character in the string of word delimiter characters
	}

	/**
	 * Specifies whether or not a given character allows a word wrap.
	 * @param c Character to analyze.
	 * @return <code>true</code> if the character allows word wrapping.
	 */
	public static boolean isWordWrap(final char c) {
		return WORD_WRAP_CHARS.indexOf(c) >= 0; //return true if we can find the character in the string of word wrap characters
	}

	/**
	 * Returns a string representing the character as 'x', or if the character is a control character, the Unicode code point of this character, e.g. "U+1234".
	 * @implSpec This method supports Unicode supplementary code points.
	 * @param c The code point a string representation of which to append.
	 * @return The string label representing the character.
	 * @see #appendLabel(StringBuilder, int)
	 */
	public static String getLabel(final int c) {
		return appendLabel(new StringBuilder(), c).toString();
	}

	/**
	 * Returns a string representing an array of these characters, each character represented as 'x', or if the character is a control character, the Unicode code
	 * point of this character, e.g. "U+1234". Example: "['a', 0x0020]"
	 * @implSpec This method does not treat surrogate characters specially.
	 * @param characters The characters to return as a string of an array.
	 * @return A string containing an array representation of these characters.
	 */
	public static String toLabelArrayString(final char... characters) {
		return appendLabelArrayString(new StringBuilder(characters.length * 8), characters).toString(); //most of the time, each character will just take up five to eight characters
	}

	/**
	 * Returns a string representing an array of these characters, each character represented as 'x', or if the character is a control character, the Unicode code
	 * point of this character, e.g. "U+1234". Example: "['a', 0x0020]"
	 * @implSpec This method does not treat surrogate characters specially.
	 * @param characters The characters to return as a string of an array.
	 * @return A string containing an array representation of these characters.
	 */
	public static String toLabelArrayString(final CharSequence characters) {
		return appendLabelArrayString(new StringBuilder(characters.length() * 8), characters.toString().toCharArray()).toString(); //most of the time, each character will just take up five to eight characters
	}

	/**
	 * Converts an array of characters to an array of bytes, using the UTF-8 charset.
	 * @param characters The characters to convert to bytes.
	 * @return An array of bytes representing the given characters in the UTF-8 charset.
	 */
	public static byte[] toByteArray(final char[] characters) {
		return toByteArray(characters, UTF_8); //convert the characters using UTF-8
	}

	/**
	 * Converts an array of characters to an array of bytes, using the given character encoding.
	 * @param characters The characters to convert to bytes.
	 * @param charset The charset to use when converting characters to bytes.
	 * @return An array of bytes representing the given characters in the specified encoding.
	 */
	public static byte[] toByteArray(final char[] characters, final Charset charset) {
		final ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream(); //create a byte array output stream
		final Writer writer = new OutputStreamWriter(byteArrayOutputStream, charset); //create a writer for converting characters to bytes
		try {
			writer.write(characters); //write the characters to the writer in one batch (writing them individually would be extremely inefficient)
			writer.flush(); //flush everything we've written to the byte output stream
		} catch(IOException ioException) { //we don't expect any errors
			throw new AssertionError(ioException);
		}
		return byteArrayOutputStream.toByteArray(); //return the bytes we collected from the character conversion
	}

	/**
	 * Appends a string representing an array of characters, each character represented as 'x', or if the character is a control character, the Unicode code point
	 * of this character, e.g. "U+1234". Example: "['a', 0x0020]"
	 * @implSpec This method does not treat surrogate characters specially.
	 * @param stringBuilder The string builder to which the string will be appended.
	 * @param characters The characters the strings of the Unicode code points to append.
	 * @return The string builder.
	 * @throws NullPointerException if the given string builder is <code>null</code>.
	 */
	public static StringBuilder appendLabelArrayString(final StringBuilder stringBuilder, final char[] characters) {
		stringBuilder.append('[');
		final int last = characters.length - 1;
		for(int i = 0; i <= last; ++i) {
			appendLabel(stringBuilder, characters[i]); //append a string for the character
			if(i != last) { //if this is not the last character
				stringBuilder.append(',').append(' '); //add delimiters
			}
		}
		stringBuilder.append(']');
		return stringBuilder;
	}

	/**
	 * Appends a string representing the character as 'x', or if the character is a control character or a surrogate, either a special representation such as '\n'
	 * or the Unicode code point of this character, e.g. "U+1234".
	 * @implSpec This method supports Unicode supplementary code points.
	 * @param stringBuilder The string builder to which the string will be appended.
	 * @param c The code point a string representation of which to append.
	 * @return The string builder.
	 * @throws NullPointerException if the given string builder is <code>null</code>.
	 * @see #appendUnicodeString(StringBuilder, int)
	 */
	public static StringBuilder appendLabel(final StringBuilder stringBuilder, final int c) {
		if(Character.isISOControl(c) || (Character.isBmpCodePoint(c) && Character.isSurrogate((char)c))) { //if this is a control character or a surrogate
			switch(c) { //see which character this is
				case '\t': //tab
					stringBuilder.append("'\\t'");
					break;
				case '\r': //CR
					stringBuilder.append("'\\r'");
					break;
				case '\n': //LF
					stringBuilder.append("'\\n'");
					break;
				default: //if we don't recognize the character
					appendUnicodeString(stringBuilder, c); //append a Unicode representation
					break;
			}
		} else { //for all other characters
			stringBuilder.append('\'');
			appendChar(stringBuilder, c);
			stringBuilder.append('\'');
		}
		return stringBuilder;
	}

	/**
	 * Appends a string representing the Unicode code point of this character, e.g. "U+1234". The length of the added string depends on the Unicode code point;
	 * most code points will result in four hex characters.
	 * @param stringBuilder The string builder to which the string will be appended.
	 * @param c The code point the Unicode string of which to append.
	 * @return The string builder.
	 * @throws NullPointerException if the given string builder is <code>null</code>.
	 */
	public static StringBuilder appendUnicodeString(final StringBuilder stringBuilder, final int c) {
		stringBuilder.append('U').append('+'); //U+
		final int length = Character.isSupplementaryCodePoint(c) ? 6 : 4; //allow for supplementary Unicode code points
		stringBuilder.append(toHexString(c, length).toUpperCase()); //append the hex value of the character
		return stringBuilder;
	}

	/**
	 * Parses a string and returns its character value.
	 * @param string A string expected to contain a single character.
	 * @return The single character contained by the string.
	 * @throws NullPointerException if the given string is <code>null</code>
	 * @throws IllegalArgumentException if the string is not composed of a single character.
	 */
	public static final Character parseCharacter(final String string) {
		if(string.length() != 1) { //if this string isn't composed of a single character
			throw new IllegalArgumentException("The string \"" + string + "\" does not represent a single character.");
		}
		return Character.valueOf(string.charAt(0)); //return the first and only character in the string
	}

}
