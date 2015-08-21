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

package com.globalmentor.unicode;

/**
 * Constants used in Unicode 3.0.
 * @author Garret Wilson
 * @version 1.0
 */
public interface UnicodeConstants {

	/** The name of control character. */
	public static final String CONTROL_NAME = "<control>";

	//general cateogories
	//normative categories
	public static final String LETTER_UPPERCASE = "Lu";
	public static final String LETTER_LOWERCASE = "Ll";
	public static final String LETTER_TITLECASE = "Lt";
	public static final String MARK_NONSPACING = "Mn";
	public static final String MARK_SPACING_COMBINING = "Mc";
	public static final String MARK_ENCLOSING = "Me";
	public static final String NUMBER_DECIMAL_DIGIT = "Nd";
	public static final String NUMBER_LETTER = "Nl";
	public static final String NUMBER_OTHER = "No";
	public static final String SEPARATOR_SPACE = "Zs";
	public static final String SEPARATOR_LINE = "Zl";
	public static final String SEPARATOR_PARAGRAPH = "Zp";
	public static final String OTHER_CONTROL = "Cc";
	public static final String OTHER_FORMAT = "Cf";
	public static final String OTHER_SURROGATE = "Cs";
	public static final String OTHER_PRIVATE_USE = "Co";
	public static final String OTHER_NOT_ASSIGNED = "Cn";
	//informative categories
	public static final String LETTER_MODIFIER = "Lm";
	public static final String LETTER_OTHER = "Lo";
	public static final String PUNCTUATION_CONNECTOR = "Pc";
	public static final String PUNCTUATION_DASH = "Pd";
	public static final String PUNCTUATION_OPEN = "Ps";
	public static final String PUNCTUATION_CLOSE = "Pe";
	public static final String PUNCTUATION_INITIAL_QUOTE = "Pi";
	public static final String PUNCTUATION_FINAL = "Pf";
	public static final String PUNCTUATION_OTHER = "Po";
	public static final String SYMBOL_MATH = "Sm";
	public static final String SYMBOL_CURRENCY = "Sc";
	public static final String SYMBOL_MODIFIER = "Sk";
	public static final String SYMBOL_OTHER = "So";

	//bidirectional categories
	public static final String LEFT_TO_RIGHT = "L";
	public static final String LEFT_TO_RIGHT_EMBEDDING = "LRE";
	public static final String LEFT_TO_RIGHT_OVERRIDE = "LRO";
	public static final String RIGHT_TO_LEFT = "R";
	public static final String RIGHT_TO_LEFT_ARABIC = "AL";
	public static final String RIGHT_TO_LEFT_EMBEDDING = "RLE";
	public static final String RIGHT_TO_LEFT_OVERRIDE = "RLO";
	public static final String POP_DIRECTIONAL_FORMAT = "PDF";
	public static final String EUROPEAN_NUMBER = "EN";
	public static final String EUROPEAN_NUMBER_SEPARATOR = "ES";
	public static final String EUROPEAN_NUMBER_TERMINATOR = "ET";
	public static final String ARABIC_NUMBER = "AN";
	public static final String COMMON_NUMBER_SEPARATOR = "CS";
	public static final String NON_SPACING_MARK = "NSM";
	public static final String BOUNDARY_NEUTRAL = "BN";
	public static final String PARAGRAPH_SEPARATOR = "B";
	public static final String SEGMENT_SEPARATOR = "S";
	public static final String WHITESPACE = "WS";
	public static final String OTHER_NEUTRALS = "ON";

	//character decomposition mappings
	public static final String FONT = "<font>";
	public static final String NO_BREAK = "<noBreak>";
	public static final String INITIAL = "<initial>";
	public static final String MEDIAL = "<medial>";
	public static final String FINAL = "<final>";
	public static final String ISOLATED = "<isolated>";
	public static final String CIRCLE = "<circle>";
	public static final String SUPER = "<super>";
	public static final String SUB = "<sub>";
	public static final String VERTICLE = "<vertical>";
	public static final String WIDE = "<wide>";
	public static final String NARROW = "<narrow>";
	public static final String SMALL = "<small>";
	public static final String SQUARE = "<square>";
	public static final String FRACTION = "<fraction>";
	public static final String COMPAT = "<compat>";
	/** The character which introduces a character decomposition tag. */
	public static final char CHARACTER_DECOMPOSITION_TAG_BEGIN = FONT.charAt(0);

	//canonical combining classes
	public static final int SPACING_SPLIT_ENCLOSING_REORDRANT_AND_TIBETAN_SUBJOINED = 0;
	public static final int OVERLAYS_AND_INTERIOR = 1;
	public static final int NUKTAS = 7;
	public static final int HIRIGANA_KATAKANA_VOICING_MARKS = 8;
	public static final int VIRAMAS = 9;
	public static final int START_FIXED_POSITION_CLASS = 10;
	public static final int END_FIXED_POSITION_CLASS = 199;
	public static final int BELOW_LEFT_ATTACHED = 200;
	public static final int BELOW_ATTACHED = 202;
	public static final int BELOW_RIGHT_ATTACHED = 204;
	public static final int LEFT_ATTACHED = 208;
	public static final int RIGHT_ATTACHED = 210;
	public static final int ABOVE_LEFT_ATTACHED = 212;
	public static final int ABOVE_ATTACHED = 214;
	public static final int ABOVE_RIGHT_ATTACHED = 216;
	public static final int BELOW_LEFT = 218;
	public static final int BELOW = 220;
	public static final int BELOW_RIGHT = 222;
	public static final int LEFT = 224;
	public static final int RIGHT = 226;
	public static final int ABOVE_LEFT = 228;
	public static final int ABOVE = 230;
	public static final int ABOVE_RIGHT = 232;
	public static final int DOUBLE_BELOW = 233;
	public static final int DOUBLE_ABOVE = 234;
	public static final int BELOW_IOTA_SUBSCRIPT = 240;
}