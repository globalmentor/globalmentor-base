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

package com.globalmentor.text.unicode;

/**Constants used in Unicode 3.0.
@author Garret Wilson
@version 1.0
*/
public interface UnicodeConstants
{

	/**The name of control character.*/
	public final static String CONTROL_NAME="<control>";

			//general cateogories
		//normative categories
	public final static String LETTER_UPPERCASE="Lu";
	public final static String LETTER_LOWERCASE="Ll";
	public final static String LETTER_TITLECASE="Lt";
	public final static String MARK_NONSPACING="Mn";
	public final static String MARK_SPACING_COMBINING="Mc";
	public final static String MARK_ENCLOSING="Me";
	public final static String NUMBER_DECIMAL_DIGIT="Nd";
	public final static String NUMBER_LETTER="Nl";
	public final static String NUMBER_OTHER="No";
	public final static String SEPARATOR_SPACE="Zs";
	public final static String SEPARATOR_LINE="Zl";
	public final static String SEPARATOR_PARAGRAPH="Zp";
	public final static String OTHER_CONTROL="Cc";
	public final static String OTHER_FORMAT="Cf";
	public final static String OTHER_SURROGATE="Cs";
	public final static String OTHER_PRIVATE_USE="Co";
	public final static String OTHER_NOT_ASSIGNED="Cn";
		//informative categories
	public final static String LETTER_MODIFIER="Lm";
	public final static String LETTER_OTHER="Lo";
	public final static String PUNCTUATION_CONNECTOR="Pc";
	public final static String PUNCTUATION_DASH="Pd";
	public final static String PUNCTUATION_OPEN="Ps";
	public final static String PUNCTUATION_CLOSE="Pe";
	public final static String PUNCTUATION_INITIAL_QUOTE="Pi";
	public final static String PUNCTUATION_FINAL="Pf";
	public final static String PUNCTUATION_OTHER="Po";
	public final static String SYMBOL_MATH="Sm";
	public final static String SYMBOL_CURRENCY="Sc";
	public final static String SYMBOL_MODIFIER="Sk";
	public final static String SYMBOL_OTHER="So";

			//bidirectional categories
	public final static String LEFT_TO_RIGHT="L";
	public final static String LEFT_TO_RIGHT_EMBEDDING="LRE";
	public final static String LEFT_TO_RIGHT_OVERRIDE="LRO";
	public final static String RIGHT_TO_LEFT="R";
	public final static String RIGHT_TO_LEFT_ARABIC="AL";
	public final static String RIGHT_TO_LEFT_EMBEDDING="RLE";
	public final static String RIGHT_TO_LEFT_OVERRIDE="RLO";
	public final static String POP_DIRECTIONAL_FORMAT="PDF";
	public final static String EUROPEAN_NUMBER="EN";
	public final static String EUROPEAN_NUMBER_SEPARATOR="ES";
	public final static String EUROPEAN_NUMBER_TERMINATOR="ET";
	public final static String ARABIC_NUMBER="AN";
	public final static String COMMON_NUMBER_SEPARATOR="CS";
	public final static String NON_SPACING_MARK="NSM";
	public final static String BOUNDARY_NEUTRAL="BN";
	public final static String PARAGRAPH_SEPARATOR="B";
	public final static String SEGMENT_SEPARATOR="S";
	public final static String WHITESPACE="WS";
	public final static String OTHER_NEUTRALS="ON";

			//character decomposition mappings
	public final static String FONT="<font>";
	public final static String NO_BREAK="<noBreak>";
	public final static String INITIAL="<initial>";
	public final static String MEDIAL="<medial>";
	public final static String FINAL="<final>";
	public final static String ISOLATED="<isolated>";
	public final static String CIRCLE="<circle>";
	public final static String SUPER="<super>";
	public final static String SUB="<sub>";
	public final static String VERTICLE="<vertical>";
	public final static String WIDE="<wide>";
	public final static String NARROW="<narrow>";
	public final static String SMALL="<small>";
	public final static String SQUARE="<square>";
	public final static String FRACTION="<fraction>";
	public final static String COMPAT="<compat>";
	/**The character which introduces a character decomposition tag.*/
	public final static char CHARACTER_DECOMPOSITION_TAG_BEGIN=FONT.charAt(0);

			//canonical combining classes
	public final static int SPACING_SPLIT_ENCLOSING_REORDRANT_AND_TIBETAN_SUBJOINED=0;
	public final static int OVERLAYS_AND_INTERIOR=1;
	public final static int NUKTAS=7;
	public final static int HIRIGANA_KATAKANA_VOICING_MARKS=8;
	public final static int VIRAMAS=9;
	public final static int START_FIXED_POSITION_CLASS=10;
	public final static int END_FIXED_POSITION_CLASS=199;
	public final static int BELOW_LEFT_ATTACHED=200;
	public final static int BELOW_ATTACHED=202;
	public final static int BELOW_RIGHT_ATTACHED=204;
	public final static int LEFT_ATTACHED=208;
	public final static int RIGHT_ATTACHED=210;
	public final static int ABOVE_LEFT_ATTACHED=212;
	public final static int ABOVE_ATTACHED=214;
	public final static int ABOVE_RIGHT_ATTACHED=216;
	public final static int BELOW_LEFT=218;
	public final static int BELOW=220;
	public final static int BELOW_RIGHT=222;
	public final static int LEFT=224;
	public final static int RIGHT=226;
	public final static int ABOVE_LEFT=228;
	public final static int ABOVE=230;
	public final static int ABOVE_RIGHT=232;
	public final static int DOUBLE_BELOW=233;
	public final static int DOUBLE_ABOVE=234;
	public final static int BELOW_IOTA_SUBSCRIPT=240;
}