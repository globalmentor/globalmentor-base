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

package com.globalmentor.text;

import static com.globalmentor.text.Text.*;

import com.globalmentor.java.Characters;

/**
 * Constants and utility methods for regular expression-related tasks.
 * @author Garret Wilson
 */
public class RegularExpressions {

	/** The regular expression character that matches any character. */
	public static final char WILDCARD_CHAR = '.';

	/** The symbol for specifying zero or one repetitions. */
	public static final char ZERO_OR_ONE_CHAR = '?';

	/** The symbol for specifying zero or more repetitions. */
	public static final char ZERO_OR_MORE_CHAR = '*';

	/** The symbol for specifying one or more repetitions. */
	public static final char ONE_OR_MORE_CHAR = '+';

	/** The beginning character of a character class. */
	public static final char CHARACTER_CLASS_BEGIN = '[';

	/** The ending character of a character class. */
	public static final char CHARACTER_CLASS_END = ']';

	/** The restricted characters which must be escaped in regular expressions. */
	public static final Characters RESTRICTED = Characters.of(ZERO_OR_ONE_CHAR, ZERO_OR_MORE_CHAR, ONE_OR_MORE_CHAR, WILDCARD_CHAR, '-', '(', ')',
			CHARACTER_CLASS_BEGIN, CHARACTER_CLASS_END);

	/** The character used for escaping regular expressions. */
	public static final char ESCAPE = '\\';

	/**
	 * Creates a regular expression character class (e.g. "[abc]") from the given characters (e.g. 'a', 'b', and 'c').
	 * @param characters The characters to be included in the character class.
	 * @return The new character class including the given characters.
	 * @throws NullPointerException if the given characters is <code>null</code>.
	 * @see #escapePatternString(String)
	 */
	public static String createCharacterClass(final char... characters) {
		return createCharacterClass(Characters.of(characters));
	}

	/**
	 * Creates a regular expression character class (e.g. "[abc]") from the given characters (e.g. 'a', 'b', and 'c').
	 * @param characters The characters to be included in the character class.
	 * @return The new character class including the given characters.
	 * @throws NullPointerException if the given characters is <code>null</code>.
	 * @see #escapePatternString(String)
	 */
	public static String createCharacterClass(final Characters characters) {
		return new StringBuilder().append(CHARACTER_CLASS_BEGIN).append(escapePatternString(characters.toString())).append(CHARACTER_CLASS_END).toString(); //escape the characters and surround them with character class characters
	}

	/**
	 * Escapes restricted characters meant to appear in a pattern.
	 * @param patternString The string to appear in a pattern.
	 * @return The pattern string with restricted characters escaped.
	 * @see #RESTRICTED
	 * @see #ESCAPE
	 */
	public static String escapePatternString(final String patternString) {
		return escape(patternString, RESTRICTED, ESCAPE); //escape the string
	}
}
