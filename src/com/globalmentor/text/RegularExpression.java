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

package com.globalmentor.text;

import static com.globalmentor.text.TextUtilities.*;

/**Constants and utility methods for regular expression-related tasks.
@author Garret Wilson
*/
public class RegularExpression
{

	/**The beginning character of a character class.*/
	public final static char CHARACTER_CLASS_BEGIN='[';

	/**The ending character of a character class.*/
	public final static char CHARACTER_CLASS_END=']';

	/**The restricted characters which must be escaped in regular expressions.*/
	public final static char[] RESTRICTED=new char[]{'*', '-', '(', ')', CHARACTER_CLASS_BEGIN, CHARACTER_CLASS_END};

	/**The character used for escaping regular expressions.*/
	public final static char ESCAPE='\\';

	/**Creates a regular expression character class (e.g. "[abc]") from the given characters (e.g. "abc").
	@param characters The characters to be included in the character class.
	@return The new character class including the given characters.
	@exception NullPointerException if the given characters is <code>null</code>.
	@see #escapePatternString(String)
	*/
	public static String createCharacterClass(final char[] characters)
	{
		return new StringBuilder().append(CHARACTER_CLASS_BEGIN).append(escapePatternString(new String(characters))).append(CHARACTER_CLASS_END).toString();	//escape the characters and surround them with character class characters
	}

	/**Escapes restricted characters meant to appear in a pattern.
	@param patternString The string to apear in a pattern.
	@return The pattern string with restricted characters escaped.
	@see #RESTRICTED
	@see #ESCAPE
	*/
	public static String escapePatternString(final String patternString)
	{
		return escape(patternString, RESTRICTED, ESCAPE);	//escape the string
	}
}
