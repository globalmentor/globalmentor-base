package com.garretwilson.text;

import static com.garretwilson.text.TextUtilities.*;

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
	*/
	public static String createCharacterClass(final char[] characters)
	{
		return new StringBuilder().append(CHARACTER_CLASS_BEGIN).append(escape(new String(characters), RESTRICTED, ESCAPE)).append(CHARACTER_CLASS_END).toString();	//escape the characters and surround them with character class characters
	}

}
