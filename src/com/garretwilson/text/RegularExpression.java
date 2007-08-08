package com.garretwilson.text;

import static com.garretwilson.lang.ObjectUtilities.*;

/**Constants and utility methods for regular expression-related tasks.
@author Garret Wilson
*/
public class RegularExpression
{

	/**The beginning character of a character class.*/
	public final static char CHARACTER_CLASS_BEGIN_CHAR='[';

	/**The ending character of a character class.*/
	public final static char CHARACTER_CLASS_END_CHAR=']';

	/**Creates a regular expression character class (e.g. "[abc]") from the given characters (e.g. "abc").
	@param characters The characters to be included in the character class.
	@return The new character class including the given characters.
	@exception NullPointerException if the given characters is <code>null</code>.
	*/
	public static String createCharacterClass(final String characters)	//TODO fix to escape characters as needed
	{
		return new StringBuilder().append(CHARACTER_CLASS_BEGIN_CHAR).append(checkInstance(characters, "Characters cannot be null.")).append(CHARACTER_CLASS_END_CHAR).toString();
	}

}
