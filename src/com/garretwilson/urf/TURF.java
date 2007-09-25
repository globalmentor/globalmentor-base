package com.garretwilson.urf;

import static com.garretwilson.text.CharacterConstants.*;

/**Constants relating to the text serialization of URF, TURF.
@author Garret Wilson
*/
public class TURF
{

	/**The character that marks the end of a series of resources encoded in TURF.*/
//TODO del if not wanted	public final static char TURF_END=';';

	/**Unicode whitespace characters.*/
	public final static char[] SEPARATORS=(PARAGRAPH_SEPARATOR_CHARS+SEGMENT_SEPARATOR_CHARS+WHITESPACE_CHARS).toCharArray();

	/**The delimiters used to a name prefix.*/
	public final static char NAME_PREFIX_DELIMITER=':';
	
	/**The delimiter that begins arrays.*/
	public final static char ARRAY_BEGIN='[';
	/**The delimiter that ends arrays.*/
	public final static char ARRAY_END=']';

	/**The delimiter that begins boolean shorthand declarations.*/
	public final static char BOOLEAN_BEGIN='!';
	/**The delimiter that ends boolean shorthand declarations.*/
	public final static char BOOLEAN_END='!';

	/**The delimiter that begins character shorthand declarations.*/
	public final static char CHARACTER_BEGIN='\'';
	/**The delimiter that ends character shorthand declarations.*/
	public final static char CHARACTER_END=CHARACTER_BEGIN;

	/**The delimiter that begins comments.*/
	public final static char COMMENT_BEGIN='*';
	/**The delimiter that ends comments.*/
	public final static char COMMENT_END=COMMENT_BEGIN;

	/**The delimiter that begins labels.*/
	public final static char LABEL_BEGIN='|';
	/**The delimiter that ends labels.*/
	public final static char LABEL_END=LABEL_BEGIN;

	/**The delimiter that begins number shorthand declarations.*/
	public final static char NUMBER_BEGIN='#';
	/**The delimiter that ends URI shorthand declarations.*/
	public final static char NUMBER_END=NUMBER_BEGIN;

	/**The delimiter that begins property declarations.*/
	public final static char PROPERTIES_BEGIN='{';
	/**The delimiter that ends property declarations.*/
	public final static char PROPERTIES_END='}';

	/**The delimiter that begins URI references.*/
	public final static char REFERENCE_BEGIN='«';
	/**The delimiter that ends URI references.*/
	public final static char REFERENCE_END='»';

	/**The delimiter that begins sequences.*/
	public final static char SEQUENCE_BEGIN='\\';
	/**The delimiter that ends sequences.*/
	public final static char SEQUENCE_END=SEQUENCE_BEGIN;

	/**The delimiter that begins string shorthand declarations.*/
	public final static char STRING_BEGIN='"';
	/**The delimiter that ends string shorthand declarations.*/
	public final static char STRING_END=STRING_BEGIN;
		/**The character used for escaping characters in a string.*/
		public final static char STRING_ESCAPE='\\';
			//escaped forms of characters
		public final static char ESCAPED_BACKSPACE='b';	//b backspace
		public final static char ESCAPED_FORM_FEED='f';	//f form feed
		public final static char ESCAPED_LINE_FEED='n';	//n line feed
		public final static char ESCAPED_CARRIAGE_RETURN='r';	//r carriage return
		public final static char ESCAPED_TAB='t';	//t tab	
		public final static char ESCAPED_UNICODE='u';	//u Unicode	
		
	/**The delimiter that begins type declarations.*/
	public final static char TYPE_BEGIN='(';
	/**The delimiter that ends type declarations.*/
	public final static char TYPE_END=')';

	/**The delimiter that begins URI shorthand declarations.*/
	public final static char URI_BEGIN='<';
	/**The delimiter that ends URI shorthand declarations.*/
	public final static char URI_END='>';

	/**The character that separates items in a list.*/
	public final static char LIST_DELIMITER=',';

	/**The character that separates properties and assigned values.*/
	public final static char PROPERTY_VALUE_DELIMITER='=';

	/**The character that separates scoped properties and values.*/
	public final static char SCOPED_PROPERTY_VALUE_DELIMITER='~';

	/**The characters that can separate properties from values.*/
	public final static char[] PROPERTY_VALUE_DELIMITERS=new char[]{PROPERTY_VALUE_DELIMITER, SCOPED_PROPERTY_VALUE_DELIMITER};

	/**Characters which which a URI resource can begin.*/
	public final static char[] URI_RESOURCE_BEGINS=new char[]{URI_BEGIN, LABEL_BEGIN, REFERENCE_BEGIN};

	/**The beginning delimiter of the lexical form of the Boolean value <code>false</code>.*/
	public final static char BOOLEAN_FALSE_BEGIN='f';
	/**The beginning delimiter of the lexical form of the Boolean value <code>true</code>.*/
	public final static char BOOLEAN_TRUE_BEGIN='t';

	/**Determines if the given character is a TURF name character.
	A name character is a Unicode letter, number, or underscore.
	@param c The character to check.
	@return <code>true</code> if the character is a TURF name character.
	*/
	public final static boolean isNameCharacter(final int c)
	{
		return Character.isLetterOrDigit(c) || c=='_' || c=='.';	//see if this is a letter, digit, an underscore, or a period TODO use constants
	}

}
