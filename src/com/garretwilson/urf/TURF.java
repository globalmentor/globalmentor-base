package com.garretwilson.urf;

import java.net.URI;
import java.util.*;

import static com.garretwilson.text.CharacterConstants.*;

/**Constants relating to the text serialization of URF, TURF.
This class also provides state regarding the processing of an URF instance in TURF,
in particular associating string prefixes to namespace URIs.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class TURF extends HashMap<String, URI>
{

	/**Unicode whitespace characters.*/
	public final static char[] SEPARATORS=(PARAGRAPH_SEPARATOR_CHARS+SEGMENT_SEPARATOR_CHARS+WHITESPACE_CHARS).toCharArray();

	/**The delimiters used to a name prefix.*/
	public final static char NAME_PREFIX_DELIMITER='.';

	/**The delimiter that begins binary shorthand declarations.*/
	public final static char BINARY_BEGIN='%';
	/**The delimiter that ends binary shorthand declarations.*/
	public final static char BINARY_END=BINARY_BEGIN;

	/**The delimiter that begins boolean shorthand declarations.*/
	public final static char BOOLEAN_BEGIN='*';

	/**The delimiter that begins character shorthand declarations.*/
	public final static char CHARACTER_BEGIN='\'';
	/**The delimiter that ends character shorthand declarations.*/
	public final static char CHARACTER_END=CHARACTER_BEGIN;

	/**The delimiter that begins comments.*/
	public final static char COMMENT_BEGIN='\u2020';	//dagger
	/**The delimiter that ends comments.*/
	public final static char COMMENT_END='\u2021';	//double dagger

	/**The delimiter that begins community short forms.*/
	public final static char COMMUNITY_BEGIN='\u00A4';	//currency symbol
	/**The delimiter that ends community short forms.*/
	public final static char COMMUNITY_END='.';

	/**The delimiter that begins labels.*/
	public final static char LABEL_BEGIN='|';
	/**The delimiter that ends labels.*/
	public final static char LABEL_END=LABEL_BEGIN;

	/**The delimiter that begins lists.*/
	public final static char LIST_BEGIN='[';
	/**The delimiter that ends lists.*/
	public final static char LIST_END=']';

	/**The delimiter that begins number shorthand declarations.*/
	public final static char NUMBER_BEGIN='#';

	/**The delimiter that begins init declarations.*/
	public final static char INITS_BEGIN='\u00A1';	//¡
	/**The delimiter that ends init declarations.*/
	public final static char INITS_END='!';

	/**The delimiter that begins ordinal shorthand declarations.*/
	public final static char ORDINAL_BEGIN='\u00BA';	//º

	/**The delimiter that begins property declarations.*/
	public final static char PROPERTIES_BEGIN=':';
	/**The delimiter that ends property declarations.*/
	public final static char PROPERTIES_END=';';

	/**The delimiter that begins proposition short forms.*/
	public final static char PROPOSITION_BEGIN=LEFT_DOUBLE_QUOTATION_MARK_CHAR;
	/**The delimiter that ends proposition short forms.*/
	public final static char PROPOSITION_END=RIGHT_DOUBLE_QUOTATION_MARK_CHAR;

	/**The delimiter that begins regular expressions.*/
	public final static char REGULAR_EXPRESSION_BEGIN='/';
	/**The delimiter that ends regular expressions.*/
	public final static char REGULAR_EXPRESSION_END=REGULAR_EXPRESSION_BEGIN;

	/**The delimiter that begins URI references.*/
	public final static char REFERENCE_BEGIN='\u00AB';	//«
	/**The delimiter that ends URI references.*/
	public final static char REFERENCE_END='\u00BB';	//»

	/**The delimiter that begins sets.*/
	public final static char SET_BEGIN='{';
	/**The delimiter that ends sets.*/
	public final static char SET_END='}';

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
		public final static char ESCAPED_START_OF_STRING=LEFT_DOUBLE_QUOTATION_MARK_CHAR;	//left double quotation mark start of string
		public final static char ESCAPED_STRING_TERMINATOR=RIGHT_DOUBLE_QUOTATION_MARK_CHAR;	//right double quotation mark string terminator
		public final static char ESCAPED_UNICODE='u';	//u Unicode

	/**The delimiter that begins temporal declarations.*/
	public final static char TEMPORAL_BEGIN='@';

	/**The delimiter that begins type declarations.*/
	public final static char TYPES_BEGIN='(';
	/**The delimiter that ends type declarations.*/
	public final static char TYPES_END=')';

	/**The delimiter that begins URI shorthand declarations.*/
	public final static char URI_BEGIN='<';
	/**The delimiter that ends URI shorthand declarations.*/
	public final static char URI_END='>';

	/**The character that separates items in a list.*/
	public final static char LIST_DELIMITER=',';

	/**The character that indicates the beginning of a new scope.*/
	public final static char SCOPE_DELIMITER='`';

	/**The character that separates properties and assigned values.*/
	public final static char PROPERTY_VALUE_DELIMITER='=';

	/**Characters which which a URI resource can begin.*/
	public final static char[] URI_RESOURCE_BEGINS=new char[]{URI_BEGIN, LABEL_BEGIN, REFERENCE_BEGIN};

	/**The beginning delimiter of the lexical form of the Boolean value <code>false</code>.*/
	public final static char BOOLEAN_FALSE_BEGIN='f';
	/**The beginning delimiter of the lexical form of the Boolean value <code>true</code>.*/
	public final static char BOOLEAN_TRUE_BEGIN='t';

	/**The "magic number" marker indicating the beginning of TURF content.*/
	public final static String TURF_SIGNATURE="`URF";

	/**The delimiter that begins the TURF preamble.*/
	public final static char PREAMBLE_BEGIN='$';
	/**The delimiter that ends the TURF preamble.*/
	public final static char PREAMBLE_END=PREAMBLE_BEGIN;

	/**Determines if the given character is a TURF name begin character.
	A name begin character is a Unicode letter.
	@param c The character to check.
	@return <code>true</code> if the character is a TURF name begin character.
	*/
	public final static boolean isNameBeginCharacter(final int c)
	{
		return Character.isLetter(c);	//see if this is a letter
	}

	/**Determines if the given character is a TURF name character.
	A name character is a Unicode letter, number, or underscore.
	@param c The character to check.
	@return <code>true</code> if the character is a TURF name character.
	*/
	public final static boolean isNameCharacter(final int c)
	{
		return Character.isLetterOrDigit(c) || Character.getType(c)==Character.CONNECTOR_PUNCTUATION;	//see if this is a letter, digit, or a connector
	}

	/**Determines if the given string is a valid TURF name.
	@param string The string to check.
	@return <code>true</code> if the string is a valid TURF name.
	@exception NullPointerException if the given string is <code>null</code>.
	@see #isNameBeginCharacter(int)
	@see #isNameCharacter(int)
	*/
	public final static boolean isName(final String string)
	{
		final int length=string.length();	//get the length of the string
		if(length<1 || !isNameBeginCharacter(string.codePointAt(0)))	//if the string is empty or it doesn't start with a name beginning character
		{
			return false;	//empty strings and string starting with non-name-begin characters are not valid names
		}
		for(int i=1; i<length; ++i)	//for each character, skipping the first because we already checked it
		{
			if(!isNameCharacter(string.charAt(i)))	//if this is not a name character
			{
				return false;	//this is not a name
			}
		}
		return true;	//this string passed all the tests
	}
}
