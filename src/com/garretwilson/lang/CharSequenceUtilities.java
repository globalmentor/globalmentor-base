package com.garretwilson.lang;

/**Various text manipulating functions. These methods work on
	objects that implement the <code>CharacterSequence</code> interface.
	To avoid creation of new strings, some of these methods should
	be avoided in favor of their corresponding <code>StringBufferUtilities</code>
	methods, which operate on <code>StringBuffer</code> objects.
@see StringBufferUtilities
@author Garret Wilson
*/
public class CharSequenceUtilities extends CharacterUtilities
{

	/**Escapes the indicated characters in the character iterator
		using the supplied escape character.
	Every matching character is converted to its Unicode hex equivalent
		and prefixed with the given escape character.
	<p>As an example, the URI encoding rules in
		<a href="http://www.ietf.org/rfc/rfc2396.txt">RFC 2396</a>,
		"Uniform Resource Identifiers (URI): Generic Syntax" would use
		<code>escapeHex(charSequence, ";/?:@&=+$,", '%', 2);</code>.</p>
	@param charSequence The data to escape.
	@param encodeCharacters The characters that, if they appear, should be escaped.
	@param escapeChar The character to prefix the hex representation.
	@param length The number of characters to use for the hex representation.
	@return A string containing the escaped data.
	*/
	public static String escapeHex(final CharSequence charSequence, final String encodeCharacters, final char escapeChar, final int length)
	{
		final StringBuffer stringBuffer=new StringBuffer();	//create a new string buffer to hold the result
		for(int i=0; i<charSequence.length(); ++i)	//look at each character in the sequence
		{
			final char c=charSequence.charAt(i);	//get a reference to this character
			if(c==escapeChar || encodeCharacters.indexOf(c)>=0)	//if this a character to escape
			{
					//append the escape character, along with a two-digit representation of the character value
				stringBuffer.append(escapeChar).append(IntegerUtilities.toHexString(c, length));
			}
			else	//if this is not a character to escape
			{
				stringBuffer.append(c);	//add this character to the result without escaping it
			}	
		}
		return stringBuffer.toString();	//return the result we constructed
	}

	/**Decodes the escaped characters in the character iterator by
		converting the hex value after each occurrence of the escape
		character to the corresponding Unicode character. 
	<p>For example, to decode a URI according to the URI encoding rules
		in <a href="http://www.ietf.org/rfc/rfc2396.txt">RFC 2396</a>,
		"Uniform Resource Identifiers (URI): Generic Syntax", one would
		use <code>unescapeHex(charSequence, '%', 2)</code>.</p>
	@param charSequence The data to unescape.
	@param escapeChar The character that prefixes the hex representation.
	@param length The number of characters used for the hex representation.
	@return A string containing the unescaped data.
	*/
	public static String unescapeHex(final CharSequence charSequence, final char escapeChar, final int length)
	{
		final StringBuffer stringBuffer=new StringBuffer();	//create a new string buffer to hold the result
		for(int i=0; i<charSequence.length(); ++i)	//look at each character in the sequence
		{
			final char c=charSequence.charAt(i);	//get a reference to this character
				//if this is the beginning of an escaped character, and there's room for enough hex characters after it (the last test is lenient, throwing no exception if the escape character doesn't actually encode anything) 
			if(c==escapeChar && i<charSequence.length()-length)	
			{
				try
				{
						//convert the next two hex characters to a single character value and add it to the string buffer
					stringBuffer.append((char)Integer.parseInt(charSequence.subSequence(i+1, i+length+1).toString(), 16));
					i+=length;	//skip the escape sequence (we'll go to the last character, and we'll be advanced one character when we go back to the start of the loop)
				}
				catch(NumberFormatException numberFormatException)	//if the characters weren't really hex characters
				{
					stringBuffer.append(c);	//we'll assume this wasn't an escape character after all, and add it normally (this is really lenient; this method could be written to throw an exception)
				}
			} 
			else	//if this is not an escaped character
			{
				stringBuffer.append(c);	//add this character to the result with no change
			}	
		}
		return stringBuffer.toString();	//return the result we constructed
	}


}
