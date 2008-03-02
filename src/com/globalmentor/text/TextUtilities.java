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

import java.io.UnsupportedEncodingException;

import javax.mail.internet.ContentType;

import com.globalmentor.text.xml.XMLUtilities;
import com.globalmentor.util.Arrays;

import static com.globalmentor.io.ContentTypes.*;
import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Characters.*;
import static com.globalmentor.java.Objects.*;
import static com.globalmentor.text.xml.XMLUtilities.*;

/**Constants and utilities for text.
@author Garret Wilson
*/
public class TextUtilities
{

	/**The MIME subtype of <code>text/plain</code>.*/
	public final static String PLAIN_SUBTYPE="plain";

	/**The content type for plain text: <code>text/plain</code>.*/
	public static final ContentType TEXT_PLAIN_CONTENT_TYPE=new ContentType(TEXT_PRIMARY_TYPE, PLAIN_SUBTYPE, null);

	/**Creates a control string according to ECMA-48, "Control Functions for Coded Character Sets", Section 5.6, "Control strings".
	A control string begins with the Start of String control character (U+0098) and ends with a String Terminator control character (U+009C).
	ECMA-48 publication is also approved as ISO/IEC 6429.
	@param string The string from which a control string will be created.
	@return An ECMA-48 control string with the given string as its content.
	@exception NullPointerException if the given string is <code>null</code>.
	@see <a href="http://www.ecma-international.org/publications/standards/Ecma-048.htm">ECMA-48: Control Functions for Coded Character Sets</a>
	*/
	public final static String createControlString(final String string)
	{
		return START_OF_STRING_CHAR+checkInstance(string, "String cannot be null.")+STRING_TERMINATOR_CHAR;	//wrap the string with a SOS/ST pair
	}

	/**Determines if the given content type is one representing text in some form.
	<p>Text media types include:</p>
	<ul>
		<li><code>text/*</code></li>
		<li><code>application/xml</code></li>
		<li><code>application/*+xml</code></li>
		<li><code>application/xml-external-parsed-entity</code></li>
		<li><code>application/*+xml-external-parsed-entity</code> (not formally defined)</li>
	</ul>
	@param contentType The content type of a resource, or <code>null</code> for no
		content type.
	@return <code>true</code> if the given content type is one of several text
		media types.
	@see XMLUtilities#isXML(ContentType)
	@see XMLUtilities#isXMLExternalParsedEntity(ContentType)
	*/ 
	public static boolean isText(final ContentType contentType)
	{
		if(contentType!=null)	//if a content type is given
		{
			if(TEXT_PRIMARY_TYPE.equals(contentType.getPrimaryType()))	//if this is "text/*"
			{
				return true;	//text/* is a text content type
			}
			return isXML(contentType) || isXMLExternalParsedEntity(contentType);	//return whether this is an XML document or external parsed entity content type; all XML content types are text content types
		}
		return false;	//this is not a media type we recognize as being HTML
	}

	/**Re-encodes the given string to the new encoding (such as UTF-8), assuming
		the string was encoded from an array of bytes using the old encoding
		(e.g. ISO-8859-1).
	@param string The string to recode.
	@param oldEncoding The encoding used to create the string originally.
	@param newEncoding The new encoding to use when creating the string.
	@return The a string created from encoding the characters in the specified
		new encoding.
	@exception UnsupportedEncodingException Thrown if either the old encoding or
		the new encoding is not supported.
	*/
	public static String recode(final String string, final String oldEncoding, final String newEncoding) throws UnsupportedEncodingException
	{
		final byte[] bytes=string.getBytes(oldEncoding);  //get the bytes of the string as they were before they were encoded
		return new String(bytes, newEncoding);  //create a string from the bytes using the new encoding
	}

	/**Escapes a given string by inserting an escape character before every restricted character, including any occurrence of the given escape character.
	@param charSequence The data to escape.
	@param restricted The characters to be escaped; should not include the escape character.
	@param escape The character used to escape the restricted characters.
	@return A string containing the escaped data.
	@exception NullPointerException if the given character sequence is <code>null</code>.
	*/
	public static String escape(final CharSequence charSequence, final char[] restricted, final char escape)
	{
		if(!contains(charSequence, restricted))	//if there are no restricted characters in the string (assuming that most strings won't need to be escaped, it's less expensive to check up-front before we start allocating and copying)
		{
			return charSequence.toString();	//the string doesn't need to be escaped
		}
		final StringBuilder stringBuilder=new StringBuilder(charSequence);	//put the string in a string builder so that we can work with it; although inserting encoded sequences may seem inefficient, it should be noted that filling a string buffer with the entire string is more efficient than doing it one character at a time, that characters needed encoding are generally uncommon, and that any copying of the string characters during insertion is done via a native method, which should happen very quickly
		for(int characterIndex=stringBuilder.length()-1; characterIndex>=0; --characterIndex)	//work backwords; this keeps us from having a separate variable for the length, but it also makes it simpler to calculate the next position when we swap out characters
		{
			final char c=stringBuilder.charAt(characterIndex);	//get the current character
			if(c==escape || Arrays.contains(restricted, c))	//if we should encode this character (always encode the escape character)
			{
				stringBuilder.insert(characterIndex, escape);	//insert the escape character
			}
		}
		return stringBuilder.toString();	//return the encoded version of the string
	}

}