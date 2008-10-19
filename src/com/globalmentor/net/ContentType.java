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

package com.globalmentor.net;

import java.net.URLConnection;

import javax.activation.*;

import static com.globalmentor.java.Characters.SPACE_CHAR;
import com.globalmentor.java.Objects;
import com.globalmentor.text.ArgumentSyntaxException;
import com.globalmentor.util.NameValuePair;

/**An encapsulation of an Internet media content type as originally defined in <a href="http://www.rfc-editor.org/rfc/rfc2046.txt">RFC 2046</a>, "MIME Part 2: Media Types".
<p>Neither {@link MimeType} nor <code>javax.mail.internet.ContentType</code> correctly implements {@link Object#equals(Object)} and therefore cannot reliably be used in sets and maps.
Furthermore, <code>javax.mail.internet.ContentType</code> as of JDK 6 is not included in default JDK distributions.
However, {@link MimeType} has recently been added to JDK distributions, so this class is provided, with appropriate factory methods,
to provide special content type instances that provide correct equality checking.</p> 
@author Garret Wilson
@see <a href="http://www.rfc-editor.org/rfc/rfc2046.txt">RFC 2046</a>
@see <a href="http://www.w3.org/TR/xhtml-media-types/">XHTML Media Types</a>
*/
public class ContentType extends MimeType
{

	/**The divider character for media type strings.*/
	public final static char TYPE_DIVIDER='/';
	/**The delimiter character separating parameters from the base content type and from each other.*/
	public final static char PARAMETER_DELIMITER_CHAR=';';
	/**The character used to assign parameter values.*/
	public final static char PARAMETER_ASSIGNMENT_CHAR='=';
	/**The wildcard character.*/
	public final static char TYPE_WILDCARD_CHAR='*';
	/**The wildcard subtype, matching any subtype.*/
	public final static String WILDCARD_SUBTYPE=String.valueOf(TYPE_WILDCARD_CHAR);

		//discrete top-level media types
	public final static String TEXT_PRIMARY_TYPE="text";
	public final static String IMAGE_PRIMARY_TYPE="image";
	public final static String AUDIO_PRIMARY_TYPE="audio";
	public final static String VIDEO_PRIMARY_TYPE="video";
	public final static String APPLICATION_PRIMARY_TYPE="application";
		//composite top-level media types
	public final static String MULTIPART_PRIMARY_TYPE="multipart";
	public final static String MESSAGE_PRIMARY_TYPE="message";
	/**The pseudo top-level type used by Java {@link URLConnection} to indicate unknown content by <code>content/unknown</code>.*/
	public final static String CONTENT_PRIMARY_TYPE="content";

	/**The separator character that begins a non-standard extension type.*/
	public final static String SUBTYPE_EXTENSION_PREFIX="x-";
	/**The separator character that delimits a subtype suffix.*/
	public final static char SUBTYPE_SUFFIX_DELIMITER_CHAR='+';

		//parameters
	/**The character set parameters.*/
	public final static String CHARSET_PARAMETER="charset";

		//content media types
	/**The pseudo subtype used by Java {@link URLConnection} to indicate unknown content by <code>content/unknown</code>.*/
	public final static String UNKNOWN_SUBTYPE="unknown";

		//application media types
	/**A stream of bytes.*/
	public final static String OCTET_STREAM_SUBTYPE="octet-stream";

	/**A Java object.*/
	public final static String X_JAVA_OBJECT=SUBTYPE_EXTENSION_PREFIX+"java-object";
	
	/**The shared <code>application/octet-stream</code> content type.*/
	public final static ContentType APPLICATION_OCTET_STREAM_CONTENT_TYPE=getInstance(APPLICATION_PRIMARY_TYPE, OCTET_STREAM_SUBTYPE);

	/**Content type string constructor.
	@param contentTypeString The lexical form of the content type.
	@throws MimeTypeParseException If the given content type string does not represent a valid content type.
	*/
	private ContentType(final String contentTypeString) throws MimeTypeParseException
	{
		super(contentTypeString);	//construct the parent class
	}

	/**Primary type and subtype constructor.
	@param primaryType The primary type with which to compare the content type.
	@param subType The subtype with which to compare the content type.
	@param parameters The content type parameters.
	@throws NullPointerException if the given parameters is <code>null</code>.
	@throws MimeTypeParseException If the given content type string does not represent a valid content type.
	*/
	private ContentType(final String primaryType, final String subType, final NameValuePair<String, String>... parameters) throws MimeTypeParseException 
	{
		super(primaryType, subType);	//construct the parent class
		for(final NameValuePair<String, String> parameter:parameters)	//look at each given parameter
		{
			setParameter(parameter.getName(), parameter.getValue());	//set the given parameter
		}
	}

	/**Retrieves a content type object from a string.
	Any parsing errors will be wrapped in a <code>IllegalArgumentException</code>.
	@param string The string representation of the content type.
	@return A new content type object parsed from the string.
	@exception ArgumentSyntaxException Thrown if the string is not a syntactically correct content type.
	*/
	public static ContentType getInstance(final String string) throws ArgumentSyntaxException
	{
		try
		{
			return new ContentType(string);	//create a new content type from the string
		}
		catch(final MimeTypeParseException parseException)	//if the string has a syntax error
		{
			throw new ArgumentSyntaxException(parseException);	//create a new illegal argument exception from the parsing error
		}
	}

	/**Creates a content type object from primary type, a subtype, and optional parameters.
	@param primaryType The primary type.
	@param subType The subtype.
	@param parameters Optional name-value pairs representing parameters of the content type.
	@return A new content type object constructed from the given information.
	*/
	public static ContentType getInstance(final String primaryType, final String subType, final NameValuePair<String, String>... parameters)
	{
		try
		{
			return new ContentType(primaryType, subType, parameters);	//create a new content type from the given values
		}
		catch(final MimeTypeParseException parseException)	//if the string has a syntax error
		{
			throw new ArgumentSyntaxException(parseException);	//create a new illegal argument exception from the parsing error
		}
	}

	/**Matches a content type against a primary type and subtype.
	This method supports wildcard subtypes.
	@param primaryType The primary type with which to compare the content type.
	@param subType The subtype with which to compare the content type.
	@return <code>true</code> if the content type has the same primary type and subtype as that given.
	*/
	public boolean match(final String primaryType, final String subType)
	{
		final String contentTypeSubType=getSubType();	//get the content type's subtype
		return getPrimaryType().equals(primaryType) && (contentTypeSubType.equals(subType) || WILDCARD_SUBTYPE.equals(contentTypeSubType) || WILDCARD_SUBTYPE.equals(subType));	//check the primary type and subtype and wildcards
	}

	/**Matches a content type against a primary type and subtype, with a class parameter indicating the given object class.
	This method supports wildcard subtypes.
	@param primaryType The primary type with which to compare the content type.
	@param subType The subtype with which to compare the content type.
	@param objectClass The class for which to check in the parameters under the key "class".
	@return <code>true</code> if the content type has the same primary type and subtype as that given, along with a class parameter.
	*/
	public boolean match(final String primaryType, final String subType, final Class<?> objectClass)
	{
		return match(primaryType, subType) && objectClass.getName().equals(getParameter("string"));	//see if the primary type and subtype match, and that "class" parameter indicates this class TODO use a constant
	}

	/**Matches a content type against the {@value #APPLICATION_PRIMARY_TYPE} primary type and {@value ContentType#X_JAVA_OBJECT} subtype, with a "class" parameter indicating the given object class.
	This method supports wildcard subtypes.
	@param objectClass The class for which to check in the parameters under the key "class".
	@return <code>true</code> if the content type has the same primary type and subtype as that given, along with a class parameter.
	*/
	public boolean match(final Class<?> objectClass)
	{
		return match(APPLICATION_PRIMARY_TYPE, X_JAVA_OBJECT);	//check for application/x-java-object and class name
	}

	/**Determines the base content type, with no parameters, of the content type.
	Useful for making comparisons or for storing in canonical form in a hash table.
	If this content type is already a base content type, this content type is returned.
	@return A content type with the same primary and subtype as the content type, with <code>null</code> parameters.
	*/
	public ContentType getBaseContentType()
	{
		return getParameters().size()!=0 ? getInstance(getPrimaryType(), getSubType()) : this;	//if this content type is already just the base type, return that
	}

	/**Matches a content type against a primary type and subtype with no wildcard support.
	@param primaryType The primary type with which to compare the content type.
	@param subType The subtype with which to compare the content type.
	@return <code>true</code> if the content type has the same primary type and subtype as that given.
	@exception NullPointerException if the primar type and/or subtype is <code>null</code>.
	*/
	public boolean hasBaseType(final String primaryType, final String subType)
	{
		return primaryType.equals(getPrimaryType()) && subType.equals(getSubType());	//check the primary type and subtype
	}

	/**Determines if the subtype of the content type has the given suffix.
	@param suffixes The suffix strings that will be checked, after they are combined into a single suffix, each part prepended with '+'.
	@return <code>true</code> if the content type's subtype has the given suffixes.
	*/
	public boolean hasSubTypeSuffix(final String... suffixes)
	{
		return getSubType().endsWith(createSubTypeSuffix(suffixes));	//see if the content type subtype ends with the given suffixes
	}

	/**Returns a hash code value for the object.
	If this content type has no parameters, this implementation returns the hash code of the primary type and subtype;
	otherwise, this implementation returns the hash code of the string version of the content type. 
	@return A hash code value for this object.
	@see #getPrimaryType()
	@see #getSubType()
	@see #getParameters()
	@see #toString()
	*/
	public int hashCode()
	{
		final MimeTypeParameterList parameterList=getParameters();	//get our parameter list
		return parameterList==null || parameterList.size()==0 ? Objects.hashCode(getPrimaryType(), getSubType()) : toString().hashCode();	//if there are parameters (probary a minority of cases), use the expensive toString() operation  
	}

	/**Indicates whether some other object is "equal to" this one.
	If the other object is a content type has no parameters, this implementation compares the primary types and subtypes;
	otherwise, this implementation compares the string versions of the two content types if the other object is a content type.
	@param  object The reference object with which to compare.
	@return <code>true</code> if this object is equal to the argument; <code>false</code> otherwise.
	@see #getPrimaryType()
	@see #getSubType()
	@see #getParameters()
	@see #toString()
	*/
	public boolean equals(final Object object)
	{
		if(this==object)	//if the objects are the same identical object
		{
			return true;	//identical objects are always equal
		}
		if(object instanceof MimeType)	//if the other object is a MIME type
		{
			final MimeType mimeType=(MimeType)object;	//get the other object as a MIME type
			if(getPrimaryType().equals(mimeType.getPrimaryType()) && getSubType().equals(mimeType.getSubType()))	//see if the primary and subtypes are equal
			{
				final MimeTypeParameterList parameterList=getParameters();	//get our parameter list
				final int parameterCount=parameterList!=null ? parameterList.size() : 0;	//get the number of parameters
				final MimeTypeParameterList mimeTypeParameterList=mimeType.getParameters();	//get the other MIME type's parameter list
				final int mimeTypeParameterCount=mimeTypeParameterList!=null ? mimeTypeParameterList.size() : 0;	//get the number of the other MIME type's parameters
				if(parameterCount==mimeTypeParameterCount)	//if both content types have the same number of parameters
				{
					if(parameterCount==0)	//if both have no parameters (probably the most common case)
					{
						return true;	//base types with no parameters are equal
					}
					else	//if both have the same number of parameters
					{
						return toString().equals(mimeType.toString());	//this is an uncommon case; do the inefficient but easy check of comparing lexicon forms
					}
				}
			}
		}
		return false;	//for some reason the content types were not equal
	}

	/**Constructs a string representing a content type.
	@param primaryType The primary type.
	@param subType The subtype.
	@param parameters Optional name-value pairs representing parameters of the content type.
	@return A string representing the type in the form "<var>primaryType</var>/<var>subType</var>[; <var>parameters</var>]".
	*/
	public static String toString(final String primaryType, final String subType, final NameValuePair<String, String>... parameters)	//TODO determine if any of the parameters need quoted
	{
		final StringBuilder stringBuilder=new StringBuilder();
		stringBuilder.append(primaryType).append(TYPE_DIVIDER).append(subType);	//primaryType/subType
		if(parameters.length>0)	//if parameters were given
		{
			for(final NameValuePair<String, String> parameter:parameters)	//for each parameter
			{
					//append "; name=value"
				stringBuilder.append(PARAMETER_DELIMITER_CHAR).append(SPACE_CHAR).append(parameter.getName()).append(PARAMETER_ASSIGNMENT_CHAR).append(parameter.getValue());
			}
		}
		return primaryType+TYPE_DIVIDER+subType;	//separate the types with a media type divider character		
	}

	/**Creates a content type suffix by prepending '+' to each suffix and concatenating the suffixes.
	@param suffixes The suffix strings to combine into a suffix.
	@return A suffix composed of the given suffix strings.
	*/
	public static String createSubTypeSuffix(final String... suffixes)
	{
		final StringBuilder stringBuilder=new StringBuilder();
		for(final String suffix:suffixes)	//for each suffix
		{
			stringBuilder.append(SUBTYPE_SUFFIX_DELIMITER_CHAR).append(suffix);	//+suffix
		}
		return stringBuilder.toString();	//return the suffix we constructed
	}
}
