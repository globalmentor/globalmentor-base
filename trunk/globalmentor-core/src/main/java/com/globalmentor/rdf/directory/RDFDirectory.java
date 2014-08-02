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

package com.globalmentor.rdf.directory;

import java.net.URI;
import java.util.*;

import static com.globalmentor.java.Java.*;
import static com.globalmentor.java.Objects.*;
import static com.globalmentor.model.Locales.*;
import static com.globalmentor.rdf.RDFResources.*;
import static com.globalmentor.text.directory.Directory.*;

import com.globalmentor.model.LocaledText;
import com.globalmentor.rdf.*;

/**Constants used in RDF processing of a directory of type
	<code>text/directory</code> as defined in 
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
@author Garret Wilson
@deprecated
*/
public class RDFDirectory
{

	/**The recommended prefix to the directory namespace.*/
	public final static String DIRECTORY_NAMESPACE_PREFIX="directory";

	/**The URI to the directory namespace.*/
	public final static URI DIRECTORY_NAMESPACE_URI=URI.create("http://globalmentor.com/namespaces/directory#");

		//directory property names
	/**Identifies data in multiple languages according to <a href="http://www.ietf.org/rfc/rfc1766.txt">RFC 1766</a> (now <a href="http://www.ietf.org/rfc/rfc3066.txt">RFC 3066</a>). The local name of <code>directory:language</code>.*/
	public final static String LANGUAGE_PROPERTY_NAME=getVariableName(LANGUAGE_PARAM_NAME);

	/**Retrieves vCard text from a given RDF object.
	If the object is an {@link RDFLiteral}, the lexical form of the literal will be returned.
	If the object is an {@link RDFResource}, the {@link RDFLiteral} <code>rdf:value</code> of each resource, if present, will be used for the text,
	and any <code>directory:language</code> property of resource will be used to determine the language of the text.
	In either case, a language designation by <code>xml:lang</code> for the actual literal text will take precedence, as this value is specified most closely to the text.
	@param rdfObject The object that is supposed to represent text.
	@return An object representing the text and any language specification, or <code>null</code> if the object does not represent text.
	@throws NullPointerException if the given RDF object is <code>null</code>.
	*/
	public static LocaledText getText(final RDFObject rdfObject)
	{
		checkInstance(rdfObject, "RDF object cannot be null.");
		if(rdfObject instanceof RDFLiteral)	//if the object is a literal
		{
			final Locale language=rdfObject instanceof RDFPlainLiteral ? ((RDFPlainLiteral)rdfObject).getLanguage() : null;	//save the language if this is a plain literal
			return new LocaledText(((RDFLiteral)rdfObject).getLexicalForm(), language);	//use the lexical form as text
		}
		else if(rdfObject instanceof RDFResource)	//if the object is a resource
		{
			//TODO add RDFResource language processing
			final RDFResource rdfResource=(RDFResource)rdfObject;	//get the object as a resource
			final RDFObject value=getValue(rdfResource);	//get the value of the resource
			if(value instanceof RDFLiteral)	//if there is a literal value
			{
				final Locale language=rdfObject instanceof RDFPlainLiteral ? ((RDFPlainLiteral)rdfObject).getLanguage() : null;	//save the language if this is a plain literal
				return new LocaledText(((RDFLiteral)value).getLexicalForm(), language!=null ? language : getLanguage(rdfResource));	//use the lexical form of the value as the text; if no xml:lang was specified, see if the main resource had directory:language defined
			}
		}
		else	//if the object is not a literal or a resource
		{
			throw new AssertionError("Unrecognized RDF object type: "+rdfObject.getClass());
		}
		return null;	//indicate that no text was found
	}

	/**Retrieves an array of vCard text from a given RDF object in a context that expects one or more text values to be present.
	If the object is an {@link RDFListResource}, an array will be created from the result of {@link #getText(RDFObject)} for each list element;
	<code>null</code> text values will be excluded from the list.
	If the object is an {@link RDFLiteral} or {@link RDFResource}, an array with a single element will be returned containing the result of {@link #getText(RDFObject)}.
	@param rdfObject The object that is supposed to represent text or a list of text.
	@return An array of objects representing the text and any language specification.
	@throws NullPointerException if the given RDF object is <code>null</code>.
	*/
	public static LocaledText[] getTexts(final RDFObject rdfObject)
	{
		checkInstance(rdfObject, "RDF object cannot be null.");
		if(rdfObject instanceof RDFListResource)	//if the object is a list
		{
			final RDFListResource<?> rdfList=(RDFListResource<?>)rdfObject;	//get the object as a list
			final List<LocaledText> texts=new ArrayList<LocaledText>(rdfList.size());	//create a list of locale texts
			for(final RDFObject item:rdfList)	//for each list element
			{
				final LocaledText text=getText(item);	//get this text
				if(text!=null)	//if this resource represents text
				{
					texts.add(text);	//add this text to the list
				}
			}
			return texts.toArray(new LocaledText[texts.size()]);	//return the texts as an array
		}
		else	//for all other component types
		{
			return new LocaledText[]{getText(rdfObject)};	//get the text from the object normally and store it in an array of a single element
		}
	}

	/**Returns the value of the first <code>dc:language</code> property.
	@param resource The resource the property of which should be located.
	@return The value of the first <code>dc:language</code> property, or
		<code>null</code> if no such property exists or the property value does
		not contain a literal language tag.
	*/
	public static Locale getLanguage(final RDFResource resource)
	{
		final RDFObject languageObject=resource.getPropertyValue(DIRECTORY_NAMESPACE_URI, LANGUAGE_PROPERTY_NAME);
		if(languageObject instanceof RDFLiteral)	//if this is a literal value
		{
			return createLocale(((RDFLiteral)languageObject).getLexicalForm());	//create a locale from the literal's lexical form
		}
		else	//if there is no literal language tag
		{
			return null;	//show that we can't create a language locale from a non-literal
		}
	}

	/**Sets the <code>directory:language</code> property with the given value to the resource.
	@param resource The resource to which the property should be set.
	@param locale The property value to set.
	@return The added literal property value.
	*/
	public static RDFLiteral setLanguage(final RDFResource resource, final Locale locale)
	{
		return resource.setProperty(DIRECTORY_NAMESPACE_URI, LANGUAGE_PROPERTY_NAME, getLanguageTag(locale));
	}

}
