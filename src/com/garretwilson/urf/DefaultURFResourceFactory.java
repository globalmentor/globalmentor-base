package com.garretwilson.urf;

import java.net.URI;
import java.util.Locale;

import javax.mail.internet.ContentType;
import com.garretwilson.io.ContentTypeUtilities;
import static com.garretwilson.net.URIConstants.*;
import static com.garretwilson.urf.URF.*;
import com.garretwilson.urf.content.Content;
import static com.garretwilson.util.LocaleUtilities.*;

/**A default factory to create default resources.
This factory also has convenience methods to create default resources of several lexical types.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class DefaultURFResourceFactory implements URFResourceFactory
{

	/**Creates a resource with the provided URI based upon the type URI, if any.
	If a type URI is provided, a corresponding type property value may be added to the resource before it is returned.
	This implementation returns a {@link DefaultURFResource}.
	@param resourceURI The URI of the resource to create, or <code>null</code> if the resource created should be anonymous.
	@param typeURI The URI of the resource type, or <code>null</code> if the type is not known.
	@return The resource created with this URI.
	@exception IllegalArgumentException if a resource could not be created based upon the given criteria.
	*/
	public URFResource createResource(final URI resourceURI, final URI typeURI)
	{
		return new DefaultURFResource(resourceURI, typeURI!=null ? new URI[]{typeURI} : NO_URIS);	//create and return a default resource with the type added, if any
	}

	/**Creates a default resource with a URI in a lexical namespace for the given resource type and lexical form.
	The indicated type is added as one of the resource's type property.
	This method delegates to {@link #createResource(URI, URI)}.
	@param typeURI The URI of the type of the resource.
	@param lexicalForm The canonical lexical form of the resource.
	@return A resource with the URI in the lexical namespace for the specified type based upon its lexical form.
	@exception NullPointerException if the given type URI and/or lexical form is <code>null</code>.
	*/
	public URFResource createLexicalResource(final URI typeURI, final String lexicalForm)
	{
		return createResource(createLexicalURI(typeURI, lexicalForm), typeURI);	//create a new resource from the appropriate lexical URI and add the indicated type
	}

	/**Creates a default integer resource with its type added as a type property.
	This method delegates to {@link #createLexicalResource(URI, String)}.
	@param integer The integer for which a default resource should be created.
	@return A default integer resource with the appropriate type property added.
	@exception NullPointerException if the given integer is <code>null</code>.
	*/
	public URFResource createIntegerResource(final Long integer)
	{
		return createLexicalResource(INTEGER_CLASS_URI, Long.toString(integer));	//create and return a default integer resource
	}

	/**Creates a default date time resource with its type added as a type property.
	This method delegates to {@link #createLexicalResource(URI, String)}.
	@param dateTime The date time for which a default resource should be created.
	@return A default date time resource with the appropriate type property added.
	@exception NullPointerException if the given date time is <code>null</code>.
	*/
	public URFResource createDateTimeResource(final URFDateTime dateTime)
	{
		return createLexicalResource(DATE_TIME_CLASS_URI, dateTime.toString());	//create and return a default date time resource
	}

	/**Creates a default language resource with its type added as a type property.
	This method delegates to {@link #createLexicalResource(URI, String)}.
	@param language The language for which a default resource should be created.
	@return A default language resource with the appropriate type property added.
	@exception NullPointerException if the given language is <code>null</code>.
	*/
	public URFResource createLanguageResource(final Locale language)
	{
		return createLexicalResource(LANGUAGE_CLASS_URI, getLanguageTag(language));	//create and return a default language resource
	}

	/**Creates a default media type resource with its type added as a type property.
	This method delegates to {@link #createLexicalResource(URI, String)}.
	@param mediaType The media type for which a default resource should be created.
	@return A default media type resource with the appropriate type property added.
	@exception NullPointerException if the given media type is <code>null</code>.
	*/
	public URFResource createMediaTypeResource(final ContentType mediaType)
	{
		return createLexicalResource(Content.MEDIA_TYPE_CLASS_URI, ContentTypeUtilities.toString(mediaType.getPrimaryType(), mediaType.getSubType()));	//create and return a default media type resource
	}

	/**Creates a default string resource with its type added as a type property.
	This method delegates to {@link #createLexicalResource(URI, String)}.
	@param string The string for which a default resource should be created.
	@return A default string resource with the appropriate type property added.
	@exception NullPointerException if the given string is <code>null</code>.
	*/
	public URFResource createStringResource(final String string)
	{
		return createLexicalResource(STRING_CLASS_URI, string);	//create and return a default string resource
	}

	/**Creates a default URI resource with its type added as a type property.
	This method delegates to {@link #createLexicalResource(URI, String)}.
	@param uri The URI for which a default resource should be created.
	@return A default URI resource with the appropriate type property added.
	@exception NullPointerException if the given URI is <code>null</code>.
	*/
	public URFResource createURIResource(final URI uri)
	{
		return createLexicalResource(URI_CLASS_URI, uri.toString());	//create and return a default URI resource
	}
}