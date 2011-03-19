/*
 * Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.urf;

import java.net.URI;
import java.nio.charset.Charset;
import java.util.Locale;

import static com.globalmentor.model.Locales.*;
import static com.globalmentor.net.URIs.*;
import static com.globalmentor.urf.URF.*;

import com.globalmentor.java.Classes;
import com.globalmentor.net.ContentType;
import com.globalmentor.urf.content.Content;

/**A default factory to create default resources.
This factory also has convenience methods to create default resources of several inline types.
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

	/**Creates a default resource with a URI in an inline namespace for the given resource type and lexical form.
	The indicated type is added as one of the resource's type property.
	This method delegates to {@link #createResource(URI, URI)}.
	@param typeURI The URI of the type of the resource.
	@param lexicalForm The canonical lexical form of the resource.
	@return A resource with the URI in the inline namespace for the specified type based upon its lexical form.
	@exception NullPointerException if the given type URI and/or lexical form is <code>null</code>.
	*/
	public URFResource createInlineResource(final URI typeURI, final String lexicalForm)
	{
		return createResource(createInlineURI(typeURI, lexicalForm), typeURI);	//create a new resource from the appropriate type URI and add the indicated type
	}

	/**Creates a default charset resource with its type added as a type property.
	This method delegates to {@link #createInlineResource(URI, String)}.
	@param charset The charset for which a default resource should be created.
	@return A default charset resource with the appropriate type property added.
	@exception NullPointerException if the given charset is <code>null</code>.
	*/
	public URFResource createCharsetResource(final Charset charset)
	{
		return createInlineResource(Content.CHARSET_CLASS_URI, charset.name());	//create and return a default charset resource
	}

	/**Creates a default class resource with the appropriate Java class URI.
	This method delegates to {@link #createResource(URI, URI)}.
	@param objectClass The class for which a default resource should be created.
	@return A default class resource with the appropriate Java class URI.
	@exception NullPointerException if the given class is <code>null</code>.
	*/
	public URFResource createClassResource(final Class<?> objectClass)
	{
		return createResource(Classes.createJavaURI(objectClass), null);	//create an untyped resource with a Java class URI
	}

	/**Creates a default boolean resource with its type added as a type property.
	This method delegates to {@link #createInlineResource(URI, String)}.
	@param bool The boolean for which a default resource should be created.
	@return A default boolean resource with the appropriate type property added.
	*/
	public URFResource createBooleanResource(final boolean bool)
	{
		return createInlineResource(BOOLEAN_CLASS_URI, Boolean.toString(bool));	//create and return a default boolean resource
	}

	/**Creates a default integer resource with its type added as a type property.
	This method delegates to {@link #createInlineResource(URI, String)}.
	@param integer The integer for which a default resource should be created.
	@return A default integer resource with the appropriate type property added.
	*/
	public URFResource createIntegerResource(final long integer)
	{
		return createInlineResource(INTEGER_CLASS_URI, Long.toString(integer));	//create and return a default integer resource
	}

	/**Creates a default date resource with its type added as a type property.
	This method delegates to {@link #createInlineResource(URI, String)}.
	@param date The date for which a default resource should be created.
	@return A default date resource with the appropriate type property added.
	@exception NullPointerException if the given date is <code>null</code>.
	*/
	public URFResource createDateResource(final URFDate date)
	{
		return createInlineResource(DATE_CLASS_URI, date.toString());	//create and return a default date resource
	}

	/**Creates a default date time resource with its type added as a type property.
	This method delegates to {@link #createInlineResource(URI, String)}.
	@param dateTime The date time for which a default resource should be created.
	@return A default date time resource with the appropriate type property added.
	@exception NullPointerException if the given date time is <code>null</code>.
	*/
	public URFResource createDateTimeResource(final URFDateTime dateTime)
	{
		return createInlineResource(DATE_TIME_CLASS_URI, dateTime.toString());	//create and return a default date time resource
	}

	/**Creates a default language resource with its type added as a type property.
	This method delegates to {@link #createInlineResource(URI, String)}.
	@param language The language for which a default resource should be created.
	@return A default language resource with the appropriate type property added.
	@exception NullPointerException if the given language is <code>null</code>.
	*/
	public URFResource createLanguageResource(final Locale language)
	{
		return createInlineResource(LANGUAGE_CLASS_URI, getLanguageTag(language));	//create and return a default language resource
	}

	/**Creates a default media type resource with its type added as a type property.
	This method delegates to {@link #createInlineResource(URI, String)}.
	@param mediaType The media type for which a default resource should be created.
	@return A default media type resource with the appropriate type property added.
	@exception NullPointerException if the given media type is <code>null</code>.
	@see ContentType#getBaseType()
	*/
	public URFResource createMediaTypeResource(final ContentType mediaType)
	{
		return createInlineResource(Content.MEDIA_TYPE_CLASS_URI, mediaType.getBaseType());	//create and return a default media type resource from the media type base type
	}

	/**Creates a default rational resource with its type added as a type property.
	This method delegates to {@link #createInlineResource(URI, String)}.
	@param rational The rational for which a default resource should be created.
	@return A default integer resource with the appropriate type property added.
	*/
	public URFResource createRationalResource(final double rational)
	{
		return createInlineResource(RATIONAL_CLASS_URI, Double.toString(rational));	//create and return a default rational resource
	}

	/**Creates a default string resource with its type added as a type property.
	This method delegates to {@link #createInlineResource(URI, String)}.
	@param string The string for which a default resource should be created.
	@return A default string resource with the appropriate type property added.
	@exception NullPointerException if the given string is <code>null</code>.
	*/
	public URFResource createStringResource(final String string)
	{
		return createInlineResource(STRING_CLASS_URI, string);	//create and return a default string resource
	}

	/**Creates a default URI resource with its type added as a type property.
	This method delegates to {@link #createInlineResource(URI, String)}.
	@param uri The URI for which a default resource should be created.
	@return A default URI resource with the appropriate type property added.
	@exception NullPointerException if the given URI is <code>null</code>.
	*/
	public URFResource createURIResource(final URI uri)
	{
		return createInlineResource(URI_CLASS_URI, uri.toString());	//create and return a default URI resource
	}
}