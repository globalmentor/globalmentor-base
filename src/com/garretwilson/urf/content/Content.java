package com.garretwilson.urf.content;

import java.net.URI;

import javax.mail.internet.ContentType;

import static com.garretwilson.io.ContentTypeUtilities.*;
import com.garretwilson.io.ContentTypeUtilities;
import com.garretwilson.net.Resource;
import com.garretwilson.urf.*;
import static com.garretwilson.urf.URF.*;

/**The URF content ontology.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) <http://www.urf.name/> specification and processing
written by Garret Wilson <http://www.garretwilson.com/> and Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class Content
{

	/**The URI to the URF content namespace.*/
	public final static URI CONTENT_NAMESPACE_URI=URI.create("http://urf.name/content");

		//classes
	/**The URI of the content <code>MediaType</code> class.*/
	public final static URI MEDIA_TYPE_CLASS_URI=createResourceURI(URF_NAMESPACE_URI, "MediaType");
	/**The URI of the content <code>Resource</code> class.*/
	public final static URI RESOURCE_CLASS_URI=createResourceURI(CONTENT_NAMESPACE_URI, "Resource");
		//properties
	/**The actual content, such as bytes or a string, of a resource.*/
	public final static URI CONTENT_PROPERTY_URI=createResourceURI(CONTENT_NAMESPACE_URI, "content");
	/**The array of child resources contained by a resource such as a collection or package.*/
	public final static URI CONTENTS_PROPERTY_URI=createResourceURI(CONTENT_NAMESPACE_URI, "contents");
	/**The time when a resource was last modified.*/
	public final static URI MODIFIED_TIME_PROPERTY_URI=createResourceURI(CONTENT_NAMESPACE_URI, "modifiedTime");
	/**The size of the contents of the resource. For <code>urf.Binary</code> content, this indicates the number of bytes. For <code>urf.String</code> content, this indicates the number of characters.*/
	public final static URI LENGTH_PROPERTY_URI=createResourceURI(CONTENT_NAMESPACE_URI, "length");
	/**The Internet media type of a resource.*/
	public final static URI TYPE_PROPERTY_URI=createResourceURI(CONTENT_NAMESPACE_URI, "type");

		//lexical namespaces
	/**The media type lexical namespace URI.*/
	public final static URI MEDIA_TYPE_NAMESPACE_URI=createLexicalNamespaceURI(MEDIA_TYPE_CLASS_URI);

	/**Creates a URI to represent a content media type.
	@param mediaType The media type represent.
	@return A URI representing the given content media type.
	@exception NullPointerException if the given media type is <code>null</code>.
	@see #MEDIA_TYPE_CLASS_URI
	*/
	public static URI createMediaTypeURI(final ContentType mediaType)
	{
		return createLexicalURI(MEDIA_TYPE_CLASS_URI, ContentTypeUtilities.toString(mediaType.getBaseType(), mediaType.getSubType()));	//create a media type URI
	}

	/**Determines the media type represented by the given resource.
	@param resource The resource which is expected to represent a media type, or <code>null</code>.
	@return The media type represented by the given resource, or <code>null</code> if the resource does not represent a media type.
	@exception IllegalArgumentException if the given resource represents a media type that does not have the correct syntax.
	@see #asMediaType(URI)
	*/
	public static ContentType asMediaType(final Resource resource)
	{
		return resource!=null ? asMediaType(resource.getURI()) : null;	//if a resource was given, see if its URI represents a media type
	}

	/**Determines the media type represented by the given URI.
	@param resourceURI The URI which is expected to represent a media type, or <code>null</code>.
	@return The media type represented by the given URI, or <code>null</code> if the URI does not represent a media type.
	@exception IllegalArgumentException if the given URI represents a media type that does not have the correct syntax.
	@see #MEDIA_TYPE_CLASS_URI
	@see #MEDIA_TYPE_NAMESPACE_URI
	*/
	public static ContentType asMediaType(final URI resourceURI)
	{
		if(resourceURI!=null && MEDIA_TYPE_NAMESPACE_URI.equals(getNamespaceURI(resourceURI)))	//if a media type URI was given
		{
			createContentType(getLocalName(resourceURI));	//create a media type from the local name
		}
		return null;	//no media type could be found
	}

	/**Returns the declared content type of the resource.
	@param resource The resource for which the content type should be returned.
	@return This resource's content type declaration, or <code>null</code> if the resource has no <code>content:type</code> property specified.
	*/
	public static URFResource getContentType(final URFResource resource)
	{
		return resource.getPropertyValue(TYPE_PROPERTY_URI);	//return the content type
	}

	/**Returns the declared content type of the resource as an Internet media type.
	@param resource The resource for which the content type should be returned.
	@return This resource's content type declaration as a media type, or <code>null</code> if the resource has no <code>content:type</code> property specified
		or the content type was not a resource with an Internet media type <code>info:media/</code> URI.
	*/
	public static ContentType getContentMediaType(final URFResource resource)
	{
		return asMediaType(getContentType(resource));	//return the content type, if any, as a media type
	}

	/**Sets the content type property of the resource.
	@param resource The resource for which the content type property should be set.
	@param contentType The object that specifies the content type, or <code>null</code> if there should be no content type.
	*/
	public static void setContentType(final URFResource resource, final ContentType contentType)
	{
		resource.setPropertyValue(TYPE_PROPERTY_URI, DEFAULT_URF_RESOURCE_FACTORY.createMediaTypeResource(contentType));	//create a media type resource and set the resource's content type
}

	/**Retrieves the array of child resources of the resource.
	@param resource The resource the contents of which will be returned.
	@return The contents of the resource, or <code>null</code> if no <code>content:contents</code> property exists or the value is not an instance of {@link URFListResource}.
	*/
	public static URFListResource<URFResource> getContents(final URFResource resource)
	{
		return asListInstance(resource.getPropertyValue(CONTENTS_PROPERTY_URI)); //return the contents, if any
	}

	/**Set the contents property of the resource.
	@param resource The resource for which the array of contents should be set.
	@param contents The array of contents, or <code>null</code> if there should be no contents.
	*/
	public static void setContents(final URFResource resource, final URFListResource<?> contents)
	{
		resource.setPropertyValue(CONTENT_PROPERTY_URI, contents);	//set the contents of the resource
	}

	/**Returns the actual string content of the resource.
	@param resource The resource for which the content should be returned.
	@return This resource's string content declaration, or <code>null</code> if the resource has no <code>content:content</code> property specified or the content is not a string.
	*/
	public static String getStringContent(final URFResource resource)
	{
		return asString(resource.getPropertyValue(CONTENT_PROPERTY_URI));	//return the content as a string
	}

	/**Sets this resource's content declaration with a text string.
	@param resource The resource for which the content property should be set.
	@param content This resource's content declaration, or <code>null</code> if the resource should have no <code>content:content</code> property.
	*/
	public static void setContent(final URFResource resource, final String content)
	{
		resource.setPropertyValue(CONTENT_NAMESPACE_URI, content);	//set the content:content property
	}

	/**The default resource factory for the content ontology.
	This resource factory can create the following types of resource objects for the given types:
	<dl>
		<dt>{@value #RESOURCE_CLASS_URI}</dt> <dd>{@link ContentResource}</dd>
	</dl>
	*/
	public final static URFResourceFactory DEFAULT_CONTENT_RESOURCE_FACTORY=new DefaultURFResourceFactory()
			{
				/**Creates a resource with the provided URI based upon the type URI, if any.
				If a type URI is provided, a corresponding type property value may be added to the resource before it is returned.
				@param resourceURI The URI of the resource to create, or <code>null</code> if the resource created should be anonymous.
				@param typeURI The URI of the resource type, or <code>null</code> if the type is not known.
				@return The resource created with this URI.
				@exception IllegalArgumentException if a lexical resource URI was given with a different type URI than the specified type URI.
				*/
				public URFResource createResource(final URI resourceURI, final URI typeURI)
				{
					if(RESOURCE_CLASS_URI.equals(typeURI))	//if this is a content resource
					{
						return new ContentResource(resourceURI);	//create a new content resource
					}
					return super.createResource(resourceURI, typeURI);	//if we don't recognize the type, create a default resource
				}
			};
}
