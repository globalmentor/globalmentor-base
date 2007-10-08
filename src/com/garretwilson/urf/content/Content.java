package com.garretwilson.urf.content;

import java.net.URI;

import javax.mail.internet.ContentType;

import static com.garretwilson.io.ContentTypeUtilities.*;
import static com.garretwilson.net.URIConstants.*;
import static com.garretwilson.net.URIUtilities.*;
import com.garretwilson.urf.*;
import static com.garretwilson.urf.URF.*;

/**The URF content ontology.
@author Garret Wilson
*/
public class Content
{

	/**The recommended prefix to the URF content namespace.*/
	public final static String CONTENT_NAMESPACE_PREFIX="content";
	/**The URI to the URF content namespace.*/
	public final static URI CONTENT_NAMESPACE_URI=URI.create("http://urf.name/content");

		//classes
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
	@param mediaType The resource that specifies the Internet media type, or <code>null</code> if there should be no content type.
	*/
	public static void setContentType(final URFResource resource, final URFResource mediaType)
	{
		resource.setPropertyValue(TYPE_PROPERTY_URI, mediaType);	//set the content type
	}

	/**Sets the content type property of the resource.
	@param resource The resource for which the content type property should be set.
	@param mediaTypeURI The URI of the Internet media type, or <code>null</code> if there should be no content type.
	@exception IllegalArgumentException if the URI of any given media type URI is does not have an <code>info:media/</code> namespace.
	*/
	public static void setContentType(final URFResource resource, final URI mediaTypeURI)
	{
		setContentType(resource, mediaTypeURI!=null ? new DefaultURFResource(checkInfoNamespace(mediaTypeURI, INFO_SCHEME_MEDIA_NAMESPACE)) : null);	//if a media type URI was given, check for the info:media/ namespace and create a default resource
	}

	/**Sets the content type property of the resource.
	@param resource The resource for which the content type property should be set.
	@param contentType The object that specifies the content type, or <code>null</code> if there should be no content type.
	*/
	public static void setContentType(final URFResource resource, final ContentType contentType)
	{
		setContentType(resource, createInfoMediaURI(contentType));	//create an info:media/ URI from the given content type and set the resource's content type
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
