package com.garretwilson.urf.content;

import java.net.URI;

import javax.mail.internet.ContentType;
import static com.garretwilson.io.ContentTypeUtilities.*;
import com.garretwilson.net.Resource;
import com.garretwilson.urf.*;

import static com.garretwilson.urf.URF.*;

/**The URF content ontology.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class Content
{

	/**The URI to the URF content namespace.*/
	public final static URI CONTENT_NAMESPACE_URI=URI.create("http://urf.name/content");

		//classes
	/**A resource, such as a package or folder, that contains other resources.*/
//TODO del	public final static URI COLLECTION_CLASS_URI=createResourceURI(CONTENT_NAMESPACE_URI, "Collection");;
	/**The URI of the content <code>ContentResource</code> class.*/
	public final static URI CONTENT_RESOURCE_CLASS_URI=createResourceURI(CONTENT_NAMESPACE_URI, "ContentResource");
	/**The URI of the content <code>MediaType</code> class.*/
	public final static URI MEDIA_TYPE_CLASS_URI=createResourceURI(CONTENT_NAMESPACE_URI, "MediaType");
		//properties
	/**The date and time when a resource was last accessed.*/
	public final static URI ACCESSED_PROPERTY_URI=createResourceURI(CONTENT_NAMESPACE_URI, "accessed");
	/**The date and time when a resource was created.*/
	public final static URI CREATED_PROPERTY_URI=createResourceURI(CONTENT_NAMESPACE_URI, "created");
	/**The actual content, such as bytes or a string, of a resource.*/
	public final static URI CONTENT_PROPERTY_URI=createResourceURI(CONTENT_NAMESPACE_URI, "content");
	/**The array of child resources contained by a resource such as a collection or package.*/
	public final static URI CONTENTS_PROPERTY_URI=createResourceURI(CONTENT_NAMESPACE_URI, "contents");
	/**The date and time when a resource was last modified.*/
	public final static URI MODIFIED_PROPERTY_URI=createResourceURI(CONTENT_NAMESPACE_URI, "modified");
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
		return createLexicalURI(MEDIA_TYPE_CLASS_URI, mediaType.getBaseType());	//create a media type URI
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
			return createContentType(getLocalName(resourceURI));	//create a media type from the local name
		}
		return null;	//no media type could be found
	}

	/**Returns the accessed date time.
	@param resource The resource for which the accessed date time should be returned.
	@return The accessed date time of the resource, or <code>null</code> if there is no accessed date time or the property does not contain an <code>urf.DateTime</code>.
	@see #ACCESSED_PROPERTY_URI
	*/
	public static URFDateTime getAccessed(final URFResource resource)
	{
		return asDateTime(resource.getPropertyValue(ACCESSED_PROPERTY_URI));	//return the accessed timestamp as a date time
	}

	/**Sets the accessed property of the resource
	@param resource The resource the accessed date and time to set.
	@param dateTime The new accessed date and time.
	@see #ACCESSED_PROPERTY_URI
	*/
	public static void setAccessed(final URFResource resource, final URFDateTime dateTime)
	{
		resource.setPropertyValue(ACCESSED_PROPERTY_URI, dateTime);	//create a date time resource and set the resource's accessed timestamp
	}
	
	/**Returns the actual string content of the resource.
	@param resource The resource for which the content should be returned.
	@return This resource's string content declaration, or <code>null</code> if the resource has no <code>content:content</code> property specified or the content is not a string.
	@see #CONTENT_PROPERTY_URI
	*/
	public static String getStringContent(final URFResource resource)
	{
		return asString(resource.getPropertyValue(CONTENT_PROPERTY_URI));	//return the content as a string
	}

	/**Sets this resource's content declaration with a text string.
	@param resource The resource for which the content property should be set.
	@param content This resource's content declaration, or <code>null</code> if the resource should have no <code>content:content</code> property.
	@see #CONTENT_PROPERTY_URI
	*/
	public static void setContent(final URFResource resource, final String content)
	{
		resource.setPropertyValue(CONTENT_PROPERTY_URI, content);	//set the content:content property
	}

	/**Retrieves the collection of child resources of the resource.
	@param resource The resource the contents of which will be returned.
	@return The contents of the resource, or <code>null</code> if no <code>content.contents</code> property exists or the value is not an instance of {@link URFCollectionResource}.
	@see #CONTENTS_PROPERTY_URI
	*/
	public static <T extends URFResource> URFCollectionResource<T> getContents(final URFResource resource)
	{
		return asCollectionInstance(resource.getPropertyValue(CONTENTS_PROPERTY_URI)); //return the contents, if any
	}

	/**Set the contents property of the resource.
	@param resource The resource for which the collection of contents should be set.
	@param contents The collection of contents, or <code>null</code> if there should be no contents.
	@see #CONTENTS_PROPERTY_URI
	*/
	public static void setContents(final URFResource resource, final URFCollectionResource<?> contents)
	{
		resource.setPropertyValue(CONTENTS_PROPERTY_URI, contents);	//set the contents of the resource
	}

	/**Returns the created date time.
	@param resource The resource for which the created date time should be returned.
	@return The created date time of the resource, or <code>null</code> if there is no created date time or the property does not contain an <code>urf.DateTime</code>.
	@see #CREATED_PROPERTY_URI
	*/
	public static URFDateTime getCreated(final URFResource resource)
	{
		return asDateTime(resource.getPropertyValue(CREATED_PROPERTY_URI));	//return the created timestamp as a date time
	}

	/**Sets the created property of the resource
	@param resource The resource the created date and time to set.
	@param dateTime The new created date and time.
	@see #CREATED_PROPERTY_URI
	*/
	public static void setCreated(final URFResource resource, final URFDateTime dateTime)
	{
		resource.setPropertyValue(CREATED_PROPERTY_URI, dateTime);	//create a date time resource and set the resource's modified timestamp
	}
	
	/**Returns the modified date time.
	@param resource The resource for which the modified date time should be returned.
	@return The modified date time of the resource, or <code>null</code> if there is no modified date time or the property does not contain an <code>urf.DateTime</code>.
	@see #MODIFIED_PROPERTY_URI
	*/
	public static URFDateTime getModified(final URFResource resource)
	{
		return asDateTime(resource.getPropertyValue(MODIFIED_PROPERTY_URI));	//return the modified timestamp as a date time
	}

	/**Sets the modified property of the resource
	@param resource The resource the modified date and time to set.
	@param dateTime The new modified date and time.
	@see #MODIFIED_PROPERTY_URI
	*/
	public static void setModified(final URFResource resource, final URFDateTime dateTime)
	{
		resource.setPropertyValue(MODIFIED_PROPERTY_URI, dateTime);	//create a date time resource and set the resource's modified timestamp
	}

	/**Returns the length of the resource contents.
	@param resource The resource for which a content length should be returned.
	@return The size of the resource, or <code>-1</code> if the size could not be determined or the value was not an integer.
	@see #LENGTH_PROPERTY_URI
	*/ 
	public static long getContentLength(final URFResource resource)
	{
		final Long length=asInteger(resource.getPropertyValue(LENGTH_PROPERTY_URI));	//return the length as an integer
		return length!=null ? length.longValue() : -1;	//return the size, or -1 if we couldn't find the size
	}

	/**Sets the length of the resource contents.
	@param resource The resource for which the size should be set.
	@param length The content length.
	@see #LENGTH_PROPERTY_URI
	*/
	public static void setContentLength(final URFResource resource, final long length) 
	{
		resource.setPropertyValue(LENGTH_PROPERTY_URI, length); //set the length
	}

	/**Returns the declared content type of the resource as an Internet media type.
	@param resource The resource for which the content type should be returned.
	@return This resource's content type declaration as a media type, or <code>null</code> if the resource has no <code>content.type</code> property specified
		or the content type was not a resource with an Internet media type URI.
	@see #TYPE_PROPERTY_URI
	*/
	public static ContentType getContentType(final URFResource resource)
	{
		return asMediaType(resource.getPropertyValue(TYPE_PROPERTY_URI));	//return the content type, if any, as a media type
	}

	/**Sets the content type property of the resource.
	@param resource The resource for which the content type property should be set.
	@param contentType The object that specifies the content type, or <code>null</code> if there should be no content type.
	@see #TYPE_PROPERTY_URI
	*/
	public static void setContentType(final URFResource resource, final ContentType contentType)
	{
		resource.setPropertyValue(TYPE_PROPERTY_URI, DEFAULT_URF_RESOURCE_FACTORY.createMediaTypeResource(contentType));	//create a media type resource and set the resource's content type
	}

	/**The default resource factory for the content ontology.
	This resource factory can create the following types of resource objects for the given types:
	<dl>
		<dt>{@value #CONTENT_RESOURCE_CLASS_URI}</dt> <dd>{@link ContentResource}</dd>
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
					if(CONTENT_RESOURCE_CLASS_URI.equals(typeURI))	//if this is a content resource
					{
						return new ContentResource(resourceURI);	//create a new content resource
					}
					return super.createResource(resourceURI, typeURI);	//if we don't recognize the type, create a default resource
				}
			};
}
