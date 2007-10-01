package com.garretwilson.urf.content;

import java.net.URI;

import javax.mail.internet.ContentType;

import com.garretwilson.urf.*;
import static com.garretwilson.urf.content.Content.*;

/**A general resource that holds content in its description through the {@value Content#CONTENT_PROPERTY_URI} property.
@author Garret Wilson
*/
public class ContentResource extends DefaultURFResource
{

	/**Default constructor with no URI.*/
	public ContentResource()
	{
		this(null);	//create a resource without a URI
	}

	/**URI constructor.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
	*/
	public ContentResource(final URI uri)
	{
		super(uri, RESOURCE_CLASS_URI);	//construct the parent class, specifying the type
	}

	/**Returns the declared content type of the resource.
	@return This resource's content type declaration, or <code>null</code> if the resource has no <code>content:type</code> property specified.
	*/
	public URFResource getContentType()
	{
		return Content.getContentType(this);	//return the content type
	}

	/**Returns the declared content type of the resource as an Internet media type.
	@return This resource's content type declaration as a media type, or <code>null</code> if the resource has no <code>content:type</code> property specified
		or the content type was not a resource with an Internet media type <code>info:media/</code> URI.
	*/
	public ContentType getContentMediaType()
	{
		return Content.getContentMediaType(this);	//return the content type, if any, as a media type
	}

	/**Sets the content type property of the resource.
	@param mediaType The resource that specifies the Internet media type, or <code>null</code> if there should be no content type.
	*/
	public void setContentType(final URFResource mediaType)
	{
		Content.setContentType(this, mediaType);	//set the content type
	}

	/**Sets the content type property of the resource.
	@param mediaTypeURI The URI of the Internet media type, or <code>null</code> if there should be no content type.
	@exception IllegalArgumentException if the URI of any given media type URI is does not have an <code>info:media/</code> namespace.
	*/
	public void setContentType(final URI mediaTypeURI)
	{
		Content.setContentType(this, mediaTypeURI);	//set the media type URI
	}

	/**Sets the content type property of the resource.
	@param contentType The object that specifies the content type, or <code>null</code> if there should be no content type.
	*/
	public void setContentType(final ContentType contentType)
	{
		Content.setContentType(this, contentType);	//set the content type
	}

	/**Retrieves the array of child resources of the resource.
	@return The contents of the resource, or <code>null</code> if no <code>content:contents</code> property exists or the value is not an instance of {@link URFListResource}.
	*/
	public URFListResource<URFResource> getContents()
	{
		return Content.getContents(this); //return the contents, if any
	}

	/**Set the contents property of the resource.
	@param contents The array of contents, or <code>null</code> if there should be no contents.
	*/
	public void setContents(final URFListResource<?> contents)
	{
		Content.setContents(this, contents);	//set the contents of the resource
	}

	/**Returns the actual string content of the resource.
	@return This resource's string content declaration, or <code>null</code> if the resource has no <code>content:content</code> property specified or the content is not a string.
	*/
	public String getStringContent()
	{
		return Content.getStringContent(this);	//return the content as a string
	}

	/**Sets this resource's content declaration with a text string.
	@param content This resource's content declaration, or <code>null</code> if the resource should have no <code>content:content</code> property.
	*/
	public void setContent(final String content)
	{
		Content.setContent(this, content);	//set the content:content property
	}
}
