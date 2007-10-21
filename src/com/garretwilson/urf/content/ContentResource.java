package com.garretwilson.urf.content;

import java.net.URI;

import javax.mail.internet.ContentType;

import com.garretwilson.urf.*;

import static com.garretwilson.urf.content.Content.*;

/**A general resource that holds content in its description through the {@value Content#CONTENT_PROPERTY_URI} property.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
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
		super(uri, CONTENT_RESOURCE_CLASS_URI);	//construct the parent class, specifying the type
	}

	/**Returns the declared content type of the resource as an Internet media type.
	@return This resource's content type declaration as a media type, or <code>null</code> if the resource has no <code>content.type</code> property specified
		or the content type was not a resource with an Internet media type URI.
	@see Content#TYPE_PROPERTY_URI
	*/
	public ContentType getContentType()
	{
		return Content.getContentType(this);	//return the content type, if any
	}

	/**Sets the content type property of the resource.
	@param contentType The object that specifies the content type, or <code>null</code> if there should be no content type.
	@see Content#TYPE_PROPERTY_URI
	*/
	public void setContentType(final ContentType contentType)
	{
		Content.setContentType(this, contentType);	//set the content type
	}

	/**Retrieves the collection of child resources of the resource.
	@return The contents of the resource, or <code>null</code> if no <code>content.contents</code> property exists or the value is not an instance of {@link URFCollectionResource}.
	@see Content#CONTENTS_PROPERTY_URI
	*/
	public <T extends URFResource> URFCollectionResource<T> getContents()
	{
		return Content.getContents(this); //return the contents, if any
	}

	/**Set the contents property of the resource.
	@param contents The collection of contents, or <code>null</code> if there should be no contents.
	@see Content#CONTENTS_PROPERTY_URI
	*/
	public void setContents(final URFCollectionResource<?> contents)
	{
		Content.setContents(this, contents);	//set the contents of the resource
	}

	/**Returns the actual string content of the resource.
	@return This resource's string content declaration, or <code>null</code> if the resource has no <code>content.content</code> property specified or the content is not a string.
	*/
	public String getStringContent()
	{
		return Content.getStringContent(this);	//return the content as a string
	}

	/**Sets this resource's content declaration with a text string.
	@param content This resource's content declaration, or <code>null</code> if the resource should have no <code>content.content</code> property.
	*/
	public void setContent(final String content)
	{
		Content.setContent(this, content);	//set the content.content property
	}
}
