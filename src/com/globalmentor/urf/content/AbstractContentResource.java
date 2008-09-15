package com.globalmentor.urf.content;

import java.net.URI;

import javax.mail.internet.ContentType;

import com.globalmentor.urf.*;

/**An abstract implementation of a general resource that holds content in its description through the {@value Content#CONTENT_PROPERTY_URI} property.
This class allows content resource subclasses to be derived with a type automatically added for the name of the
class in the indicated namespace.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public abstract class AbstractContentResource extends AbstractClassTypedURFResource implements ContentResource
{

	/**URI and type namespace URI.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
	@param typeNamespaceURI The namespace URI of the URI of the type to be added.
	@exception NullPointerException if the given type type namespace URI is <code>null</code>.
	*/
	public AbstractContentResource(final URI uri, final URI typeNamespaceURI)
	{
		super(uri, typeNamespaceURI);	//construct the parent class
	}

	/**Returns the created date time.
	@return The created date time of the resource, or <code>null</code> if there is no created date time or the property does not contain an <code>urf.DateTime</code>.
	@see Content#CREATED_PROPERTY_URI
	*/
	public URFDateTime getCreated()
	{
		return Content.getCreated(this);
	}

	/**Sets the created property of the resource
	@param dateTime The new created date and time.
	@see Content#CREATED_PROPERTY_URI
	*/
	public void setCreated(final URFDateTime dateTime)
	{
		Content.setCreated(this, dateTime);
	}

	/**Returns the resource that created this resource on the indicated created date and time.
	@return The string value of the property, or <code>null</code> if there is no such property or the property value is not a string.
	@see Content#CREATOR_PROPERTY_URI
	*/
	public URFResource getCreator()
	{
		return Content.getCreator(this);
	}

	/**Sets the creator of the resource.
	@param value The property value to set.
	@see Content#CREATOR_PROPERTY_URI
	*/
	public void setCreator(final URFResource value)
	{
		Content.setCreator(this, value);
	}

	/**Returns the length of the resource contents.
	@return The size of the resource, or <code>-1</code> if the size could not be determined or the value was not an integer.
	@see Content#LENGTH_PROPERTY_URI
	*/ 
	public long getContentLength()
	{
		return Content.getContentLength(this);
	}

	/**Sets the length of the resource contents.
	@param length The content length.
	@see Content#LENGTH_PROPERTY_URI
	*/
	public void setContentLength(final long length) 
	{
		Content.setContentLength(this, length);
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

	/**Returns the modified date time.
	@return The modified date time of the resource, or <code>null</code> if there is no modified date time or the property does not contain an <code>urf.DateTime</code>.
	@see Content#MODIFIED_PROPERTY_URI
	*/
	public URFDateTime getModified()
	{
		return Content.getModified(this);
	}

	/**Sets the modified property of the resource
	@param dateTime The new modified date and time.
	@see Content#MODIFIED_PROPERTY_URI
	*/
	public void setModified(final URFDateTime dateTime)
	{
		Content.setModified(this, dateTime);
	}
}