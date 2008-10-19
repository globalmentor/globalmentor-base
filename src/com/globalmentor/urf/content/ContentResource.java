package com.globalmentor.urf.content;

import com.globalmentor.net.ContentType;
import com.globalmentor.urf.*;

/**A general resource that holds content in its description through the {@value Content#CONTENT_PROPERTY_URI} property.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public interface ContentResource extends URFResource
{

	/**Returns the created date time.
	@return The created date time of the resource, or <code>null</code> if there is no created date time or the property does not contain an <code>urf.DateTime</code>.
	@see Content#CREATED_PROPERTY_URI
	*/
	public URFDateTime getCreated();

	/**Sets the created property of the resource
	@param dateTime The new created date and time.
	@see Content#CREATED_PROPERTY_URI
	*/
	public void setCreated(final URFDateTime dateTime);

	/**Returns the resource that created this resource on the indicated created date and time.
	@return The string value of the property, or <code>null</code> if there is no such property or the property value is not a string.
	@see Content#CREATOR_PROPERTY_URI
	*/
	public URFResource getCreator();

	/**Sets the creator of the resource.
	@param value The property value to set.
	@see Content#CREATOR_PROPERTY_URI
	*/
	public void setCreator(final URFResource value);

	/**Returns the length of the resource contents.
	@return The size of the resource, or <code>-1</code> if the size could not be determined or the value was not an integer.
	@see Content#LENGTH_PROPERTY_URI
	*/ 
	public long getContentLength();

	/**Sets the length of the resource contents.
	@param length The content length.
	@see Content#LENGTH_PROPERTY_URI
	*/
	public void setContentLength(final long length);
	
	/**Returns the declared content type of the resource as an Internet media type.
	@return This resource's content type declaration as a media type, or <code>null</code> if the resource has no <code>content.type</code> property specified
		or the content type was not a resource with an Internet media type URI.
	@see Content#TYPE_PROPERTY_URI
	*/
	public ContentType getContentType();

	/**Sets the content type property of the resource.
	@param contentType The object that specifies the content type, or <code>null</code> if there should be no content type.
	@see Content#TYPE_PROPERTY_URI
	*/
	public void setContentType(final ContentType contentType);

	/**Retrieves the collection of child resources of the resource.
	@return The contents of the resource, or <code>null</code> if no <code>content.contents</code> property exists or the value is not an instance of {@link URFCollectionResource}.
	@see Content#CONTENTS_PROPERTY_URI
	*/
	public <T extends URFResource> URFCollectionResource<T> getContents();

	/**Set the contents property of the resource.
	@param contents The collection of contents, or <code>null</code> if there should be no contents.
	@see Content#CONTENTS_PROPERTY_URI
	*/
	public void setContents(final URFCollectionResource<?> contents);

	/**Returns the actual string content of the resource.
	@return This resource's string content declaration, or <code>null</code> if the resource has no <code>content.content</code> property specified or the content is not a string.
	*/
	public String getStringContent();

	/**Sets this resource's content declaration with a text string.
	@param content This resource's content declaration, or <code>null</code> if the resource should have no <code>content.content</code> property.
	*/
	public void setContent(final String content);

	/**Returns the modified date time.
	@return The modified date time of the resource, or <code>null</code> if there is no modified date time or the property does not contain an <code>urf.DateTime</code>.
	@see Content#MODIFIED_PROPERTY_URI
	*/
	public URFDateTime getModified();

	/**Sets the modified property of the resource
	@param dateTime The new modified date and time.
	@see Content#MODIFIED_PROPERTY_URI
	*/
	public void setModified(final URFDateTime dateTime);

}
