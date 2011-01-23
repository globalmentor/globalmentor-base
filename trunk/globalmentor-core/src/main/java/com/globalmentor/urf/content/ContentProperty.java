package com.globalmentor.urf.content;

import java.net.URI;

import static com.globalmentor.urf.content.Content.*;

/**An enumeration of the URF content properties.
<p>Copyright © 2007-2008 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public enum ContentProperty
{
	/**The date and time when a resource was last accessed.*/
	ACCESSED(ACCESSED_PROPERTY_URI),
	/**The charset of a resource.*/
	CHARSET(CHARSET_PROPERTY_URI),
	/**The date and time when a resource was created.*/
	CREATED(CREATED_PROPERTY_URI),
	/**The actual content, such as bytes or a string, of a resource..*/
	CONTENT(CONTENT_PROPERTY_URI),
	/**The list of child resources contained by a resource such as a collection or package.*/
	CONTENTS(CONTENTS_PROPERTY_URI),
	/**The date and time when a resource was last modified.*/
	MODIFIED(MODIFIED_PROPERTY_URI),
	/**The size of the contents of the resource.*/
	LENGTH(LENGTH_PROPERTY_URI),
	/**The Internet media type of a resource.*/
	TYPE(TYPE_PROPERTY_URI);

	/**The URI of this property.*/
	private final URI uri;
	
		/**@return The URI of this property.*/
		public URI getURI() {return uri;}

	/**URI constructor.
	@param uri The URI of this property.
	*/
	private ContentProperty(final URI uri)
	{
		this.uri=uri;	//save the URI
	}

}