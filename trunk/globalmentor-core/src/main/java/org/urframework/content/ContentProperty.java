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

package org.urframework.content;

import java.net.URI;

import static org.urframework.content.Content.*;

/**An enumeration of the URF content properties.
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