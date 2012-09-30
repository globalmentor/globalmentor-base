/*
 * Copyright © 2007-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package org.urframework.dcmi;

import java.net.URI;

import static org.urframework.dcmi.DCMI.*;

/**An enumeration of the Dublin Core Metadata Initiate elements.
This enumeration does not cover the newer DCMI Metatadats Terms.
@author Garret Wilson
@see <a href="http://dublincore.org/documents/dces/">Dublin Core Metadata Element Set, Version 1.1</a>
@see <a href="http://dublincore.org/documents/dcmi-terms/">DCMI Metadata Terms</a>
*/
public enum DCMIElement
{

	/**The title of a resource.*/
	TITLE(TITLE_PROPERTY_URI),
	/**The creator of a resource.*/
	CREATOR(CREATOR_PROPERTY_URI),
	/**The subject of a resource.*/
	SUBJECT(SUBJECT_PROPERTY_URI),
	/**The description of a resource.*/
	DESCRIPTION(DESCRIPTION_PROPERTY_URI),
	/**The publisher of a resource.*/
	PUBLISHER(PUBLISHER_PROPERTY_URI),
	/**The contributor of a resource.*/
	CONTRIBUTOR(CONTRIBUTOR_PROPERTY_URI),
	/**The date of a resource.*/
	DATE(DATE_PROPERTY_URI),
	/**The Dublin Core type of a resource.*/
	TYPE(TYPE_PROPERTY_URI),
	/**The format of a resource.*/
	FORMAT(FORMAT_PROPERTY_URI),
	/**The Dublin Core identifier of a resource.*/
	IDENTIFIER(IDENTIFIER_PROPERTY_URI),
	/**The source of a resource.*/
	SOURCE(SOURCE_PROPERTY_URI),
	/**The language of a resource.*/
	LANGUAGE(LANGUAGE_PROPERTY_URI),
	/**The relation of a resource.*/
	RELATION(RELATION_PROPERTY_URI),
	/**The coverage of a resource.*/
	COVERAGE(COVERAGE_PROPERTY_URI),
	/**The rights of a resource.*/
	RIGHTS(RIGHTS_PROPERTY_URI);

	/**The URI of this element.*/
	private final URI uri;
	
		/**@return The URI of this element.*/
		public URI getURI() {return uri;}

	/**URI constructor.
	@param uri The URI of this element.
	*/
	private DCMIElement(final URI uri)
	{
		this.uri=uri;	//save the URI
	}

}