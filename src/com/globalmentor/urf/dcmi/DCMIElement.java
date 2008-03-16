package com.globalmentor.urf.dcmi;

import java.net.URI;

import static com.globalmentor.urf.dcmi.DCMI.*;

/**An enumeration of the Dublin Core Metadata Initiate elements.
This enumeration does not cover the newer DCMI Metatadat Terms.
<p>Copyright © 2007-2008 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
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