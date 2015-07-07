/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.rdf;

import com.globalmentor.net.ContentType;

/**
 * Constants used in RDF+XML serializations.
 * @author Garret Wilson
 */
public class RDFXML {

	/** The name extension for Resource Description Framework (RDF) files serialized in XML. */
	public static final String NAME_EXTENSION = "rdf";

	/**
	 * The content type for RDF serialized in XML: <code>application/rdf+xml</code>.
	 * @see <a href="https://www.ietf.org/rfc/rfc3870.txt">RFC 3870: application/rdf+xml Media Type Registration</a>
	 */
	public static final ContentType CONTENT_TYPE = ContentType.create(ContentType.APPLICATION_PRIMARY_TYPE, "rdf+xml");

	//RDF XML elements
	/** The name of the enclosing RDF element. */
	public static final String ELEMENT_RDF = "RDF";
	/** The name of the RDF description element. */
	public static final String ELEMENT_DESCRIPTION = "Description";

	//RDF XML attributes
	/** The name of the rdf:about attribute. */
	public static final String ATTRIBUTE_ABOUT = "about";
	/** The name of the rdf:ID attribute. */
	public static final String ATTRIBUTE_ID = "ID";
	/** The name of the rdf:nodeID attribute. */
	public static final String ATTRIBUTE_NODE_ID = "nodeID";
	/** The name of the rdf:parseType attribute. */
	public static final String ATTRIBUTE_PARSE_TYPE = "parseType";
	/** The name of the rdf:resource attribute. */
	public static final String ATTRIBUTE_RESOURCE = "resource";
	/** The name of the rdf:datatype attribute. */
	public static final String ATTRIBUTE_DATATYPE = "datatype";

	//RDF XML attribute values
	/** The parse type for a collection. */
	public static final String COLLECTION_PARSE_TYPE = "Collection";
	/** The parse type for an XML literal. */
	public static final String LITERAL_PARSE_TYPE = "Literal";
	/** The parse type for a resource. */
	public static final String RESOURCE_PARSE_TYPE = "Resource";

}