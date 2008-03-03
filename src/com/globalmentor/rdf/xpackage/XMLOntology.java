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

package com.globalmentor.rdf.xpackage;

import java.net.URI;

import com.globalmentor.rdf.*;

/**Utilities for working with XPackage XML Ontology RDF.
@author Garret Wilson
@deprecated
*/
public class XMLOntology
{

	/**The recommended prefix to the XML ontology namespace.*/
	public final static String XML_ONTOLOGY_NAMESPACE_PREFIX="x";
	/**The URI to the XML ontology namespace.*/
	public final static URI XML_ONTOLOGY_NAMESPACE_URI=URI.create("http://xpackage.org/namespaces/xml#");


		//XML ontology property names
	/**The DTD of a resource. The local name of <code>x:dtd</code>.*/
	public final static String DTD_PROPERTY_NAME="dtd";
	/**The namespace of a resource. The local name of <code>x:namespace</code>.*/
	public final static String NAMESPACE_PROPERTY_NAME="namespace";
	/**The style of a resource. The local name of <code>x:style</code>.*/
	public final static String STYLE_PROPERTY_NAME="style";
	/**The transformation stylesheet of a resource. The local name of <code>x:transform</code>.*/
	public final static String TRANSFORM_PROPERTY_NAME="transform";

	/**Retrieves an iterable to the XML style resources, represented by <code>x:style</code> properties.
	@param resource The resource the styles of which will be returned.
	@return An iterable to the styles of the resource, if any.
	*/
	public static Iterable<RDFResource> getStyles(final RDFResource resource)
	{
		return resource.getPropertyValues(XML_ONTOLOGY_NAMESPACE_URI, STYLE_PROPERTY_NAME, RDFResource.class); //return an iterable to style properties
	}

	/**Retrieves an iterable to the XML namespace resources, represented by <code>x:namespace</code> properties.
	@param resource The resource the namespaces of which will be returned.
	@return An iterable to the namespaces of the resource, if any.
	*/
	public static Iterable<RDFResource> getNamespaces(final RDFResource resource)
	{
		return resource.getPropertyValues(XML_ONTOLOGY_NAMESPACE_URI, NAMESPACE_PROPERTY_NAME, RDFResource.class); //return an iterable to namespace properties
	}

}