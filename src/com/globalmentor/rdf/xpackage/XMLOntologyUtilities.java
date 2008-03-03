package com.globalmentor.rdf.xpackage;

import com.globalmentor.rdf.*;

import static com.globalmentor.rdf.xpackage.XMLOntologyConstants.*;

/**Utilities for working with XPackage XML Ontology RDF.
@author Garret Wilson
*/
public class XMLOntologyUtilities
{

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