package com.garretwilson.rdf.xpackage;

import java.util.Iterator;

import com.garretwilson.rdf.*;

import static com.garretwilson.rdf.xpackage.XMLOntologyConstants.*;

/**Utilities for working with XPackage XML Ontology RDF.
@author Garret Wilson
*/
public class XMLOntologyUtilities
{

	/**Retrieves an iterator to the XML style resources, represented by  <code>x:style</code> properties.
	@param resource The resource the styles of which will be returned.
	@return An iterator to the styles of the resource, if any.
	*/
	public static Iterator<RDFObject> getStyles(final RDFResource resource)
	{
		return resource.getPropertyValueIterator(XML_ONTOLOGY_NAMESPACE_URI, STYLE_PROPERTY_NAME); //return an iterator to style properties
	}

}