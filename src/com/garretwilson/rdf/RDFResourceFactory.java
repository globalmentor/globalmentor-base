package com.garretwilson.rdf;

import java.net.URI;

/**A factory to create resources.
@author Garret Wilson
@see RDF
*/
public interface RDFResourceFactory
{

	/**Creates a resource with the provided reference URI based upon the type reference URI composed of the given XML serialization type namespace and local name.
	A type property derived from the specified type namespace URI and local name will be added to the resource.
	@param referenceURI The reference URI of the resource to create, or <code>null</code> if the resource created should be represented by a blank node.
	@param typeNamespaceURI The XML namespace used in the serialization of the type URI, or <code>null</code> if the type is not known.
	@param typeLocalName The XML local name used in the serialization of the type URI, or <code>null</code> if the type is not known.
	@return The resource created with this reference URI, with the given type added if a type was given, or <code>null</code> if no suitable resource can be created.
	*/
	public RDFResource createResource(final URI referenceURI, final URI typeNamespaceURI, final String typeLocalName);

}