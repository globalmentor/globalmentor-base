package com.garretwilson.rdf;

import java.net.URI;
import java.util.*;
import com.garretwilson.util.Debug;
import com.garretwilson.util.NameValuePair;

/**Represents an RDF bag resource.
@author Garret Wilson
*/
public class RDFBagResource extends RDFContainerResource
{

	/**Constructs an RDF bag resource with a reference URI.
	@param newReferenceURI The reference URI for the new resource.
	*/
	public RDFBagResource(final URI newReferenceURI)
	{
		super(newReferenceURI); //construct the parent class
	}

	/**Convenience constructor that constructs an RDF bag resource using a
		namespace URI and local name which will be combined to form the reference
		URI.
	@param newNamespaceURI The XML namespace URI used in the serialization.
	@param newLocalName The XML local name used in the serialization.
	*/
	public RDFBagResource(final URI newNamespaceURI, final String newLocalName)
	{
		super(newNamespaceURI, newLocalName); //construct the parent class
	}

}