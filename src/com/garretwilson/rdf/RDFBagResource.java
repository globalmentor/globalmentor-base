package com.garretwilson.rdf;

import java.net.URI;

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

	/**Data model and reference URI constructor.
	@param rdf The data model associated with the container.
	@param newReferenceURI The reference URI for the new resource.
	*/
	RDFBagResource(final RDF rdf, final URI newReferenceURI)
	{
		super(rdf, newReferenceURI); //construct the parent class
	}

}