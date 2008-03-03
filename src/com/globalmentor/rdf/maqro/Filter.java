package com.globalmentor.rdf.maqro;

import java.net.URI;

import com.globalmentor.rdf.*;

import static com.globalmentor.rdf.maqro.MAQROConstants.*;

/**Filter for selecting MAQRO interactions.
@author Garret Wilson
*/
public abstract class Filter extends TypedRDFResource
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return MAQRO_NAMESPACE_URI;}

	/**Default constructor.*/
	public Filter()
	{
		super();	//construct the parent class
	}

	/**Constructs selection criteria with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	public Filter(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

}
