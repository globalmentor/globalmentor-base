package com.garretwilson.rdf.maqro;

import java.net.URI;
import com.garretwilson.rdf.*;

/**Criteria for ordering MAQRO interactions.
@author Garret Wilson
*/
public abstract class Order extends TypedRDFResource implements MAQROConstants
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return MAQRO_NAMESPACE_URI;}

	/**Default constructor.*/
	public Order()
	{
		super();	//construct the parent class
	}

	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public Order(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

}
