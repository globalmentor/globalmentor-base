package com.garretwilson.rdf.xeb;

import java.net.URI;
import com.garretwilson.rdf.*;
import static com.garretwilson.rdf.xeb.XEBConstants.*;

/**An abstract XEbook publication.
@author Garret Wilson
*/
public abstract class Publication extends TypedRDFResource
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return XEB_NAMESPACE_URI;}

	/**Default constructor.*/
	public Publication()
	{
		super();	//construct the parent class
	}
	
	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public Publication(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

}
