package com.garretwilson.rdf.xeb;

import java.net.URI;

/**An abstract XEbook publication.
@author Garret Wilson
*/
public abstract class Publication extends Binding
{

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
