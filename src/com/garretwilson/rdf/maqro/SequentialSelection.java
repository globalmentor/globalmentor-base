package com.garretwilson.rdf.maqro;

import java.net.URI;

/**Sequential selection of MAQRO interactions.
@author Garret Wilson
*/
public class SequentialSelection extends Selection implements MAQROConstants
{

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return SEQUENTIAL_SELECTION_CLASS_NAME;}

	/**Default constructor.*/
	public SequentialSelection()
	{
		super();	//construct the parent class
	}

	/**Constructs selection criteria with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	public SequentialSelection(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

}
