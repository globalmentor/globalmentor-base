package com.garretwilson.rdf.maqro;

import java.net.URI;

/**Random selection of MAQRO interactions.
@author Garret Wilson
*/
public class RandomSelection extends Selection implements MAQROConstants
{

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return SEQUENTIAL_SELECTION_CLASS_NAME;}

	/**Default constructor.*/
	public RandomSelection()
	{
		super();	//construct the parent class
	}

	/**Constructs selection criteria with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	public RandomSelection(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

}
