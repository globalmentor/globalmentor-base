package com.garretwilson.rdf.maqro;

import java.net.URI;
import static com.garretwilson.rdf.maqro.MAQROConstants.*;

/**Random selection of MAQRO interactions.
@author Garret Wilson
*/
public class RandomSelection extends Selection
{

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return RANDOM_SELECTION_CLASS_NAME;}

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
