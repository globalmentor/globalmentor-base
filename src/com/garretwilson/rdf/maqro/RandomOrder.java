package com.garretwilson.rdf.maqro;

import java.net.URI;

/**Random ordering of MAQRO interactions.
@author Garret Wilson
*/
public class RandomOrder extends Order implements MAQROConstants
{

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return RANDOM_ORDER_CLASS_NAME;}

	/**Default constructor.*/
	public RandomOrder()
	{
		super();	//construct the parent class
	}

	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public RandomOrder(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

}
