package com.garretwilson.rdf.maqro;

import java.net.URI;
import static com.garretwilson.rdf.maqro.MAQROConstants.*;

/**Sequential ordering of MAQRO interactions.
@author Garret Wilson
*/
public class SequentialOrder extends Order
{

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return SEQUENTIAL_ORDER_CLASS_NAME;}

	/**Default constructor.*/
	public SequentialOrder()
	{
		super();	//construct the parent class
	}

	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public SequentialOrder(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

}
