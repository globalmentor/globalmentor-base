package com.garretwilson.rdf.maqro;

import java.net.URI;
import com.garretwilson.rdf.*;

/**A result of an outcome evaluation.
@author Garret Wilson
@see Outcome
*/
public abstract class Result extends TypedRDFResource implements MAQROConstants
{

	/**Default constructor.*/
	public Result()
	{
		super();	//construct the parent class
	}
	
	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public Result(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

}
