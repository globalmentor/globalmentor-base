package com.globalmentor.rdf.maqro;

import java.net.URI;

import com.globalmentor.rdf.*;

import static com.globalmentor.rdf.maqro.MAQROConstants.*;

/**A result of an outcome evaluation.
@author Garret Wilson
@see Outcome
*/
public abstract class Result extends TypedRDFResource
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
