package com.garretwilson.rdf.crypto;

import java.net.URI;

/**Crypto digest information.
@author Garret Wilson
*/
public class DigestMethod extends AlgorithmMethod
{
	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return DIGEST_METHOD_CLASS_NAME;}

	/**Default constructor.*/
	public DigestMethod()
	{
		super();	//construct the parent class
	}

	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public DigestMethod(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

}
