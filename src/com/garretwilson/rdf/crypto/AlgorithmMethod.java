package com.garretwilson.rdf.crypto;

import java.net.URI;
import java.util.Iterator;

import com.garretwilson.lang.ObjectUtilities;
import com.garretwilson.rdf.*;

/**A general Crypto method, such as a digest or encryption method.
@author Garret Wilson
*/
public abstract class AlgorithmMethod extends TypedRDFResource implements CryptoConstants
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return CRYPTO_NAMESPACE_URI;}

	/**Default constructor.*/
	public AlgorithmMethod()
	{
		super();	//construct the parent class
	}

	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public AlgorithmMethod(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**Sets the <code>crypto:algorithm</code> property of the method.
	@param algorithm A resource representing the algorithm.
	*/
	public void setAlgorithm(final RDFResource algorithm)
	{
		setProperty(CRYPTO_NAMESPACE_URI, ALGORITHM_PROPERTY_NAME, algorithm); //add the algorithm resource
	}

	/**Retrieves the algorithm value of the resource. If this resource has more
		than one property of <code>crypto:algorithm</code>, it is undefined which
		of these property values will be returned.
	@return The algorithm of the resource, or <code>null</code> if there is no
		algorithm or the algorithm is not a resource.
	*/
	public RDFResource getAlgorithm()
	{
		return RDFUtilities.asResource(getPropertyValue(CRYPTO_NAMESPACE_URI, ALGORITHM_PROPERTY_NAME)); //get the value of the algorithm property as a resource
	}

}
