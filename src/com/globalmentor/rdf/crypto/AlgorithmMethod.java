/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.rdf.crypto;

import java.net.URI;

import com.globalmentor.rdf.*;
import static com.globalmentor.rdf.crypto.Crypto.*;

/**A general Crypto method, such as a digest or encryption method.
@author Garret Wilson
*/
public abstract class AlgorithmMethod extends TypedRDFResource
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
		return RDFResources.asResource(getPropertyValue(CRYPTO_NAMESPACE_URI, ALGORITHM_PROPERTY_NAME)); //get the value of the algorithm property as a resource
	}

}
