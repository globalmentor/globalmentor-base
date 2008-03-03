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

package com.globalmentor.rdf.maqro;

import java.net.URI;

import com.globalmentor.rdf.*;

import static com.globalmentor.rdf.maqro.MAQRO.*;

/**Criteria for selecting MAQRO interactions.
@author Garret Wilson
*/
public abstract class Selection extends TypedRDFResource
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return MAQRO_NAMESPACE_URI;}

	/**Default constructor.*/
	public Selection()
	{
		super();	//construct the parent class
	}

	/**Constructs selection criteria with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	public Selection(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The list of selectors for this selection, or <code>null</code>
		if there is no list of selectors or the value is not a list.
	*/
	public RDFListResource getSelectors()
	{
		return RDFResources.asListResource(getPropertyValue(MAQRO_NAMESPACE_URI, SELECTORS_PROPERTY_NAME));	//get the maqro:selectors property value as a list	
	}

	/**Sets the list of selectors.
	@param selectors The list of selectors.
	*/
	public void setSelectors(final RDFListResource selectors)
	{
		setProperty(MAQRO_NAMESPACE_URI, SELECTORS_PROPERTY_NAME, selectors);	//set the selectors
	}

	/**@return The order criteria, or <code>null</code> if there is no
		order criteria indicated or if it is not of the correct type.
	*/
	public Order getOrder()
	{
		final RDFObject order=getPropertyValue(MAQRO_NAMESPACE_URI, ORDER_PROPERTY_NAME);	//get the maqro:order property value
		return order instanceof Order ? (Order)order : null;	//return the order criteria if there are any
	}

	/**Sets the order criteria for the activity.
	@param orderDescription The order criteria.
	*/
	public void setOrder(final Order orderDescription)
	{
		setProperty(MAQRO_NAMESPACE_URI, ORDER_PROPERTY_NAME, orderDescription);	//set the order criteria
	}
	
}
