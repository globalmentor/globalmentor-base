package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.*;
import com.garretwilson.rdf.*;
import com.garretwilson.rdf.xmlschema.*;

/**Criteria for ordering MAQRO interactions.
@author Garret Wilson
*/
public class OrderDescription extends TypedRDFResource implements MAQROConstants
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return MAQRO_NAMESPACE_URI;}

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return ORDER_DESCRIPTION_CLASS_NAME;}

	/**Default constructor.*/
	public OrderDescription()
	{
	}

	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public OrderDescription(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**Random constructor.
	@param random <code>true</code> if randomness should be indicated, else
		<code>false</code>.
	*/
/*G**del constructor---the ambiguity of this constructor may not be worth its convenience
	public OrderDescription(final boolean random)
	{
		this();	//construct the parent class
		setRandom(random);	//set the randomness valud
	}
*/
	
	/**Determines whether order should be random.
	@return <code>true</code> if the resource is marked as random, else
		<code>false</code> if the resource is marked as not random or does not
		indicate randomness.
	*/
	public boolean isRandom()
	{
		return MAQROUtilities.isRandom(this);	//return whether the order is random
	}

	/**Sets whether order should be random.
	@param random <code>true</code> if randomness should be indicated, else
		<code>false</code>.
	*/
	public void setRandom(final boolean random)
	{
		MAQROUtilities.setRandom(this, random);	//set whether the selection should be random
	}

}
