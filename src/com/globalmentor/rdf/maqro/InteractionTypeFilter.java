package com.globalmentor.rdf.maqro;

import java.net.URI;

import com.globalmentor.rdf.*;

import static com.globalmentor.rdf.maqro.MAQROConstants.*;

/**Filter for choosing MAQRO interactions based upon interaction type.
@author Garret Wilson
*/
public class InteractionTypeFilter extends Filter
{

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return INTERACTION_TYPE_FILTER_CLASS_NAME;}

	/**Default constructor.*/
	public InteractionTypeFilter()
	{
		super();	//construct the parent class
	}

	/**Constructs selection criteria with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	public InteractionTypeFilter(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

}
