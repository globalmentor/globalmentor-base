package com.garretwilson.rdf.maqro;

import java.net.URI;
import com.garretwilson.rdf.*;

/**Class representing a MAQRO activity.
@author Garret Wilson
*/
public class Activity extends DefaultRDFResource implements MAQROConstants
{

	/**Default constructor.*/
	public Activity()
	{
	}

	/**Constructs an activity with a reference URI.
	@param referenceURI The reference URI for the new publication.
	*/
	public Activity(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The list of interactions for this activity.
	@exception ClassCastException if the value of the interactions property
		is not a list resource.
	*/
	public RDFListResource getInteractions()
	{
		return (RDFListResource)getPropertyValue(MAQRO_NAMESPACE_URI, INTERACTIONS_PROPERTY_NAME);	//get the maqro:interactions property value
		
	}

}
