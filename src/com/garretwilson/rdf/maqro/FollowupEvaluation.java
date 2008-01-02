package com.garretwilson.rdf.maqro;

import java.net.URI;


import static com.garretwilson.rdf.maqro.MAQROConstants.*;
import static com.globalmentor.java.Objects.*;

/**Class representing an evaluation to determine a followup.
@author Garret Wilson
*/
public class FollowupEvaluation extends Evaluation
{

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return FOLLOWUP_EVALUATION_CLASS_NAME;}

	/**Default constructor.*/
	public FollowupEvaluation()
	{
	}

	/**Constructs an activity with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	public FollowupEvaluation(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The followup interaction indicated by this evaluation, or <code>null</code> if no followup interaction is indicated.*/
	public Interaction getFollowup()
	{
		return asInstance(getPropertyValue(MAQRO_NAMESPACE_URI, FOLLOWUP_PROPERTY_NAME), Interaction.class);	//retrieve the followup interaction, if one is present
	}

	/**Sets the followup for this evaluation.
	@param followup The followup interaction for this evaluation.
	 */
	public void setFollowup(final Interaction followup)
	{
		setProperty(MAQRO_NAMESPACE_URI, FOLLOWUP_PROPERTY_NAME, followup);	//set the followup
	}

}
