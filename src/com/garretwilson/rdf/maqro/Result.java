package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.*;
import com.garretwilson.rdf.*;

/**Represents the result, including responses, evaluations, and scores, of a
	MAQRO interaction such as an activity or question.
@author Garret Wilson
*/
public class Result extends DefaultRDFResource implements MAQROConstants
{

	/**Default constructor.*/
	public Result()
	{
		super();	//construct the parent class
	}

	/**Interaction constructor.
	@param interaction The interaction this result represents.
	*/
	public Result(final Interaction interaction)
	{
		this();	//construct the class
		setInteraction(interaction);	//set the interaction
	}
	
	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public Result(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The interaction for which this object gives the result.*/
	public Interaction getInteraction()
	{
		return (Interaction)getPropertyValue(MAQRO_NAMESPACE_URI, INTERACTION_PROPERTY_NAME);	//retrieve the interaction
	}

	/**Sets the interaction for which this object gives the result.
	@param interaction The interaction this result represents.
	 */
	public void setInteraction(final Interaction interaction)
	{
		setProperty(MAQRO_NAMESPACE_URI, INTERACTION_PROPERTY_NAME, interaction);	//set the interaction
	}

	/**Adds a response to the result.
	@param response The response to add.
	*/
	public void addResponse(final RDFObject response)
	{
		addProperty(MAQRO_NAMESPACE_URI, RESPONSE_PROPERTY_NAME, response);	//add the response to the result
	}

	/**@return An iterator to responses, if any, of the result.*/
	public Iterator getResponseIterator()
	{
		return getPropertyValueIterator(MAQRO_NAMESPACE_URI, RESPONSE_PROPERTY_NAME);	//return an iterator to the responses 
	}

}
