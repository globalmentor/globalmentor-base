package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.*;
import com.garretwilson.rdf.*;
import com.garretwilson.rdf.xmlschema.BooleanLiteral;

/**Represents the outcome, including responses, evaluations, and scores, of a
	MAQRO interaction such as an activity or question.
@author Garret Wilson
*/
public class Outcome extends TypedRDFResource implements MAQROConstants
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return MAQRO_NAMESPACE_URI;}

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return OUTCOME_CLASS_NAME;}

	/**Default constructor.*/
	public Outcome()
	{
		super();	//construct the parent class
	}

	/**Interaction constructor.
	@param interaction The interaction this outcome represents.
	*/
	public Outcome(final Interaction interaction)
	{
		this();	//construct the class
		setInteraction(interaction);	//set the interaction
	}
	
	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public Outcome(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The interaction for which this object gives the outcome.*/
	public Interaction getInteraction()
	{
		return (Interaction)getPropertyValue(MAQRO_NAMESPACE_URI, INTERACTION_PROPERTY_NAME);	//retrieve the interaction
	}

	/**Sets the interaction for which this object gives the outcome.
	@param interaction The interaction this outcome represents.
	 */
	public void setInteraction(final Interaction interaction)
	{
		setProperty(MAQRO_NAMESPACE_URI, INTERACTION_PROPERTY_NAME, interaction);	//set the interaction
	}

	/**@return The list of outcomes for this interaction's contained interactions,
		or <code>null</code> if there is no list of outcomes or the value is not a list.
	*/
	public RDFListResource getOutcomes()
	{
		return RDFUtilities.asListResource(getPropertyValue(MAQRO_NAMESPACE_URI, OUTCOMES_PROPERTY_NAME));	//get the maqro:outcomes property value as a list	
	}

	/**Sets the list of outcomes for this interaction's contained interactions.
	@param outcomes The list of outcomes for this interaction group, or
		<code>null</code> if the list of outcomes should be removed.
	*/
	public void setOutcomes(final RDFListResource interactions)
	{
		setProperty(MAQRO_NAMESPACE_URI, OUTCOMES_PROPERTY_NAME, interactions);	//set the maqro:outcomes property
	}

	/**Adds a response to the outcome.
	@param response The response to add.
	*/
	public void addResponse(final RDFObject response)
	{
		addProperty(MAQRO_NAMESPACE_URI, RESPONSE_PROPERTY_NAME, response);	//add the response to the outcome
	}

	/**@return An iterator to responses, if any, of the outcome.*/
	public Iterator getResponseIterator()
	{
		return getPropertyValueIterator(MAQRO_NAMESPACE_URI, RESPONSE_PROPERTY_NAME);	//return an iterator to the responses 
	}

	/**Adds a result to the outcome.
	@param result The result to add.
	*/
	public void addResult(final Result result)
	{
		addProperty(MAQRO_NAMESPACE_URI, RESULT_PROPERTY_NAME, result);	//add the result to the outcome
	}

	/**@return An iterator to results, if any, of the outcome.*/
	public Iterator getResultIterator()
	{
		return getPropertyValueIterator(MAQRO_NAMESPACE_URI, RESULT_PROPERTY_NAME);	//return an iterator to the results 
	}

	/**Determines whether the outcome is correct.
	@return <code>true</code> if the resource is marked as correct, else
		<code>false</code> if the resource is marked as not correct or does not
		indicate correctness.
	*/
	public boolean isCorrect()
	{
		final RDFObject rdfObject=getPropertyValue(MAQRO_NAMESPACE_URI, CORRECT_PROPERTY_NAME); //get the value of the correct property
		return rdfObject instanceof BooleanLiteral ? ((BooleanLiteral)rdfObject).getBoolean().booleanValue() : false; //return the boolean value of the object, or null if is no such property or the property value is not a boolean typed literal
	}

	/**Sets whether the outcome is correct.
	@param correct <code>true</code> if the outcome is correct, else
		<code>false</code>.
	*/
	public void setCorrect(final boolean correct)
	{
		setProperty(MAQRO_NAMESPACE_URI, CORRECT_PROPERTY_NAME, new BooleanLiteral(correct)); //set the correct property with a boolean typed literal
	}

}
