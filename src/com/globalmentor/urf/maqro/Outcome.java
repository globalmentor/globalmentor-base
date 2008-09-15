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

package com.globalmentor.urf.maqro;

import java.net.URI;
import java.util.List;

import static com.globalmentor.java.Objects.*;
import static com.globalmentor.urf.maqro.MAQRO.*;
import com.globalmentor.urf.*;

import static com.globalmentor.urf.URF.*;

/**Represents the outcome, including responses, evaluations, and scores, of a MAQRO interaction such as an activity or question.
@author Garret Wilson
*/
public class Outcome extends AbstractClassTypedURFResource
{

	/**Default constructor.*/
	public Outcome()
	{
		this((URI)null);	//construct the class with no URI
	}

	/**Interaction constructor.
	@param interaction The interaction this outcome represents.
	*/
	public Outcome(final Interaction interaction)
	{
		this();	//construct the parent class
		setInteraction(interaction);	//set the interaction
	}
	
	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	public Outcome(final URI uri)
	{
		super(uri, MAQRO_NAMESPACE_URI);  //construct the parent class
	}

	/**@return The interaction for which this object gives the outcome, or <code>null</code> if there is no interaction or the value is not an interaction.*/
	public Interaction getInteraction()
	{
		return asInstance(getPropertyValue(INTERACTION_PROPERTY_URI), Interaction.class);	//retrieve the interaction
	}

	/**Sets the interaction for which this object gives the outcome.
	@param interaction The interaction this outcome represents.
	 */
	public void setInteraction(final Interaction interaction)
	{
		setPropertyValue(INTERACTION_PROPERTY_URI, interaction);	//set the interaction
	}

	/**@return The list of outcomes for this outcome's contained outcomes,
		or <code>null</code> if there is no list of outcomes or the value is not a list.
	*/
	public List<Outcome> getOutcomes()
	{
		return asListInstance(getPropertyValue(OUTCOMES_PROPERTY_URI));	//get the maqro.outcomes property value as a list	
	}
	
	/**Retrieves the first outcome that represents a particular interaction from the list of contained outcomes.
	@param interaction The interaction for which an outcome should be retrieved.
	@return An outcome containing information for the given interaction, or <code>null</code> if there
		is no outcome the interaction.
	@see #getOutcomes()
	*/
	public Outcome getOutcome(final Interaction interaction)
	{
		for(final Outcome outcome:getOutcomes())	//look at each outcome
		{
			if(interaction.equals(outcome.getInteraction()))	//if this is an outcome specified to be for the given interaction
			{
				return outcome;	//return the matching outcome
			}
		}
		return null;	//show that we couldn't find a matching outcome
	}

	/**Sets the list of outcomes for this interaction's contained interactions.
	@param outcomes The list of outcomes for this interaction group, or
		<code>null</code> if the list of outcomes should be removed.
	*/
	public void setOutcomes(final URFListResource<Outcome> interactions)
	{
		setPropertyValue(OUTCOMES_PROPERTY_URI, interactions);	//set the maqro.outcomes property
	}

	/**Adds a response to the outcome.
	@param response The response to add.
	*/
	public void addResponse(final URFResource response)
	{
		addPropertyValue(RESPONSE_PROPERTY_URI, response);	//add the response to the outcome
	}

	/**@return An iterable to responses, if any, of the outcome.*/
	public Iterable<URFResource> getResponses()
	{
		return getPropertyValues(RESPONSE_PROPERTY_URI);	//return an iterable to the responses 
	}

	/**Adds a result to the outcome.
	@param result The result to add.
	*/
	public void addResult(final Result result)
	{
		addPropertyValue(RESULT_PROPERTY_URI, result);	//add the result to the outcome
	}

	/**@return An iterable to results, if any, of the outcome.*/
	public Iterable<Result> getResults()
	{
		return getPropertyValues(RESULT_PROPERTY_URI, Result.class);	//return an iterable to the results 
	}

	/**Determines whether the outcome is marked as correct or incorrect.
	@return <code>true</code> if the resource is marked as correct or incorrect, else
		<code>false</code> if the resource does not indicate correctness.
	*/
	public boolean hasCorrect()
	{
		return asBoolean(getPropertyValue(CORRECT_PROPERTY_URI)); //see if the correct property is present
	}

	/**Determines whether the outcome is correct.
	@return <code>true</code> if the resource is marked as correct, else
		<code>false</code> if the resource is marked as not correct or does not
		indicate correctness.
	*/
	public boolean isCorrect()
	{
		return Boolean.TRUE.equals(asBoolean(getPropertyValue(CORRECT_PROPERTY_URI))); //see if the correct property is true
	}

	/**Sets whether the outcome is correct.
	@param correct <code>true</code> if the outcome is correct, else
		<code>false</code>.
	*/
	public void setCorrect(final boolean correct)
	{
		setPropertyValue(CORRECT_PROPERTY_URI, correct); //set the correct property with a boolean value
	}

}
