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

import static com.globalmentor.java.Objects.*;
import com.globalmentor.urf.*;
import static com.globalmentor.urf.URF.*;
import com.globalmentor.urf.content.Text;
import static com.globalmentor.urf.maqro.MAQRO.*;

/**Class representing a MAQRO question.
@author Garret Wilson
*/
public class Question extends AbstractInteraction
{

	/**Default constructor.*/
	public Question()
	{
		this(null);	//construct the class with no URI
	}
	
	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	public Question(final URI uri)
	{
		super(uri);  //construct the parent class
	}

	/**@return The query part of the question, or <code>null</code> if there is
		no query or the query is not text.
	*/
	public Text getQuery()
	{
		return asInstance(getPropertyValue(QUERY_PROPERTY_URI), Text.class);	//get the query only if it is Text		
	}

	/**Sets the query of the question.
	@param query The query part of the question, or <code>null</code> if there
		should be no query.
	*/
	public void setQuery(final Text query)
	{
		setPropertyValue(QUERY_PROPERTY_URI, query);	//set the query		
	}

	/**@return An iteratoble to the choices for this question.*/
	public Iterable<Text> getChoices()
	{
		return getPropertyValues(CHOICE_PROPERTY_URI, Text.class);	//get the maqro.choices property value if they are Text resources	
	}

	/**Adds a choice to the question.
	@param choice The choice to add.
	*/
	public void addChoice(final Text choice)
	{
		addPropertyValue(CHOICE_PROPERTY_URI, choice);	//add the choice
	}

	/**Adds ordered choices to the question.
	All existing choices will be replaced.
	@param choices The question choices.
	*/
	public void setChoices(final Text... choices)
	{
		setPropertyValues(CHOICE_PROPERTY_URI, choices);	//set the choices
	}

	/**@return The <code>maqro:evaluations</code> list of evaluations for this question, or <code>null</code> if there is no list of evaluations or the value is not a list.*/
/*TODO fix
	public RDFListResource<?> getEvaluations()
	{
		return asListResource(getPropertyValue(MAQRO_NAMESPACE_URI, EVALUATIONS_PROPERTY_NAME));	//get the maqro:evaluations property value as a list	
	}
*/

	/**Sets the <code>maqro:evaluations</code> list of evaluations for this question.
	@param evaluations The list of evaluations for this question, or <code>null</code> if the list of evaluations should be removed.
	*/
/*TODO fix
	public void setEvaluations(final RDFListResource evaluations)
	{
		setProperty(MAQRO_NAMESPACE_URI, EVALUATIONS_PROPERTY_NAME, evaluations);	//set the maqro:evaluations property
	}
*/	
	
	/**@return The resource indicating the type expected in the response, or
		<code>null</code> if there is no expected type.
	*/
	public URFResource getExpectation()
	{
		return getPropertyValue(EXPECTATION_PROPERTY_URI);	//get the expectation		
	}

	/**Sets the expectation of the question.
	@param expectation The resource indicating the type expected in the
		response, or <code>null</code> if there is no expected type.
	*/
	public void setExpectation(final URFResource expectation)
	{
		setPropertyValue(EXPECTATION_PROPERTY_URI, expectation);	//set the expectation		
	}

	/**Adds an explanation to the question.
	@param explanation An explanation of the question.
	*/
	public void addExplanation(final Text explanation)
	{
		addPropertyValue(EXPLANATION_PROPERTY_URI, explanation);	//add the explanation
	}

	/**@return An iterable to the explanations, if any, of the question.*/
	public Iterable<Text> getExplanations()
	{
		return getPropertyValues(EXPLANATION_PROPERTY_URI, Text.class);	//return an iterable to the explanations 
	}

	/**Removes all answers from the question.*/
	public void removeExplanations()
	{
		removePropertyValues(EXPLANATION_PROPERTY_URI);	//remove all explanations
	}

	/**@return The <code>maqro:followups</code> list of followups for this question, or <code>null</code> if there is no list of followups or the value is not a list.*/
/*TODO del when works
	public RDFListResource getFollowups()
	{
		return asListResource(getPropertyValue(MAQRO_NAMESPACE_URI, FOLLOWUPS_PROPERTY_NAME));	//get the maqro:followups property value as a list	
	}
*/

	/**Sets the <code>maqro:followups</code> list of followups for this question.
	@param followups The list of followups for this question, or <code>null</code> if the list of followups should be removed.
	*/
/*TODO del when works
	public void setFollowups(final RDFListResource followups)
	{
		setProperty(MAQRO_NAMESPACE_URI, FOLLOWUPS_PROPERTY_NAME, followups);	//set the maqro:followups property
	}
*/
	
	/**@return An iterable to the hints for this question.*/
	public Iterable<Text> getHints()
	{
		return getPropertyValues(HINT_PROPERTY_URI, Text.class);	//get the maqro.hint property values	
	}

	/**Adds a hint to the question.
	@param hint The hint to add.
	*/
	public void addHint(final Text hint)
	{
		addPropertyValue(HINT_PROPERTY_URI, hint);	//add the hint
	}

	/**Adds ordered hints to the question.
	All existing hints will be replaced.
	@param hints The question hints.
	*/
	public void setHints(final Text... hints)
	{
		setPropertyValues(HINT_PROPERTY_URI, hints);	//set the hints
	}

	/**Adds answer text to the question.
	@param answer A correct answer to the question.
	*/
	public void addAnswer(final Text answer)
	{
		addPropertyValue(ANSWER_PROPERTY_URI, answer);	//add the correct answer
	}

	/**Adds an answer list to the question.
	@param answerList A correct answer to the question.
	*/
/*TODO del
	public void addAnswer(final RDFListResource answer)
	{
		addProperty(MAQRO_NAMESPACE_URI, ANSWER_PROPERTY_NAME, answer);	//add the correct answer
	}
*/

	/**@return An iterable to the answers, if any, of the question.*/
	public Iterable<Text> getAnswers()
	{
		return getPropertyValues(ANSWER_PROPERTY_URI, Text.class);	//return an iterable to the answers 
	}

	/**Removes all answers from the question.*/
	public void removeAnswers()
	{
		removePropertyValues(ANSWER_PROPERTY_URI);	//remove all answers
	}

	/**@return The maximum number of responses to accept, <code>null</code> if the maximum response count is not indicated or not an integer.*/
	public Long getMaxResponseCount()
	{
		return asInteger(getPropertyValue(MAX_RESPONSE_COUNT_URI));
	}

	/**Sets the maximum number of responses to accept.
	@param count The maximum number of responses, or <code>null</code> if the maximum response count should be infinite.
	*/
	public void setMaxResponseCount(final Long count)
	{
		setPropertyValue(MAX_RESPONSE_COUNT_URI, count);
	}

	/**Determines a string value to use for representation.
	This method may take into account the current properties of the resource in order to provide the best possible string representation.
	This implementation determines the label in the following sequence:
	<ol>
		<li>The determined label of any «{@value MAQRO#QUERY_PROPERTY_URI}» property.</li>
		<li>Whatever label is determined by the parent class.</li>
	</ol>
	@return A string label to use for representation of the resource.
	@see #getQuery()
	*/
	public String determineLabel()
	{
		final Text query=getQuery();	//get the query, if there is one
		return query!=null ? query.determineLabel() : super.determineLabel();	//determine the label of the query, if there is one; otherwise determine the default label
	}
	
}
