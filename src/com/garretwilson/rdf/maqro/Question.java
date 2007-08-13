package com.garretwilson.rdf.maqro;

import java.net.URI;

import com.garretwilson.lang.ObjectUtilities;
import com.garretwilson.rdf.*;

import static com.garretwilson.rdf.RDFUtilities.asListResource;
import static com.garretwilson.rdf.maqro.MAQROConstants.*;

import com.garretwilson.rdf.xmlschema.IntegerLiteral;

/**Class representing a MAQRO question.
@author Garret Wilson
*/
public class Question extends Interaction
{

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return QUESTION_CLASS_NAME;}

	/**Default constructor.*/
	public Question()
	{
		super();	//construct the parent class
	}
	
	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public Question(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The query part of the question, or <code>null</code> if there is
		no query.
	*/
	public Dialogue getQuery()
	{
		return ObjectUtilities.asInstance(getPropertyValue(MAQRO_NAMESPACE_URI, QUERY_PROPERTY_NAME), Dialogue.class);	//get the query only if it is Dialogue		
	}

	/**Sets the query of the question.
	@param query The query part of the question, or <code>null</code> if there
		should be no query.
	*/
	public void setQuery(final Dialogue query)
	{
		this.setProperty(MAQRO_NAMESPACE_URI, QUERY_PROPERTY_NAME, query);	//set the query		
	}

	/**@return The list of choices for this question, or <code>null</code>
		if there is no list of choices or the value is not a list.
	*/
	public RDFListResource getChoices()
	{
		return RDFUtilities.asListResource(getPropertyValue(MAQRO_NAMESPACE_URI, CHOICES_PROPERTY_NAME));	//get the maqro:choices property value as a list	
	}

	/**Sets the list of question choices.
	@param choices The list of question choices
	*/
	public void setChoices(final RDFListResource choices)
	{
		setProperty(MAQRO_NAMESPACE_URI, CHOICES_PROPERTY_NAME, choices);	//set the choices
	}

	/**@return The <code>maqro:evaluations</code> list of evaluations for this question, or <code>null</code> if there is no list of evaluations or the value is not a list.*/
	public RDFListResource<?> getEvaluations()
	{
		return asListResource(getPropertyValue(MAQRO_NAMESPACE_URI, EVALUATIONS_PROPERTY_NAME));	//get the maqro:evaluations property value as a list	
	}

	/**Sets the <code>maqro:evaluations</code> list of evaluations for this question.
	@param evaluations The list of evaluations for this question, or <code>null</code> if the list of evaluations should be removed.
	*/
	public void setEvaluations(final RDFListResource evaluations)
	{
		setProperty(MAQRO_NAMESPACE_URI, EVALUATIONS_PROPERTY_NAME, evaluations);	//set the maqro:evaluations property
	}
	
	/**@return The resource indicating the datatype expected in the response, or
		<code>null</code> if there is no expected datatype or the expected datatype
		is not a resource.
	*/
	public RDFResource getExpectation()
	{
		return RDFUtilities.asResource(getPropertyValue(MAQRO_NAMESPACE_URI, EXPECTATION_PROPERTY_NAME));	//get the expectation		
	}

	/**Sets the expectation of the question.
	@param expectation The resource indicating the datatype expected in the
		response, or <code>null</code> if there is no expected datatype.
	*/
	public void setExpectation(final RDFResource expectation)
	{
		this.setProperty(MAQRO_NAMESPACE_URI, EXPECTATION_PROPERTY_NAME, expectation);	//set the expectation		
	}

	/**Adds an explanation to the question.
	@param explanation An explanation of the question.
	*/
	public void addExplanation(final Dialogue explanation)
	{
		addProperty(MAQRO_NAMESPACE_URI, EXPLANATION_PROPERTY_NAME, explanation);	//add the explanation
	}

	/**@return An iterable to the explanations, if any, of the question.*/
	public Iterable<RDFObject> getExplanations()
	{
		return getPropertyValues(MAQRO_NAMESPACE_URI, EXPLANATION_PROPERTY_NAME);	//return an iterable to the explanations 
	}

	/**Removes all answers from the question.*/
	public void removeExplanations()
	{
		removeProperties(MAQRO_NAMESPACE_URI, EXPLANATION_PROPERTY_NAME);	//remove all explanations
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
	
	/**@return The list of hints for this question, or <code>null</code>
		if there is no list of hints or the value is not a list.
	*/
	public RDFListResource getHints()
	{
		return RDFUtilities.asListResource(getPropertyValue(MAQRO_NAMESPACE_URI, HINTS_PROPERTY_NAME));	//get the maqro:hints property value as a list	
	}

	/**Sets the list of question hints.
	@param hints The list of question hints.
	*/
	public void setHints(final RDFListResource hints)
	{
		setProperty(MAQRO_NAMESPACE_URI, HINTS_PROPERTY_NAME, hints);	//set the hints
	}

	/**Adds an answer dialogue to the question.
	@param answer A correct answer to the question.
	*/
	public void addAnswer(final Dialogue answer)
	{
		addProperty(MAQRO_NAMESPACE_URI, ANSWER_PROPERTY_NAME, answer);	//add the correct answer
	}

	/**Adds an answer list to the question.
	@param answerList A correct answer to the question.
	*/
	public void addAnswer(final RDFListResource answer)
	{
		addProperty(MAQRO_NAMESPACE_URI, ANSWER_PROPERTY_NAME, answer);	//add the correct answer
	}

	/**@return An iterable to the answers, if any, of the question.*/
	public Iterable<RDFObject> getAnswers()
	{
		return getPropertyValues(MAQRO_NAMESPACE_URI, ANSWER_PROPERTY_NAME);	//return an iterable to the answers 
	}

	/**Removes all answers from the question.*/
	public void removeAnswers()
	{
		removeProperties(MAQRO_NAMESPACE_URI, ANSWER_PROPERTY_NAME);	//remove all answers
	}

	/**@return The maximum number of responses to accept, or -1 if the maximum
		response count is not indicated and should be infinite.
	*/
	public int getMaxResponseCount()
	{
		final RDFObject maxResponseCount=getPropertyValue(MAQRO_NAMESPACE_URI, MAX_RESPONSE_COUNT_PROPERTY_NAME);	//get the value
		return maxResponseCount instanceof IntegerLiteral ? ((IntegerLiteral)maxResponseCount).getValue().intValue() : -1;	//return the integer value, if there is one
	}

	/**Sets the maximum number of responses to accept.
	@param count The maximum number of responses, or -1 if the maximum response
		count should be infinite.
	*/
	public void setMaxResponseCount(final int count)
	{
		setProperty(MAQRO_NAMESPACE_URI, MAX_RESPONSE_COUNT_PROPERTY_NAME, count>=0 ? new IntegerLiteral(count) : null);	//set the value or remove it
	}

	/**Returns a string representation of the resource.
	<p>This implementation returns the question query, if there is a query;
		otherwise, the default string value is returned.</p> 
	@return A string representation of the resource.
	*/
	public String toString()
	{
		final Dialogue query=getQuery();	//get the query, if there is one
		return query!=null ? query.toString() : super.toString();	//return the string value of the query, if there is one; otherwise return the default string representation 
	}
	
}
