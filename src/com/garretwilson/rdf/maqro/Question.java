package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.*;
import com.garretwilson.lang.ObjectUtilities;
import com.garretwilson.rdf.*;
import com.garretwilson.rdf.xmlschema.IntegerLiteral;

/**Class representing a MAQRO question.
@author Garret Wilson
*/
public class Question extends Interaction
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return MAQRO_NAMESPACE_URI;}

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
		return (Dialogue)ObjectUtilities.asInstance(getPropertyValue(MAQRO_NAMESPACE_URI, QUERY_PROPERTY_NAME), Dialogue.class);	//get the query only if it is Dialogue		
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

	/**@return The resource indicating the datatype expected in the response, or
		<code>null</code> if there is no expected datatype or the expected datatype
		is not a resource.
	*/
	public RDFResource getExpect()
	{
		return RDFUtilities.asResource(getPropertyValue(MAQRO_NAMESPACE_URI, EXPECT_PROPERTY_NAME));	//get the expectation		
	}

	/**Sets the expectation of the question.
	@param expect The resource indicating the datatype expected in the response,
		or <code>null</code> if there is no expected datatype.
	*/
	public void setExpect(final RDFResource expect)
	{
		this.setProperty(MAQRO_NAMESPACE_URI, EXPECT_PROPERTY_NAME, expect);	//set the expectation		
	}

	/**Adds an answer to the question.
	@param answer A correct answer to the question.
	*/
	public void addAnswer(final RDFObject answer)
	{
		addProperty(MAQRO_NAMESPACE_URI, ANSWER_PROPERTY_NAME, answer);	//add the correct answer
	}

	/**@return An iterator to the answers, if any, of the question.*/
	public Iterator getAnswerIterator()
	{
		return getPropertyValueIterator(MAQRO_NAMESPACE_URI, ANSWER_PROPERTY_NAME);	//return an iterator to the answers 
	}

	/**Removes all answers from the question.*/
	public void removeAnswers()
	{
		removeProperties(MAQRO_NAMESPACE_URI, ANSWER_PROPERTY_NAME);	//remove all answers
	}

	/**@return The maximum number of responses to accept, or -1 if the maximum
		response count is not indicated.
	*/
	public int getMaxResponseCount()
	{
		final RDFObject maxResponseCount=getPropertyValue(MAQRO_NAMESPACE_URI, MAX_RESPONSE_COUNT_PROPERTY_NAME);	//get the value
		return maxResponseCount instanceof IntegerLiteral ? ((IntegerLiteral)maxResponseCount).getInteger().intValue() : -1;	//return the integer value, if there is one
	}

	/**Sets the maximum number of responses to accept.
	@param count The maximum number of responses.
	*/
	public void setMaxResponseCount(final int count)
	{
		setProperty(MAQRO_NAMESPACE_URI, MAX_RESPONSE_COUNT_PROPERTY_NAME, new IntegerLiteral(count));	//set the value
	}
	
}
