package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.*;
import com.garretwilson.rdf.*;
import com.garretwilson.rdf.xmlschema.IntegerLiteral;

/**Class representing a MAQRO question.
@author Garret Wilson
*/
public class Question extends Interaction
{

	/**Default constructor.*/
	public Question()
	{
		super();	//construct the parent class
	}
	
	/**Constructs a question with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	public Question(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The query part of the question, or <code>null</code> if there is
		no query.
	*/
	public RDFObject getQuery()
	{
		return getPropertyValue(MAQRO_NAMESPACE_URI, QUERY_PROPERTY_NAME);	//get the query		
	}

	/**Replaces the query, if any, with a query that has the given string value.
	@param queryValue The literal value of the query.
	@param language The language of the query value, or <code>null</code> if
		no language should be specified.
	@return The added query resource containing the given value.
	*	*/
	public RDFResource setQueryValue(final String queryValue, final Locale language)
	{
		final RDFResource query=RDFUtilities.locateResource(this, null);	//create an anonymous query
		RDFUtilities.setValue(query, queryValue, language);	//set the query value
		return (RDFResource)setProperty(MAQRO_NAMESPACE_URI, QUERY_PROPERTY_NAME, query);	//store the query
	}

	/**Adds a choice to the question.
	@param choice The choice to add.
	*/
	public void addChoice(final RDFResource choice)
	{
		addProperty(MAQRO_NAMESPACE_URI, CHOICE_PROPERTY_NAME, choice);	//add the choice to the question
	}

	/**@return An iterator to choices, if any, of the question.*/
	public Iterator getChoiceIterator()
	{
		return getPropertyValueIterator(MAQRO_NAMESPACE_URI, CHOICE_PROPERTY_NAME);	//return an iterator to the choices 
	}

	/**@return The resource indicating the datatype expected in the response, or
		<code>null</code> if there is no expected datatype or the expected datatype
		is not a resource.
	*/
	public RDFResource getExpect()
	{
		return RDFUtilities.asResource(getPropertyValue(MAQRO_NAMESPACE_URI, EXPECT_PROPERTY_NAME));	//get the expectation		
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
