package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.*;
import com.garretwilson.rdf.*;

/**Class representing a MAQRO question.
@author Garret Wilson
*/
public class Question extends DefaultRDFResource implements MAQROConstants, Interaction
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
	*/
	public void setQueryValue(final String queryValue, final Locale language)
	{
		final RDF rdf=new RDF();	//TODO fix the data model
		final RDFResource query=rdf.locateResource(null);	//get an anonymous query
		RDFUtilities.setValue(rdf, query, queryValue, language);	//set the query value
		RDFUtilities.setProperty(rdf, this, MAQRO_NAMESPACE_URI, QUERY_PROPERTY_NAME, query);	//store the query
	}

	/**Adds a choice to the question.
	@param choice The choice to add.
	*/
	public void addChoice(final RDFResource choice)
	{
		final RDF rdf=new RDF();	//TODO fix the data model
		RDFUtilities.addProperty(rdf, this, MAQRO_NAMESPACE_URI, CHOICE_PROPERTY_NAME, choice);	//add the choice to the question
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
	
}
