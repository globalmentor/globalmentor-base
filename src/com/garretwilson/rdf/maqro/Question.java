package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.Iterator;
import com.garretwilson.rdf.*;

/**Class representing a MAQRO question.
@author Garret Wilson
*/
public class Question extends DefaultRDFResource implements MAQROConstants, Interaction
{

	/**Default constructor.*/
	protected Question()
	{
	}
	
	/**Constructs a question with a reference URI.
	@param referenceURI The reference URI for the new publication.
	*/
	protected Question(final URI referenceURI)
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

	/**@return An iterator to choices, if any, of the question.*/
	public Iterator getChoiceIterator()
	{
		return getPropertyValueIterator(MAQRO_NAMESPACE_URI, CHOICE_PROPERTY_NAME);	//return an iterator to the choices 
	}
}
