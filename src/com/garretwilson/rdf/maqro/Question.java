package com.garretwilson.rdf.maqro;

import java.net.URI;
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
}
