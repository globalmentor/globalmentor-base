package com.garretwilson.rdf;

import com.globalmentor.net.Resource;

/**Represents a statement in an RDF data model.
@author Garret Wilson
*/
public interface Statement
{

	/**@return The statement subject.*/
	public Resource getSubject();

	/**@return The statement predicate.*/
	public Resource getPredicate();

	/**@return The statement object.*/
	public Object getObject();

}