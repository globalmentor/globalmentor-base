package com.garretwilson.rdf.dicto;

import java.net.URI;

/**Class representing a word that can be part of a Dictionary Ontology
	(Dicto) dictionary.
@author Garret Wilson
*/
public class Word extends Entry
{

	/**Default constructor.*/
	protected Word()
	{
		super();	//construct the parent class
	}
	
	/**Constructs a word with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	protected Word(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}
}
