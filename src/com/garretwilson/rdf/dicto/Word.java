package com.garretwilson.rdf.dicto;

import java.net.URI;
import com.garretwilson.rdf.*;

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

	/**@return The part of speech of the word, or <code>null</code> if there is
		no part of speech.
	@exception ClassCastException Thrown if the property object is not a plain
		literal.
	*/
	public RDFPlainLiteral getSpeechPart()
	{
		return (RDFPlainLiteral)getPropertyValue(DICTO_NAMESPACE_URI, SPEECH_PART_PROPERTY_NAME);	//get the part of speech		
	}

	/**@return The gender of the word, or <code>null</code> if there is
		no gender.
	@exception ClassCastException Thrown if the property object is not a plain
		literal.
	*/
	public RDFPlainLiteral getGender()
	{
		return (RDFPlainLiteral)getPropertyValue(DICTO_NAMESPACE_URI, GENDER_PROPERTY_NAME);	//get the gender		
	}
}
