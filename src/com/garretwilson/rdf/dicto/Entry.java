package com.garretwilson.rdf.dicto;

import java.net.URI;
import com.garretwilson.rdf.*;

/**Class representing an entry that can be part of a Dictionary Ontology
	(Dicto) dictionary.
@author Garret Wilson
*/
public class Entry extends DefaultRDFResource implements DictoConstants
{

	/**Default constructor.*/
	protected Entry()
	{
	}
	
	/**Constructs an entry with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	protected Entry(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The orthography of the entry, or <code>null</code> if there is
		no orthography.
	@exception ClassCastException Thrown if the property object is not a plain
		literal.
	*/
	public RDFPlainLiteral getOrthography()
	{
		return (RDFPlainLiteral)getPropertyValue(DICTO_NAMESPACE_URI, ORTHOGRAPHY_PROPERTY_NAME);	//get the orthography		
	}

	/**@return The translation of the entry, or <code>null</code> if there is
		no translation.
	@exception ClassCastException Thrown if the property object is not a plain
		literal.
	*/
	public RDFPlainLiteral getTranslation()
	{
		return (RDFPlainLiteral)getPropertyValue(DICTO_NAMESPACE_URI, TRANSLATION_PROPERTY_NAME);	//get the translation		
	}

}
