package com.globalmentor.rdf.dicto;

import java.net.URI;

import com.globalmentor.rdf.*;

/**Class representing an entry that can be part of a Dictionary Ontology
	(Dicto) dictionary.
@author Garret Wilson
*/
public abstract class Entry extends TypedRDFResource implements DictoConstants
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

	/**@return The definition of the entry, or <code>null</code> if there is
		no definition.
	@exception ClassCastException Thrown if the property object is not a plain
		literal.
	*/
	public RDFPlainLiteral getDefinition()
	{
		return (RDFPlainLiteral)getPropertyValue(DICTO_NAMESPACE_URI, DEFINITION_PROPERTY_NAME);	//get the definition		
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

	/**@return The pronunciation of the entry, or <code>null</code> if there is
		no pronunciation.
	*/
	public RDFObject getPronunciation()
	{
		return getPropertyValue(DICTO_NAMESPACE_URI, PRONUNCIATION_PROPERTY_NAME);	//get the pronunciation		
	}

	/**@return An iterable to pronunciations, if any, of the entry.*/
	public Iterable<RDFObject> getPronunciations()
	{
		return getPropertyValues(DICTO_NAMESPACE_URI, PRONUNCIATION_PROPERTY_NAME); //return an iterable to the pronunciation properties
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

	/**@return The transliteration of the entry, or <code>null</code> if there is
		no transliteration.
	@exception ClassCastException Thrown if the property object is not a plain
		literal.
	*/
	public RDFPlainLiteral getTransliteration()
	{
		return (RDFPlainLiteral)getPropertyValue(DICTO_NAMESPACE_URI, TRANSLITERATION_PROPERTY_NAME);	//get the transliteration		
	}

}
