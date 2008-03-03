package com.globalmentor.rdf.dicto;

import java.net.URI;
import java.util.Locale;

import com.globalmentor.rdf.*;
import com.globalmentor.rdf.dublincore.DCUtilities;

/**Class representing a Dictionary Ontology (Dicto) dictionary.
@author Garret Wilson
*/
public class Dictionary extends TypedRDFResource implements DictoConstants
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return DICTO_NAMESPACE_URI;}

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return DICTIONARY_CLASS_NAME;}

	/**Default constructor.*/
	public Dictionary()
	{
	}

	/**Constructs an activity with a reference URI.
	@param referenceURI The reference URI for the new publication.
	*/
	public Dictionary(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The list of entries for this dictionary.
	@exception ClassCastException if the value of the interactions property
		is not a list resource.
	*/
	public RDFListResource getEntries()
	{
		return (RDFListResource)getPropertyValue(DICTO_NAMESPACE_URI, ENTRIES_PROPERTY_NAME);	//get the dicto:entries property value		
	}

	/**@return The language of the dictionary entries, or <code>null</code> if
		dictionary language is not indicated.
	*/
	public Locale getDictionaryLanguage()
	{
		return DictoUtilities.getLanguage(this);	//return the dictionary language
	}

	/**@return The language of any translations, or <code>null</code> if
		no translation language is indicated. If there are no translations, this
		property should return the same as <code>getDictionaryLanguage()</code>.
	*/
	public Locale getLanguage()
	{
		return DCUtilities.getLanguage(this);	//return the language
	}

}
