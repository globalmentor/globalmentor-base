package com.garretwilson.rdf.dicto;

import java.net.URI;
import com.garretwilson.rdf.*;

/**Class representing a Dictionary Ontology (Dicto) dictionary.
@author Garret Wilson
*/
public class Dictionary extends DefaultRDFResource implements DictoConstants
{

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

}
