package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.Iterator;
import java.util.Locale;

import com.garretwilson.rdf.RDFListResource;
import com.garretwilson.rdf.RDFUtilities;
import com.garretwilson.rdf.TypedRDFResource;
import com.garretwilson.rdf.xmlschema.IntegerLiteral;

/**A selector of MAQRO group selection criteria.
@author Garret Wilson
*/
public class Selector extends TypedRDFResource implements MAQROConstants
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return MAQRO_NAMESPACE_URI;}

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return SELECTOR_CLASS_NAME;}

	/**Default constructor.*/
	public Selector()
	{
		super();	//construct the parent class
	}

	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public Selector(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The number of interactions to include, or -1 if the question
		count is not indicated.
	*/
	public int getCount()	//TODO do we want this to be a long?
	{
		return IntegerLiteral.asIntValue(getPropertyValue(MAQRO_NAMESPACE_URI, COUNT_PROPERTY_NAME));	//get the integer question count
	}

	/**Sets the number of interactions to include.
	@param count The number of questions to include.
	*/
	public void setCount(final int questionCount)
	{
		setProperty(MAQRO_NAMESPACE_URI, COUNT_PROPERTY_NAME, new IntegerLiteral(questionCount));	//set the interaction count
	}

	/**@return The list of filters for this selection, or <code>null</code>
		if there is no list of filters or the value is not a list.
	*/
	public RDFListResource getFilters()
	{
		return RDFUtilities.asListResource(getPropertyValue(MAQRO_NAMESPACE_URI, FILTERS_PROPERTY_NAME));	//get the maqro:filters property value as a list	
	}

	/**Sets the list of filters.
	@param selectors The list of filters.
	*/
	public void setFilters(final RDFListResource selectors)
	{
		setProperty(MAQRO_NAMESPACE_URI, FILTERS_PROPERTY_NAME, selectors);	//set the filters
	}

}