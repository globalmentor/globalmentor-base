package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.Iterator;
import java.util.Locale;

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
	public int getCount()
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

	/**Adds a category to the selection.
	@param category The category to add.
	@param language The language of the category, or <code>null</code> if
		no language should be specified.
	*/
	public void addCategory(final String category, final Locale language)
	{
		MAQROUtilities.addCategory(this, category, language);	//add the category to the selection
	}

	/**@return An iterator to categories, if any, of the selection.*/
	public Iterator getCategoryIterator()
	{
		return MAQROUtilities.getCategoryIterator(this);	//return an iterator to the categories
	}
}