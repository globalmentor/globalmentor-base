package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.*;

import com.garretwilson.rdf.RDFLiteral;

/**Filter for choosing MAQRO interactions based upon category.
@author Garret Wilson
*/
public class CategoryFilter extends Filter implements MAQROConstants
{

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return CATEGORY_FILTER_CLASS_NAME;}

	/**Default constructor.*/
	public CategoryFilter()
	{
		super();	//construct the parent class
	}

	/**Constructs selection criteria with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	public CategoryFilter(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**Adds a category to the interaction.
	@param categoryLiteral A literal category value.
	*/
	public void addCategory(final RDFLiteral categoryLiteral)
	{
		MAQROUtilities.addCategory(this, categoryLiteral);	//add the category to the filter
	}

	/**Adds a category to the filter.
	@param category The category to add.
	@param language The language of the category, or <code>null</code> if
		no language should be specified.
	*/
	public void addCategory(final String category, final Locale language)
	{
		MAQROUtilities.addCategory(this, category, language);	//add the category to the filter
	}

	/**@return An iterator to categories, if any, of the interaction.*/
	public Iterator getCategoryIterator()
	{
		return MAQROUtilities.getCategoryIterator(this);	//return an iterator to the categories
	}

}