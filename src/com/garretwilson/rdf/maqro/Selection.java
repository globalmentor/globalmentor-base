package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.*;
import com.garretwilson.rdf.*;
import com.garretwilson.rdf.xmlschema.*;

/**Criteria for selecting MAQRO interactions.
@author Garret Wilson
*/
public class Selection extends DefaultRDFResource implements MAQROConstants
{

	/**Default constructor.*/
	public Selection()
	{
	}

	/**Constructs selection criteria with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	public Selection(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}
	
	/**@return The number of questions to include, or -1 if the question
		count is not indicated.
	*/
	public int getQuestionCount()
	{
		final RDFObject questionCount=getPropertyValue(MAQRO_NAMESPACE_URI, QUESTION_COUNT_PROPERTY_NAME);	//get the question count
		return questionCount instanceof IntegerLiteral ? ((IntegerLiteral)questionCount).getInteger().intValue() : -1;	//return the question count, if there is one
	}

	/**Sets the number of questions to include.
	@param questionCount The number of questions to include
	*/
	public void setQuestionCount(final int questionCount)
	{
		RDFUtilities.setProperty(this, MAQRO_NAMESPACE_URI, QUESTION_COUNT_PROPERTY_NAME, new IntegerLiteral(questionCount));	//set the interaction count
	}

	/**Determines whether the selection criteria is random.
	@return <code>true</code> if the resource is marked as random, else
		<code>false</code> if the resource is marked as not random or does not
		indicate randomness.
	*/
	public boolean isRandom()
	{
		return MAQROUtilities.isRandom(this);	//return whether the selection is random
	}

	/**Sets the random designation of the selection criteria.
	@param random <code>true</code> if randomness should be indicated, else
		<code>false</code>.
	*/
	public void setRandom(final boolean random)
	{
		MAQROUtilities.setRandom(this, random);	//set whether the selection should be random
	}


	/**@return The number of choices to include, or -1 if the choice
		count is not indicated.
	*/
	public int getChoiceCount()
	{
		final RDFObject choiceCount=getPropertyValue(MAQRO_NAMESPACE_URI, CHOICE_COUNT_PROPERTY_NAME);	//get the choice count
		return choiceCount instanceof IntegerLiteral ? ((IntegerLiteral)choiceCount).getInteger().intValue() : -1;	//return the choice count, if there is one
	}

	/**Sets the number of choices to include.
	@param choiceCount The number of choices to include
	*/
	public void setChoiceCount(final int choiceCount)
	{
		RDFUtilities.setProperty(this, MAQRO_NAMESPACE_URI, CHOICE_COUNT_PROPERTY_NAME, new IntegerLiteral(choiceCount));	//set the choice count
	}

	/**Replaces the query, if any, with a query that has the given string value.
	@param queryValue The literal value of the query.
	@param language The language of the query value, or <code>null</code> if
		no language should be specified.
	*/
/*G***fix
	public void setQueryValue(final String queryValue, final Locale language)
	{
		final RDF rdf=new RDF();	//TODO fix the data model
		final RDFResource query=rdf.locateResource(null);	//get an anonymous query
		RDFUtilities.setValue(rdf, query, queryValue, language);	//set the query value
		RDFUtilities.setProperty(rdf, this, MAQRO_NAMESPACE_URI, QUERY_PROPERTY_NAME, query);	//store the query
	}
*/

	/**@return The resource indicating the order of selection, or
		<code>null</code> if order is not indicated or is of an unexpected type.
	*/
/*G**fix
	public RDFResource getOrder()
	{
		final RDFObject order=getPropertyValue(MAQRO_NAMESPACE_URI, ORDER_PROPERTY_NAME);	//get the order
		return order instanceof RDFResource ? (RDFResource)order : null;	//return the order, if there is one
	}
*/

	/**Sets the selection order.
	@param order A resource the reference URI of which indicates the order of selection
	*/
/*G***fix
	public void setOrder(final RDFResource order)
	{
		RDFUtilities.setProperty(this, MAQRO_NAMESPACE_URI, ORDER_PROPERTY_NAME, order);	//set the order
	}
*/

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
