package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.*;
import com.garretwilson.rdf.*;
import com.garretwilson.rdf.xmlschema.*;

/**Criteria for selecting MAQRO interactions.
@author Garret Wilson
*/
public class SelectDescription extends DefaultRDFResource implements MAQROConstants
{

	/**Default constructor.*/
	public SelectDescription()
	{
	}

	/**Constructs selection criteria with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	public SelectDescription(final URI referenceURI)
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
	@param questionCount The number of questions to include.
	*/
	public void setQuestionCount(final int questionCount)
	{
		setProperty(MAQRO_NAMESPACE_URI, QUESTION_COUNT_PROPERTY_NAME, new IntegerLiteral(questionCount));	//set the interaction count
	}

	/**Determines whether selection should be random.
	@return <code>true</code> if the resource is marked as random, else
		<code>false</code> if the resource is marked as not random or does not
		indicate randomness.
	*/
	public boolean isRandom()
	{
		return MAQROUtilities.isRandom(this);	//return whether the selection is random
	}

	/**Sets whether selection should be random.
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
		setProperty(MAQRO_NAMESPACE_URI, CHOICE_COUNT_PROPERTY_NAME, new IntegerLiteral(choiceCount));	//set the choice count
	}

	/**@return The order criteria, or <code>null</code> if there is no
		order criteria indicated or if it is not of the correct type.
	*/
	public OrderDescription getOrder()
	{
		final RDFObject order=getPropertyValue(MAQRO_NAMESPACE_URI, ORDER_PROPERTY_NAME);	//get the maqro:order property value
		return order instanceof OrderDescription ? (OrderDescription)order : null;	//return the order criteria if there are any
	}

	/**Sets the order criteria for the activity.
	@param orderDescription The order criteria.
	*/
	public void setOrder(final OrderDescription orderDescription)
	{
		setProperty(MAQRO_NAMESPACE_URI, ORDER_PROPERTY_NAME, orderDescription);	//set the order criteria
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
