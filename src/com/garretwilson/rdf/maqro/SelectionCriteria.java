package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.*;
import com.garretwilson.rdf.*;
import com.garretwilson.rdf.xmlschema.IntegerLiteral;

/**Criteria for selecting MAQRO interactions.
@author Garret Wilson
*/
public class SelectionCriteria extends DefaultRDFResource implements MAQROConstants
{

	/**Default constructor.*/
	public SelectionCriteria()
	{
	}

	/**Constructs selection criteria with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	public SelectionCriteria(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}
	
	/**@return The number of interactions to include, or -1 if the interaction
		count is not indicated.
	*/
	public int getInteractionCount()
	{
		final RDFObject interactionCount=getPropertyValue(MAQRO_NAMESPACE_URI, INTERACTION_COUNT_PROPERTY_NAME);	//get the interaction count
		return interactionCount instanceof IntegerLiteral ? ((IntegerLiteral)interactionCount).getInteger().intValue() : -1;	//return the choice count, if there is one
	}

	/**Sets the number of interactions to include.
	@param interactionCount The number of interactions to include
	*/
	public void setInteractionCount(final int interactionCount)
	{
		RDFUtilities.setProperty(this, MAQRO_NAMESPACE_URI, INTERACTION_COUNT_PROPERTY_NAME, new IntegerLiteral(interactionCount));	//set the interaction count
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

	/**Adds a choice to the question.
	@param choice The choice to add.
	*/
	public void addChoice(final RDFResource choice)
	{
		RDFUtilities.addProperty(this, MAQRO_NAMESPACE_URI, CHOICE_PROPERTY_NAME, choice);	//add the choice to the question
	}

	/**@return An iterator to choices, if any, of the question.*/
	public Iterator getChoiceIterator()
	{
		return getPropertyValueIterator(MAQRO_NAMESPACE_URI, CHOICE_PROPERTY_NAME);	//return an iterator to the choices 
	}

	/**@return The resource indicating the order of selection, or
		<code>null</code> if order is not indicated or is of an unexpected type.
	*/
	public RDFResource getOrder()
	{
		final RDFObject order=getPropertyValue(MAQRO_NAMESPACE_URI, ORDER_PROPERTY_NAME);	//get the order
		return order instanceof RDFResource ? (RDFResource)order : null;	//return the order, if there is one
	}

	/**Sets the selection order.
	@param order A resource the reference URI of which indicates the order of selection
	*/
	public void setOrder(final RDFResource order)
	{
		RDFUtilities.setProperty(this, MAQRO_NAMESPACE_URI, ORDER_PROPERTY_NAME, order);	//set the order
	}
	
}
