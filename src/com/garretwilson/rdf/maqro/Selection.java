package com.garretwilson.rdf.maqro;

import java.net.URI;
import com.garretwilson.rdf.*;

/**Criteria for selecting MAQRO interactions.
@author Garret Wilson
*/
public abstract class Selection extends TypedRDFResource implements MAQROConstants
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return MAQRO_NAMESPACE_URI;}

	/**Default constructor.*/
	public Selection()
	{
		super();	//construct the parent class
	}

	/**Constructs selection criteria with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	public Selection(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The number of choices to include, or -1 if the choice
		count is not indicated.
	*/
/*G***fix or del
	public int getChoiceCount()
	{
		final RDFObject choiceCount=getPropertyValue(MAQRO_NAMESPACE_URI, CHOICE_COUNT_PROPERTY_NAME);	//get the choice count
		return choiceCount instanceof IntegerLiteral ? ((IntegerLiteral)choiceCount).getInteger().intValue() : -1;	//return the choice count, if there is one
	}
*/

	/**Sets the number of choices to include.
	@param choiceCount The number of choices to include
	*/
/*G***fix or del
	public void setChoiceCount(final int choiceCount)
	{
		setProperty(MAQRO_NAMESPACE_URI, CHOICE_COUNT_PROPERTY_NAME, new IntegerLiteral(choiceCount));	//set the choice count
	}
*/

	/**@return The list of selectors for this selection, or <code>null</code>
		if there is no list of selectors or the value is not a list.
	*/
	public RDFListResource getSelectors()
	{
		return RDFUtilities.asListResource(getPropertyValue(MAQRO_NAMESPACE_URI, SELECTORS_PROPERTY_NAME));	//get the maqro:selectors property value as a list	
	}

	/**Sets the list of selectors.
	@param selectors The list of selectors.
	*/
	public void setSelectors(final RDFListResource selectors)
	{
		setProperty(MAQRO_NAMESPACE_URI, SELECTORS_PROPERTY_NAME, selectors);	//set the selectors
	}

	/**@return The order criteria, or <code>null</code> if there is no
		order criteria indicated or if it is not of the correct type.
	*/
	public Order getOrder()
	{
		final RDFObject order=getPropertyValue(MAQRO_NAMESPACE_URI, ORDER_PROPERTY_NAME);	//get the maqro:order property value
		return order instanceof Order ? (Order)order : null;	//return the order criteria if there are any
	}

	/**Sets the order criteria for the activity.
	@param orderDescription The order criteria.
	*/
	public void setOrder(final Order orderDescription)
	{
		setProperty(MAQRO_NAMESPACE_URI, ORDER_PROPERTY_NAME, orderDescription);	//set the order criteria
	}
	
}
