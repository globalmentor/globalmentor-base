package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.List;

import com.garretwilson.rdf.*;

/**Class representing a MAQRO activity.
	The activity, besides containing interactions, is an interaction itself.
@author Garret Wilson
*/
public class Activity extends Interaction
{

	/**Default constructor.*/
	public Activity()
	{
	}

	/**Constructs an activity with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	public Activity(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The selection criteria, or <code>null</code> if there is no
		selection criteria indicated or if it is not of the correct type.
	*/
	public Selection getSelect()
	{
		final RDFObject selectionCriteria=getPropertyValue(MAQRO_NAMESPACE_URI, SELECT_PROPERTY_NAME);	//get the maqro:selection property value
		return selectionCriteria instanceof Selection ? (Selection)selectionCriteria : null;	//return the selection criteria if there are any
	}

	/**Sets the selection criteria for the activity.
	@param selectionCriteria The selection criteria
	*/
	public void setSelect(final Selection selectionCriteria)
	{
		RDFUtilities.setProperty(this, MAQRO_NAMESPACE_URI, SELECT_PROPERTY_NAME, selectionCriteria);	//set the selection criteria
	}

	/**@return The list of interactions for this activity.
	@exception ClassCastException if the value of the interactions property
		is not a list.
	*/
	public List getInteractions()	//G***should we automatically create a list if there isn't one already?
	{
		return (List)getPropertyValue(MAQRO_NAMESPACE_URI, INTERACTIONS_PROPERTY_NAME);	//get the maqro:interactions property value	
	}

	/**Adds an interaction to the activity.
	@param interaction The interaction to add to the list.
	*/
	public void addInteraction(final Interaction interaction)
	{
		List interactionList=getInteractions();	//get the list of interactions
		if(interactionList==null)	//if we have no list
		{
				//create a new list resource containing the added interaction 
			RDFUtilities.setProperty(this, MAQRO_NAMESPACE_URI, INTERACTIONS_PROPERTY_NAME, new RDFListResource(getRDF(), interaction));
		}
		else	//if we already have a list
		{
			interactionList.add(interaction);	//add the interaction to the list
		}
	}
}
