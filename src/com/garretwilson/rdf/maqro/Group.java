package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.List;
import com.garretwilson.rdf.*;

/**Class representing a group of MAQRO interactions.
@author Garret Wilson
*/
public class Group extends Interaction
{

	/**Default constructor.*/
	public Group()
	{
	}

	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public Group(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The list of interactions for this group.
	@exception ClassCastException if the value of the interactions property
		is not a list.
	*/
	public List getInteractions()	//G***should we automatically create a list if there isn't one already?
	{
		return (List)getPropertyValue(MAQRO_NAMESPACE_URI, INTERACTIONS_PROPERTY_NAME);	//get the maqro:interactions property value	
	}

	/**Adds an interaction to the group.
	@param interaction The interaction to add to the group.
	*/
	public void addInteraction(final Interaction interaction)
	{
		List interactionList=getInteractions();	//get the list of interactions
		if(interactionList==null)	//if we have no list
		{
				//create a new list resource containing the added interaction 
			setProperty(MAQRO_NAMESPACE_URI, INTERACTIONS_PROPERTY_NAME, new RDFListResource(getRDF(), interaction));
		}
		else	//if we already have a list
		{
			interactionList.add(interaction);	//add the interaction to the list
		}
	}
}
