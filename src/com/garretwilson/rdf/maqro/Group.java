package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.List;
import com.garretwilson.rdf.*;

/**Class representing a group of MAQRO interactions.
@author Garret Wilson
*/
public class Group extends Interaction
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return MAQRO_NAMESPACE_URI;}

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return GROUP_CLASS_NAME;}

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

	/**@return The list of interactions for this group, or <code>null</code>
		if there is no list of interactions or the value is not a list.
	*/
	public List getInteractions()	//TODO we're forced to return List instead of RDFListResource because of DictionaryActivity; see how we can get around this---this may be a holdover from some obsolete code
	{
		return RDFUtilities.asListResource(getPropertyValue(MAQRO_NAMESPACE_URI, INTERACTIONS_PROPERTY_NAME));	//get the maqro:interactions property value as a list	
	}

	/**Sets the list of interactions for this group.
	@param interactions The list of interactions for this group, or
		<code>null</code> if the list of interactions should be removed.
	*/
	public void setInteractions(final RDFListResource interactions)
	{
		setProperty(MAQRO_NAMESPACE_URI, INTERACTIONS_PROPERTY_NAME, interactions);	//set the maqro:interactions property
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
			setInteractions(new RDFListResource(getRDF(), interaction));
		}
		else	//if we already have a list
		{
			interactionList.add(interaction);	//add the interaction to the list
		}
	}
}
