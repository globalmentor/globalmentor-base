package com.garretwilson.rdf.maqro;

import java.net.URI;
import com.garretwilson.rdf.*;

/**Class representing a MAQRO activity.
	The activity, besides containing interaction, is an interaction itself.
@author Garret Wilson
*/
public class Activity extends DefaultRDFResource implements Interaction, MAQROConstants
{

	/**Default constructor.*/
	public Activity()
	{
	}

	/**Constructs an activity with a reference URI.
	@param referenceURI The reference URI for the new publication.
	*/
	public Activity(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The list of interactions for this activity.
	@exception ClassCastException if the value of the interactions property
		is not a list resource.
	*/
	public RDFListResource getInteractions()	//G***should we automatically create a list if there isn't one already?
	{
		return (RDFListResource)getPropertyValue(MAQRO_NAMESPACE_URI, INTERACTIONS_PROPERTY_NAME);	//get the maqro:interactions property value	
	}

	/**Adds an interaction to the activity.
	@param interaction The interaction to add to the list.
	*/
	public void addInteraction(final Interaction interaction)
	{
		RDFListResource interactionList=getInteractions();	//get the list of interactions
		if(interactionList==null)	//if we have no list
		{
			final RDF rdf=new RDF();	//TODO fix data model
				//create a new list resource containing the added interaction 
			RDFUtilities.setProperty(rdf, this, MAQRO_NAMESPACE_URI, INTERACTIONS_PROPERTY_NAME, new RDFListResource(rdf, interaction));
		}
		else	//if we already have a list
		{
			interactionList.add(interaction);	//add the interaction to the list
		}
	}
}
