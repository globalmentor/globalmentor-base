package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.List;
import com.garretwilson.rdf.*;

import static com.garretwilson.rdf.RDFUtilities.*;
import static com.garretwilson.rdf.maqro.MAQROConstants.*;

import static com.garretwilson.rdf.dublincore.DCUtilities.*;
import com.garretwilson.rdf.xmlschema.IntegerLiteral;

/**Class representing a group of MAQRO interactions.
@author Garret Wilson
*/
public class Group extends Interaction
{

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

	/**@return The list of interactions for this group, or <code>null</code> if there is no list of interactions or the value is not a list.*/
	public List<RDFResource> getInteractions()	//TODO we're forced to return List instead of RDFListResource because of DictionaryActivity; see how we can get around this---this may be a holdover from some obsolete code
	{
		return asListResource(getPropertyValue(MAQRO_NAMESPACE_URI, INTERACTIONS_PROPERTY_NAME));	//get the maqro:interactions property value as a list	
	}

	/**Sets the list of interactions for this group.
	@param interactions The list of interactions for this group, or <code>null</code> if the list of interactions should be removed.
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
		List<RDFResource> interactionList=getInteractions();	//get the list of interactions
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

	/**@return The maximum amount of time for the group, in milliseconds, or -1
	if the time is not specified.
	*/
	public long getMaxTime()
	{
		return IntegerLiteral.asLongValue(getPropertyValue(MAQRO_NAMESPACE_URI, MAX_TIME_PROPERTY_NAME)); //get the integer value of the property
	}

	/**Sets the maximum amount of time for the group, in milliseconds.
	@param maxTime The maximum amount of time for the group, in milliseconds, or
		-1 if there should not be a maximum time.
	*/
	public void setMaxTime(final long maxTime)
	{
		setProperty(MAQRO_NAMESPACE_URI, MAX_TIME_PROPERTY_NAME, maxTime>=0 ? new IntegerLiteral(maxTime) : null); //set the property with an integer typed literal
	}


	/**Returns a string representation of the resource.
	<p>This implementation returns the <code>dc:title</code>, if there is title, otherwise, the default string value is returned.</p> 
	@return A string representation of the resource.
	*/
	public String toString()
	{
		final RDFObject title=getTitle(this);	//get the group title, if there is one
		return title instanceof RDFLiteral ? ((RDFLiteral)title).getLexicalForm() : super.toString();	//use the title if there is one
	}
}
