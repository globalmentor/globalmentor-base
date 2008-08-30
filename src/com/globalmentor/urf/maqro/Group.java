/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.urf.maqro;

import java.net.URI;
import java.util.List;

import static com.globalmentor.urf.maqro.MAQRO.*;
import static com.globalmentor.urf.URF.*;

import com.globalmentor.urf.URFListResource;

/**Class representing a group of MAQRO interactions.
@author Garret Wilson
*/
public class Group extends AbstractInteraction
{

	/**Default constructor.*/
	public Group()
	{
		this(null);
	}

	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public Group(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The list of interactions for this group, or <code>null</code> if there is no list of interactions or the value is not a list.*/
	public List<Interaction> getInteractions()
	{
		return asListInstance(getPropertyValue(INTERACTIONS_PROPERTY_URI));	//get the maqro.interactions property value as a list	
	}

	/**Sets the list of interactions for this group.
	@param interactions The list of interactions for this group, or <code>null</code> if the list of interactions should be removed.
	*/
	public void setInteractions(final URFListResource<Interaction> interactions)
	{
		setPropertyValue(INTERACTIONS_PROPERTY_URI, interactions);	//set the maqro.interactions property
	}

	/**Adds an interaction to the group.
	@param interaction The interaction to add to the group.
	*/
/*TODO fix
	public void addInteraction(final Interaction interaction)
	{
		List<RDFObject> interactionList=getInteractions();	//get the list of interactions
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
*/

	/**Removes an interaction from the group.
	@param interaction The interaction to remove from the group.
	*/
/*TODO fix
	public void removeInteraction(final Interaction interaction)
	{
		List<RDFObject> interactionList=getInteractions();	//get the list of interactions
		if(interactionList!=null)	//if we have a list
		{
			interactionList.remove(interaction);	//remove the interaction from the list
		}
	}
*/

	/**@return The maximum amount of time for the group, in milliseconds, or -1
	if the time is not specified.
	*/
/*TODO fix
	public long getMaxTime()
	{
		return IntegerLiteral.asLongValue(getPropertyValue(MAQRO_NAMESPACE_URI, MAX_TIME_PROPERTY_NAME)); //get the integer value of the property
	}
*/

	/**Sets the maximum amount of time for the group, in milliseconds.
	@param maxTime The maximum amount of time for the group, in milliseconds, or
		-1 if there should not be a maximum time.
	*/
/*TODO fix
	public void setMaxTime(final long maxTime)
	{
		setProperty(MAQRO_NAMESPACE_URI, MAX_TIME_PROPERTY_NAME, maxTime>=0 ? new IntegerLiteral(maxTime) : null); //set the property with an integer typed literal
	}
*/

}
