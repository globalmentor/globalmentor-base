package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.*;
import com.garretwilson.rdf.*;
import com.garretwilson.util.CollectionUtilities;

/**Class representing a MAQRO activity.
The activity, besides containing interactions, is an interaction itself.
The activity is also a group, and can contain other groups, which themselves
	are interactions.
@author Garret Wilson
*/
public class Activity extends Group
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

	/**@return A filter that filters objects based upon the contents of the
		selection criteria, or <code>null</code> if there is no filter.
	@see #getSelect()
	*/
/*G***del
	public Filter getSelectFilter()
	{
		final SelectDescription selectDescription=getSelect();	//get the selection criteria
		return selectDescription!=null ? new InteractionFilter(selectDescription) : null;	//return a filter if we have selection criteria
	}
*/

	/**Selects root-level interactions from which to choose.
	@return The list of root-level interactions for the activity, correctly
		filtered to include only those interactions that meet the selection
		criteria.
	@see #getSelect
	*/
	public List getSelectedInteractionList()
	{
		return getSelectedInteractionList(this);	//return selected interactions from the activity itself
	}

	/**Selects group interactions from which to choose.
	@param group The group from which to select interactions.
	@return The list of group-level interactions for the activity, correctly
		filtered to include only those interactions that meet the selection
		criteria.
	@see #getSelect
	*/
	public List getSelectedInteractionList(final Group group)
	{
		final SelectDescription select=getSelect();	//get the selection criteria
		if(select!=null)	//if we have selection criteria
		{
				//check categories
			final Set selectCategorySet=new HashSet();	//create a set of categories
			CollectionUtilities.addAll(selectCategorySet, select.getCategoryIterator());	//put the selection categories into our set
			final List selectedInteractionList=new ArrayList();	//create a list into which we'll place our selected interactions
			final List interactionList=group.getInteractions();	//get the interactions from this group
			final Iterator interactionIterator=interactionList.iterator();	//get an iterator to the interactions
			while(interactionIterator.hasNext())	//while there are more interactions
			{
				final Interaction interaction=(Interaction)interactionIterator.next();	//get the next interaction
				if(isSelected(interaction, selectCategorySet))	//if we should select this interaction
				{
					selectedInteractionList.add(interaction);	//add this interaction to our list
				}
			}
			return selectedInteractionList;	//return the list of selected interactions
		}
		else	//if there is no selection criteria
		{
			return group.getInteractions();	//return all the interactions for the group
		} 
	}

	/**Determines if the given interaction should be selected.
	@param interaction The interaction to select.
	@param categorySet The set of categories, any of which will allow the
		interaction to be selected.
	@return <code>true</code> if the interaction should be selected.
	*/
	protected boolean isSelected(final Interaction interaction, final Set categorySet)
	{
		if(!interaction.hasCategory(categorySet))	//if this interaction doesn't have one of the correct categories
		{
			return false;	//don't select this category
		}
		return true;	//show that this interaction passed all the tests and should be selected
	}

	/**@return The selection criteria, or <code>null</code> if there is no
		selection criteria indicated or if it is not of the correct type.
	*/
	public SelectDescription getSelect()
	{
		final RDFObject select=getPropertyValue(MAQRO_NAMESPACE_URI, SELECT_PROPERTY_NAME);	//get the maqro:select property value
		return select instanceof SelectDescription ? (SelectDescription)select : null;	//return the selection criteria if there are any
	}

	/**Sets the selection criteria for the activity.
	@param selectDescription The selection criteria.
	*/
	public void setSelect(final SelectDescription selectDescription)
	{
		RDFUtilities.setProperty(this, MAQRO_NAMESPACE_URI, SELECT_PROPERTY_NAME, selectDescription);	//set the selection criteria
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
