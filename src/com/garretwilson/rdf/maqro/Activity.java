package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.*;
import com.garretwilson.rdf.*;
import com.garretwilson.rdf.xmlschema.BooleanLiteral;
import com.garretwilson.util.CollectionUtilities;

/**Class representing a MAQRO activity.
The activity, besides containing interactions, is an interaction itself.
The activity is also a group, and can contain other groups, which themselves
	are interactions.
@author Garret Wilson
*/
public class Activity extends Group
{

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return ACTIVITY_CLASS_NAME;}

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
/*TODO fix selection
	public List getSelectedInteractionList()
	{
		return getSelectedInteractionList(this);	//return selected interactions from the activity itself
	}
*/

	/**Selects group interactions from which to choose.
	@param group The group from which to select interactions.
	@return The list of group-level interactions for the activity, correctly
		filtered to include only those interactions that meet the selection
		criteria.
	@see #getSelect
	*/
/*TODO fix selection
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
*/

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
	public Selection getSelection()
	{
		final RDFObject select=getPropertyValue(MAQRO_NAMESPACE_URI, SELECTION_PROPERTY_NAME);	//get the maqro:select property value
		return select instanceof Selection ? (Selection)select : null;	//return the selection criteria if there are any
	}

	/**Sets the selection criteria for the activity.
	@param selectDescription The selection criteria.
	*/
	public void setSelection(final Selection selection)
	{
		setProperty(MAQRO_NAMESPACE_URI, SELECTION_PROPERTY_NAME, selection);	//set the selection criteria
	}

		//behavior
			//permissions
	/**@return <code>true</code> if hints are allowed.*/
	public boolean isAllowHint()
	{
		return BooleanLiteral.asBooleanValue(getPropertyValue(MAQRO_NAMESPACE_URI, ALLOW_HINT_PROPERTY_NAME)); //get the boolean value of the property
	}

	/**Sets whether hints are allowed.
	@param allowHint <code>true</code> if hints should be allowed.
	*/
	public void setAllowHint(final boolean allowHint)
	{
		setProperty(MAQRO_NAMESPACE_URI, ALLOW_HINT_PROPERTY_NAME, new BooleanLiteral(allowHint)); //set the property with a boolean typed literal
	}

	/**@return <code>true</code> if previous nativation is allowed.*/
	public boolean isAllowPrevious()
	{
		return BooleanLiteral.asBooleanValue(getPropertyValue(MAQRO_NAMESPACE_URI, ALLOW_PREVIOUS_PROPERTY_NAME)); //get the boolean value of the property
	}

	/**Sets whether previous navigation is allowed.
	@param allowPrevious <code>true</code> if previous navigation should be allowed.
	*/
	public void setAllowPrevious(final boolean allowPrevious)
	{
		setProperty(MAQRO_NAMESPACE_URI, ALLOW_PREVIOUS_PROPERTY_NAME, new BooleanLiteral(allowPrevious)); //set the property with a boolean typed literal
	}

	/**@return <code>true</code> if the activity can be canceled.*/
	public boolean isAllowCancel()
	{
		return BooleanLiteral.asBooleanValue(getPropertyValue(MAQRO_NAMESPACE_URI, ALLOW_CANCEL_PROPERTY_NAME)); //get the boolean value of the property
	}

	/**Sets whether the activity can be canceled.
	@param allowStop <code>true</code> if canceling should be allowed.
	*/
	public void setAllowCancel(final boolean allowCancel)
	{
		setProperty(MAQRO_NAMESPACE_URI, ALLOW_CANCEL_PROPERTY_NAME, new BooleanLiteral(allowCancel)); //set the property with a boolean typed literal
	}

			//process
	/**@return <code>true</code> if each response commit should be confirmed.*/
	public boolean isConfirmCommit()
	{
		return BooleanLiteral.asBooleanValue(getPropertyValue(MAQRO_NAMESPACE_URI, CONFIRM_COMMIT_PROPERTY_NAME)); //get the boolean value of the property
	}

	/**Sets whether each response commit must be confirmed.
	@param confirmCommit <code>true</code> if each commit must be confirmed.
	*/
	public void setConfirmCommit(final boolean confirmCommit)
	{
		setProperty(MAQRO_NAMESPACE_URI, CONFIRM_COMMIT_PROPERTY_NAME, new BooleanLiteral(confirmCommit)); //set the property with a boolean typed literal
	}

	/**@return <code>true</code> if activity submission should be confirmed.*/
	public boolean isConfirmSubmit()
	{
		return BooleanLiteral.asBooleanValue(getPropertyValue(MAQRO_NAMESPACE_URI, CONFIRM_SUBMIT_PROPERTY_NAME)); //get the boolean value of the property
	}

	/**Sets whether activity submission must be confirmed.
	@param confirmSubmit <code>true</code> if activity submission must be confirmed.
	*/
	public void setConfirmSubmit(final boolean confirmSubmit)
	{
		setProperty(MAQRO_NAMESPACE_URI, CONFIRM_SUBMIT_PROPERTY_NAME, new BooleanLiteral(confirmSubmit)); //set the property with a boolean typed literal
	}
	
	/**@return <code>true</code> if a response is required for all applicable interactions.*/
	public boolean isRequireResponse()
	{
		return BooleanLiteral.asBooleanValue(getPropertyValue(MAQRO_NAMESPACE_URI, REQUIRE_RESPONSE_PROPERTY_NAME)); //get the boolean value of the property
	}

	/**Sets whether a response is required for all applicable interactions.
	@param confirmCommit <code>true</code> if a response is always required.
	*/
	public void setRequireResponse(final boolean requireResponse)
	{
		setProperty(MAQRO_NAMESPACE_URI, REQUIRE_RESPONSE_PROPERTY_NAME, new BooleanLiteral(requireResponse)); //set the property with a boolean typed literal
	}
	
			//feedback
	/**@return <code>true</code> if the result of each interaction should immediately be shown.*/
	public boolean isShowEachResult()
	{
		return BooleanLiteral.asBooleanValue(getPropertyValue(MAQRO_NAMESPACE_URI, SHOW_EACH_RESULT_PROPERTY_NAME)); //get the boolean value of the property
	}

	/**Sets whether the result of each interaction should immediately be shown.
	@param showEachResult <code>true</code> if each result should be shown.
	*/
	public void setShowEachResult(final boolean showEachResult)
	{
		setProperty(MAQRO_NAMESPACE_URI, SHOW_EACH_RESULT_PROPERTY_NAME, new BooleanLiteral(showEachResult)); //set the property with a boolean typed literal
	}

	/**@return <code>true</code> if the final result of the activity should be shown.*/
	public boolean isShowFinalResult()
	{
		return BooleanLiteral.asBooleanValue(getPropertyValue(MAQRO_NAMESPACE_URI, SHOW_FINAL_RESULT_PROPERTY_NAME)); //get the boolean value of the property
	}

	/**Sets whether the final result of the activity should be shown.
	@param showFinalResult <code>true</code> if the activity result should be shown.
	*/
	public void setShowFinalResult(final boolean showFinalResult)
	{
		setProperty(MAQRO_NAMESPACE_URI, SHOW_FINAL_RESULT_PROPERTY_NAME, new BooleanLiteral(showFinalResult)); //set the property with a boolean typed literal
	}

	/**@return <code>true</code> if the progress of results should be shown.*/
	public boolean isShowResultProgress()
	{
		return BooleanLiteral.asBooleanValue(getPropertyValue(MAQRO_NAMESPACE_URI, SHOW_RESULT_PROGRESS_PROPERTY_NAME)); //get the boolean value of the property
	}

	/**Sets whether real-time results progress should be shown.
	@param showResultProgress <code>true</code> if the results should continuously be shown.
	*/
	public void setShowResultProgress(final boolean showResultProgress)
	{
		setProperty(MAQRO_NAMESPACE_URI, SHOW_RESULT_PROGRESS_PROPERTY_NAME, new BooleanLiteral(showResultProgress)); //set the property with a boolean typed literal
	}

	/**@return <code>true</code> if the position within the activity should be shown.*/
	public boolean isShowProgress()
	{
		return BooleanLiteral.asBooleanValue(getPropertyValue(MAQRO_NAMESPACE_URI, SHOW_PROGRESS_PROPERTY_NAME)); //get the boolean value of the property
	}

	/**Sets whether the position within the activity should be shown.
	@param showProgress <code>true</code> if the position within the activity should be shown.
	*/
	public void setShowProgress(final boolean showProgress)
	{
		setProperty(MAQRO_NAMESPACE_URI, SHOW_PROGRESS_PROPERTY_NAME, new BooleanLiteral(showProgress)); //set the property with a boolean typed literal
	}

	/**@return <code>true</code> if the time used and/or time available should be shown.*/
	public boolean isShowTime()
	{
		return BooleanLiteral.asBooleanValue(getPropertyValue(MAQRO_NAMESPACE_URI, SHOW_TIME_PROPERTY_NAME)); //get the boolean value of the property
	}

	/**Sets whether the time used and/or time available should be shown..
	@param showTime <code>true</code> if the time used and/or time available should be shown..
	*/
	public void setShowTime(final boolean showTime)
	{
		setProperty(MAQRO_NAMESPACE_URI, SHOW_TIME_PROPERTY_NAME, new BooleanLiteral(showTime)); //set the property with a boolean typed literal
	}

}
