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

import com.globalmentor.urf.URF;
import static com.globalmentor.urf.URF.*;
import com.globalmentor.urf.content.Text;
import static com.globalmentor.java.Objects.*;
import static com.globalmentor.urf.maqro.MAQRO.*;

/**Class representing a MAQRO activity.
The activity, besides containing interactions, is an interaction itself.
The activity is also a group, and can contain other groups, which themselves are interactions.
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

	/**@return The introduction to the activity, or <code>null</code> if there is no introductiono or the introduction is not text.*/
	public Text getIntroduction()
	{
		return asInstance(getPropertyValue(INTRODUCTION_PROPERTY_URI), Text.class);	//get the introduction only if it is Text		
	}

	/**Sets the introduction of the activity.
	@param introduction The introduction to the activity, or <code>null</code> if there should be no introduction.
	*/
	public void setIntroduction(final Text introduction)
	{
		setPropertyValue(INTRODUCTION_PROPERTY_URI, introduction);	//set the introduction		
	}

	/**@return A filter that filters objects based upon the contents of the selection criteria, or <code>null</code> if there is no filter.
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
/*TODO fix
	protected boolean isSelected(final Interaction interaction, final Set categorySet)
	{
		if(!interaction.hasCategory(categorySet))	//if this interaction doesn't have one of the correct categories
		{
			return false;	//don't select this category
		}
		return true;	//show that this interaction passed all the tests and should be selected
	}
*/

		//interaction behavior
			//permissions
	/**@return <code>true</code> if hints are allowed.*/
	public boolean isAllowHint()
	{
		return Boolean.TRUE.equals(asBoolean(getPropertyValue(ALLOW_HINT_PROPERTY_URI)));
	}

	/**Sets whether hints are allowed.
	@param allowHint <code>true</code> if hints should be allowed.
	*/
	public void setAllowHint(final boolean allowHint)
	{
		setPropertyValue(ALLOW_HINT_PROPERTY_URI, allowHint);
	}

	/**@return <code>true</code> if previous nativation is allowed.*/
	public boolean isAllowPrevious()
	{
		return Boolean.TRUE.equals(asBoolean(getPropertyValue(ALLOW_PREVIOUS_PROPERTY_URI)));
	}

	/**Sets whether previous navigation is allowed.
	@param allowPrevious <code>true</code> if previous navigation should be allowed.
	*/
	public void setAllowPrevious(final boolean allowPrevious)
	{
		setPropertyValue(ALLOW_PREVIOUS_PROPERTY_URI, allowPrevious);
	}

	/**@return <code>true</code> if the activity can be canceled.*/
	public boolean isAllowCancel()
	{
		return Boolean.TRUE.equals(asBoolean(getPropertyValue(ALLOW_CANCEL_PROPERTY_URI)));
	}

	/**Sets whether the activity can be canceled.
	@param allowStop <code>true</code> if canceling should be allowed.
	*/
	public void setAllowCancel(final boolean allowCancel)
	{
		setPropertyValue(ALLOW_CANCEL_PROPERTY_URI, allowCancel);
	}

	/**@return <code>true</code> if submission is allowed; the default is <code>true</code>.*/
	public boolean isAllowSubmit()
	{
		return asInstance(getPropertyValue(ALLOW_SUBMIT_PROPERTY_URI), Boolean.class, Boolean.TRUE).booleanValue(); //get the boolean value of the property, defaulting to true
	}

	/**Sets whether submission is allowed.
	@param allowSubmit <code>true</code> if submission should be allowed.
	*/
	public void setAllowSubmit(final boolean allowSubmit)
	{
		setPropertyValue(ALLOW_SUBMIT_PROPERTY_URI, allowSubmit);
	}

			//process
	/**@return <code>true</code> if each response commit should be confirmed.*/
	public boolean isConfirmCommit()
	{
		return Boolean.TRUE.equals(asBoolean(getPropertyValue(CONFIRM_COMMIT_PROPERTY_URI)));
	}

	/**Sets whether each response commit must be confirmed.
	@param confirmCommit <code>true</code> if each commit must be confirmed.
	*/
	public void setConfirmCommit(final boolean confirmCommit)
	{
		setPropertyValue(CONFIRM_COMMIT_PROPERTY_URI, confirmCommit);
	}

	/**@return <code>true</code> if activity submission should be confirmed.*/
	public boolean isConfirmSubmit()
	{
		return Boolean.TRUE.equals(asBoolean(getPropertyValue(CONFIRM_SUBMIT_PROPERTY_URI)));
	}

	/**Sets whether activity submission must be confirmed.
	@param confirmSubmit <code>true</code> if activity submission must be confirmed.
	*/
	public void setConfirmSubmit(final boolean confirmSubmit)
	{
		setPropertyValue(CONFIRM_SUBMIT_PROPERTY_URI, confirmSubmit);
	}
	
	/**@return <code>true</code> if a response is required for all applicable interactions.*/
	public boolean isRequireResponse()
	{
		return Boolean.TRUE.equals(asBoolean(getPropertyValue(REQUIRE_RESPONSE_PROPERTY_URI)));
	}

	/**Sets whether a response is required for all applicable interactions.
	@param confirmCommit <code>true</code> if a response is always required.
	*/
	public void setRequireResponse(final boolean requireResponse)
	{
		setPropertyValue(REQUIRE_RESPONSE_PROPERTY_URI, requireResponse);
	}
	
			//feedback
	/**@return <code>true</code> if the result of each interaction should immediately be shown.*/
	public boolean isShowEachResult()
	{
		return Boolean.TRUE.equals(asBoolean(getPropertyValue(SHOW_EACH_RESULT_PROPERTY_URI)));
	}

	/**Sets whether the result of each interaction should immediately be shown.
	@param showEachResult <code>true</code> if each result should be shown.
	*/
	public void setShowEachResult(final boolean showEachResult)
	{
		setPropertyValue(SHOW_EACH_RESULT_PROPERTY_URI, showEachResult);
	}

	/**@return <code>true</code> if the final result of the activity should be shown.*/
	public boolean isShowFinalResult()
	{
		return Boolean.TRUE.equals(asBoolean(getPropertyValue(SHOW_FINAL_RESULT_PROPERTY_URI)));
	}

	/**Sets whether the final result of the activity should be shown.
	@param showFinalResult <code>true</code> if the activity result should be shown.
	*/
	public void setShowFinalResult(final boolean showFinalResult)
	{
		setPropertyValue(SHOW_FINAL_RESULT_PROPERTY_URI, showFinalResult);
	}

	/**@return <code>true</code> if the progress of results should be shown.*/
	public boolean isShowResultProgress()
	{
		return Boolean.TRUE.equals(asBoolean(getPropertyValue(SHOW_RESULT_PROGRESS_PROPERTY_URI)));
	}

	/**Sets whether real-time results progress should be shown.
	@param showResultProgress <code>true</code> if the results should continuously be shown.
	*/
	public void setShowResultProgress(final boolean showResultProgress)
	{
		setPropertyValue(SHOW_RESULT_PROGRESS_PROPERTY_URI, showResultProgress);
	}

	/**@return <code>true</code> if the position within the activity should be shown.*/
	public boolean isShowProgress()
	{
		return Boolean.TRUE.equals(asBoolean(getPropertyValue(SHOW_PROGRESS_PROPERTY_URI)));
	}

	/**Sets whether the position within the activity should be shown.
	@param showProgress <code>true</code> if the position within the activity should be shown.
	*/
	public void setShowProgress(final boolean showProgress)
	{
		setPropertyValue(SHOW_PROGRESS_PROPERTY_URI, showProgress);
	}

	/**@return <code>true</code> if the time used and/or time available should be shown.*/
	public boolean isShowTime()
	{
		return Boolean.TRUE.equals(asBoolean(getPropertyValue(SHOW_TIME_PROPERTY_URI)));
	}

	/**Sets whether the time used and/or time available should be shown..
	@param showTime <code>true</code> if the time used and/or time available should be shown.
	*/
	public void setShowTime(final boolean showTime)
	{
		setPropertyValue(SHOW_TIME_PROPERTY_URI, showTime);
	}

}
