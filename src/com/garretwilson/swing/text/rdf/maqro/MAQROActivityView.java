package com.garretwilson.swing.text.rdf.maqro;

import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.swing.text.rdf.RDFStyleUtilities.*;

import java.awt.Dimension;

import javax.swing.*;
import javax.swing.text.*;

import com.garretwilson.rdf.maqro.Activity;
import com.garretwilson.swing.text.ViewComponentManager;
import com.garretwilson.swing.text.xml.*;

/**View representing a MAQRO activity.
The view optionally contains a submit button representing a provided submit action.
@author Garret Wilson
*/
public class MAQROActivityView extends XMLComponentBlockView	//TODO transfer all the actual MAQRO interactivity views to a globalmentor package
{

	/**The action representing a submission, or <code>null</code> if there is no submit action.*/
	private final Action submitAction;

		/**@return The action representing a submission, or <code>null</code> if there is no submit action.*/
		protected Action getSubmitAction() {return submitAction;}

	/**The button interface to the submit action, or <code>null</code> if there is no submit button.*/
	private final AbstractButton submitButton;

		/**@return The button interface to the submit action, or <code>null</code> if there is no submit button.*/
		protected AbstractButton getSubmitButton() {return submitButton;}

	/**@return The activity stored in a element's attributes, or <code>null</code> if no activity could be found.*/ 
	public Activity getActivity()
	{
		return asInstance(getRDFResource(getElement().getAttributes()), Activity.class);	//get the question from the attributes 
	}

	/**Constructs an activity rendering view.
	@param element The element this view is responsible for.
	@param axis The tiling axis, either <code>View.X_AXIS</code> or <code>View.Y_AXIS</code>.
	@param submitAction The action representing a submission, or <code>null</code> if this activity view
		does not support submission.
	*/
	public MAQROActivityView(final Element element, final int axis, final Action submitAction)
	{
		super(element, axis); //construct the parent
		this.submitAction=submitAction;	//save the submit action
		final Activity activity=getActivity();	//get the activity
		submitButton=activity!=null && activity.isAllowSubmit() && submitAction!=null	//if we have a submitable activity and were provided a submit action	
				? new JButton(submitAction)	//create a button for the submit action
				: null;	//don't create a submit button if we don't have a submitable activity and a submit action
		if(submitButton!=null)	//if there is a submit button
		{
			submitButton.setFocusable(false);	//TODO fix component manager focus traversal
				//place the button in the far inset of the tile axis and in the middle of the perpendicular axis
			final ViewComponentManager.AxisLocation.Region regionX=axis==X_AXIS ? ViewComponentManager.AxisLocation.Region.AFTER : ViewComponentManager.AxisLocation.Region.MIDDLE; 
			final ViewComponentManager.AxisLocation.Region regionY=axis==Y_AXIS ? ViewComponentManager.AxisLocation.Region.AFTER : ViewComponentManager.AxisLocation.Region.MIDDLE; 
			getComponentManager().add(submitButton, regionX, 0, regionY, 0); //add the button to the end of the activity view
		}
	}

	/**Sets the cached properties from the attributes.
	This version compensates for the submit button, if present.
	*/
	protected void setPropertiesFromAttributes() 
	{
		super.setPropertiesFromAttributes();	//set the properties normally
		final AbstractButton submitButton=getSubmitButton();	//see if we have a submit button
		if(submitButton!=null)	//if we have a submit button
		{
			final Dimension submitButtonPreferredSize=submitButton.getPreferredSize();	//get the preferred size of the button 
			short topInset=getTopInset();	//get the insets
			short leftInset=getLeftInset();
			short bottomInset=getBottomInset();
			short rightInset=getRightInset();
			switch(getAxis())	//compensate for the submit button based upon the axis TODO compensate for orientation; eventually, maybe move all this to ViewComponent manager with an option
			{
				case X_AXIS:
					rightInset+=submitButtonPreferredSize.width;	//compensate for the button width
					break;
				case Y_AXIS:
					bottomInset+=submitButtonPreferredSize.height;	//compensate for the button height
					break;
					
			}
		  setInsets(topInset, leftInset, bottomInset, rightInset);	//update our insets with the new values
		}
	}

	/**Determines the preferred span for this view along an axis.
	This version compensates for the size of the submit button, if present.
	@param axis Either <code>View.X_AXIS</code> or <code>View.Y_AXIS</code>.
	@return The span into which the view would like to be rendered.
	*/
/*TODO del when works
	public float getPreferredSpan(final int axis)
	{
		float preferredSpan=super.getPreferredSpan(axis);	//get the default preferred span along this axis
		final AbstractButton submitButton=getSubmitButton();	//see if we have a submit button
		if(submitButton!=null)	//if we have a submit button
		{
			final Dimension submitButtonPreferredSize=submitButton.getPreferredSize();	//get the preferred size of the button 
			final int buttonSpan=axis==X_AXIS ? submitButtonPreferredSize.width : submitButtonPreferredSize.height;	//get the span of the button on the axis
			if(axis==getAxis())	//if we're getting the preferred span of our tiling axis
			{
				preferredSpan+=buttonSpan;	//compensate for the size of the button
			}
			else	//if we're getting the preferred span of the perpendicular axis
			{					
				preferredSpan=max(preferredSpan, (float)buttonSpan);	//make sure the preferred span is not too small for the button
			}
		}
		return preferredSpan;	//return the preferred span
	}
*/

	/**Determines the minimum span for this view along an axis.
	This version compensates for the size of the submit button, if present.
	This version compensates for the size of the submit button, if present.
	@param axis Either <code>View.X_AXIS</code> or <code>View.Y_AXIS</code>.
	@return The minimum span into which the view can be rendered.
	*/
/*TODO del when works
	public float getMinimumSpan(final int axis)
	{
		float minimumSpan=super.getMinimumSpan(axis);	//get the default minimum span along this axis
		final AbstractButton submitButton=getSubmitButton();	//see if we have a submit button
		if(submitButton!=null)	//if we have a submit button
		{
			final Dimension submitButtonPreferredSize=submitButton.getPreferredSize();	//get the preferred size of the button 
			final int buttonSpan=axis==X_AXIS ? submitButtonPreferredSize.width : submitButtonPreferredSize.height;	//get the span of the button on the axis
			minimumSpan=max(minimumSpan, (float)buttonSpan);	//make sure the minimum span is not too small for the button
		}
		return minimumSpan;	//return the minimum span
	}
*/

	/**Creates a fragment view into which pieces of this view will be placed.
	@param isFirstFragment Whether this fragment holds the first part of the original view.
	@param isLastFragment Whether this fragment holds the last part of the original view.
	*/
	public View createFragmentView(final boolean isFirstFragment, final boolean isLastFragment)
	{
	  return new MAQROFragmentActivityView(getElement(), getAxis(), this, isFirstFragment, isLastFragment);	//create a fragment of this view
	}

	/**The class that serves as a fragment if the paragraph is broken.
	@author Garret Wilson
	*/
	protected class MAQROFragmentActivityView extends XMLComponentFragmentBlockView	//TODO this code duplication in the fragment views is getting excessive---determine how we can consolidate perhaps by modifying the fragment hierarchy
	{

		/**Constructs a fragment view.
		@param element The element this view is responsible for.
		@param axis The tiling axis, either View.X_AXIS or View.Y_AXIS.
		@param wholeView The original, unfragmented view from which this fragment (or one or more intermediate fragments) was broken.
		@param firstFragment Whether this is the first fragment of the original view.
		@param lastFragment Whether this is the last fragment of the original view.
		*/
		public MAQROFragmentActivityView(final Element element, final int axis, final View wholeView, final boolean firstFragment, final boolean lastFragment)
		{
			super(element, axis, wholeView, firstFragment, lastFragment); //do the default construction
		}

		/**Creates a fragment view into which pieces of this view will be placed.
		@param isFirstFragment Whether this fragment holds the first part of the original view.
		@param isLastFragment Whether this fragment holds the last part of the original view.
		*/
		public View createFragmentView(final boolean isFirstFragment, final boolean isLastFragment)
		{
		  return new MAQROFragmentActivityView(getElement(), getAxis(), getWholeView(), isFirstFragment, isLastFragment);	//create a fragment of this view, indicating the original view
		}

		/**Sets the cached properties from the attributes.
		This version compensates for the submit button, if present.
		*/
		protected void setPropertiesFromAttributes() 
		{
			super.setPropertiesFromAttributes();	//set the properties normally
			final AbstractButton submitButton=getSubmitButton();	//see if we have a submit button
			if(submitButton!=null)	//if we have a submit button
			{
				final Dimension submitButtonPreferredSize=submitButton.getPreferredSize();	//get the preferred size of the button 
				short topInset=getTopInset();	//get the insets
				short leftInset=getLeftInset();
				short bottomInset=getBottomInset();
				short rightInset=getRightInset();
				switch(getAxis())	//compensate for the submit button based upon the axis TODO compensate for orientation; eventually, maybe move all this to ViewComponent manager with an option
				{
					case X_AXIS:
						rightInset+=submitButtonPreferredSize.width;	//compensate for the button width
						break;
					case Y_AXIS:
						bottomInset+=submitButtonPreferredSize.height;	//compensate for the button height
						break;
						
				}
			  setInsets(topInset, leftInset, bottomInset, rightInset);	//update our insets with the new values
			}
		}
	}

}