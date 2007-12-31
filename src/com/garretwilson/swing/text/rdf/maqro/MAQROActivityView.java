package com.garretwilson.swing.text.rdf.maqro;

import static com.garretwilson.lang.Objects.*;
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
		super(element, axis, true); //construct the parent, compensating for the submit button
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
			getComponentManager().add(submitButton, regionX, 0.5f, regionY, 0.5f); //add the button to the end of the activity view
		}
	}

}