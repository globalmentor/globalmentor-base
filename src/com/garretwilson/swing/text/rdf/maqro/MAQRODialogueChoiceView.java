package com.garretwilson.swing.text.rdf.maqro;

import javax.swing.*;
import javax.swing.text.*;
import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.rdf.maqro.MAQROConstants.*;

import com.garretwilson.rdf.RDFResource;
import com.garretwilson.rdf.maqro.Question;
import com.garretwilson.swing.text.ViewComponentManager;

import static com.garretwilson.swing.text.rdf.RDFStyleUtilities.*;

import com.garretwilson.swing.text.xml.*;
import static com.garretwilson.swing.text.xml.XMLStyleUtilities.*;

/**Displays a MAQRO choice as dialogue.
TODO fix	The toggle button's action command will be set to the ident of the response label.
@author Garret Wilson
*/
public class MAQRODialogueChoiceView extends XMLComponentParagraphView
{

	/**The toggle button (radio button or checkbox) this choice view presents.*/
	private final JToggleButton toggleButton;

		/**@return The toggle button (radio button or checkbox) this choice view presents.*/
		public JToggleButton getToggleButton() {return toggleButton;}

	/**@return The choice stored in a element's attributes, or <code>null</code> if no choice could be found.*/ 
	public RDFResource getChoice()
	{
		return getRDFResource(getElement().getAttributes());	//get the choice from the attributes 
	}
				
	/**Constructs a response label view for a question and adds the view to the map of the logical ID response view.
	@param element The element for which this view is responsible.
	@param questionView The question view with which this choice is associated, or <code>null</code> if there is no associated question rendering view.
 	@exception IllegalArgumentException if the element of the given question view does not represent a question.
	*/
	public MAQRODialogueChoiceView(final Element element, final MAQROQuestionView questionView)
	{
		super(element); //construct the parent view
		final Question question=questionView.getQuestion();	//get the corresponding question
		if(question==null)
		{
			throw new IllegalArgumentException("Question view element does not represent a question.");
		}
		final int maxResponseCount=question.getMaxResponseCount();	//find out the maximum responses allowed
		if(maxResponseCount>1)  //if we allow multiple responses
		{
			toggleButton=new JCheckBox(); //display a checkbox, which allows multiple selections
		}
		else  //if we don't allow multiple selections
		{
			toggleButton=new JRadioButton(); //display a radio button, which does not allow multiple selections
		}
		toggleButton.setContentAreaFilled(false); //don't paint the background---let the toggle button be transparent
		toggleButton.setFocusable(false);	//don't allow the button to receive the focus TODO maybe fix the focus transfer to other components in the book
/*TODO fix
		final String ident=(String)StyleUtilities.getDefinedAttribute(element.getAttributes(), ATTRIBUTE_IDENT); //get the ident, if available
		toggleButton.setActionCommand(ident); //set the ident as the action command G***should we check to see if this is null? probably not---just use it as is
*/
		questionView.addChoiceView(this);	//tell the question view that this view is one of its choices
		if(maxResponseCount<=1) //if we shouldn't allow multiple selections
		{
			questionView.getButtonGroup().add(getToggleButton()); //add our radio button to the enclosing question button group to allow for mutual exclusion
		}
			//place the button in the near inset of the tile axis and in the middle of the perpendicular axis
		final int axis=getAxis();	//get our axis
		final ViewComponentManager.AxisLocation.Region regionX=axis==X_AXIS ? ViewComponentManager.AxisLocation.Region.BEFORE : ViewComponentManager.AxisLocation.Region.MIDDLE; 
		final ViewComponentManager.AxisLocation.Region regionY=axis==Y_AXIS ? ViewComponentManager.AxisLocation.Region.BEFORE : ViewComponentManager.AxisLocation.Region.MIDDLE; 
		getComponentManager().add(getToggleButton(), regionX, 0, regionY, 0); //add the radio button at the start of the line
//TODO del when works		getComponentManager().add(getToggleButton(), ViewComponentManager.Border.LINE_START); //add the radio button at the start of the line
	}

}