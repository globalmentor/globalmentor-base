package com.garretwilson.swing.text.rdf.maqro;

import javax.swing.*;
import javax.swing.text.*;
import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.rdf.maqro.MAQROConstants.*;
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

	/**@return The question stored in a parent element's attributes, or <code>null</code> if no question could be found.*/ 
	protected Question getQuestion()
	{
		final Element questionElement=getAncestorElement(getElement(), MAQRO_NAMESPACE_URI.toString(), QUESTION_CLASS_NAME);
		return questionElement!=null ? asInstance(getRDFResource(questionElement.getAttributes()), Question.class) : null;	//get the question from the attributes 
	}
				
	/**Constructs a response label view for a question and adds the view to the map of the logical ID response view.
	@param element The element for which this view is responsible.
	@param questionView The question view with which this choice is associated, or <code>null</code> if there is no associated question rendering view.
 	@exception IllegalArgumentException if the given element does not have an ancestor element representing a question.
	*/
	public MAQRODialogueChoiceView(final Element element, final MAQROQuestionView questionView)
	{
		super(element); //construct the parent view
		final Question question=getQuestion();	//get the corresponding question
		if(question==null)
		{
			throw new IllegalArgumentException("Element does not have question ancestor.");
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
		if(questionView!=null && maxResponseCount<=1) //if we have an enclosing choice rendering view, and we shouldn't allow multiple selections
		{
			questionView.getButtonGroup().add(getToggleButton()); //add our radio button to the enclosing question button group to allow for mutual exclusion
		}
		getComponentManager().add(getToggleButton(), ViewComponentManager.Border.LINE_START); //add the radio button at the start of the line
	}

}