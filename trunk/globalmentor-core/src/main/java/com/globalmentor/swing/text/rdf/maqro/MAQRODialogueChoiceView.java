/*
 * Copyright © 1996-2009 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.swing.text.rdf.maqro;

import javax.swing.text.*;


import static com.globalmentor.swing.text.rdf.RDFStyles.*;

import com.globalmentor.rdf.RDFResource;
import com.globalmentor.swing.text.ViewComponentManager;
import com.globalmentor.swing.text.xml.*;
import com.globalmentor.urf.maqro.Question;

/**Displays a MAQRO choice as dialogue.
@author Garret Wilson
*/
public class MAQRODialogueChoiceView extends XMLComponentParagraphView	//TODO fix	The toggle button's action command will be set to the ident of the response label.
{

	/**The toggle button (radio button or checkbox) this choice view presents.*/
//TODO fix	private final JToggleButton toggleButton;

		/**@return The toggle button (radio button or checkbox) this choice view presents.*/
//TODO fix		public JToggleButton getToggleButton() {return toggleButton;}

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
		super(element, true); //construct the parent view, compensating for the choice toggle button
		final Question question=questionView.getQuestion();	//get the corresponding question
		if(question==null)
		{
			throw new IllegalArgumentException("Question view element does not represent a question.");
		}
/*TODO fix
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
*/
/*TODO fix
		final String ident=(String)StyleUtilities.getDefinedAttribute(element.getAttributes(), ATTRIBUTE_IDENT); //get the ident, if available
		toggleButton.setActionCommand(ident); //set the ident as the action command TODO should we check to see if this is null? probably not---just use it as is
*/
		questionView.addChoiceView(this);	//tell the question view that this view is one of its choices
/*TODO fix
		if(maxResponseCount<=1) //if we shouldn't allow multiple selections
		{
			questionView.getButtonGroup().add(getToggleButton()); //add our radio button to the enclosing question button group to allow for mutual exclusion
		}
*/
			//place the button in the near inset of the tile axis and in the middle of the perpendicular axis
		final int axis=getAxis();	//get our axis
		final ViewComponentManager.AxisLocation.Region regionX=axis==X_AXIS ? ViewComponentManager.AxisLocation.Region.MIDDLE : ViewComponentManager.AxisLocation.Region.BEFORE; 
		final ViewComponentManager.AxisLocation.Region regionY=axis==Y_AXIS ? ViewComponentManager.AxisLocation.Region.MIDDLE : ViewComponentManager.AxisLocation.Region.BEFORE; 
//TODO fix		getComponentManager().add(getToggleButton(), regionX, 0.5f, regionY, 0.5f); //add the radio button at the start of the line
//TODO del when works		getComponentManager().add(getToggleButton(), ViewComponentManager.Border.LINE_START); //add the radio button at the start of the line
	}

}