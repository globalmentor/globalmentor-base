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

package com.garretwilson.swing.text.xml.qti;

import javax.swing.text.*;
import com.garretwilson.swing.text.xml.*;
import com.globalmentor.mentoract.qti.Assessment;

/**Displays a QTI assessment, along with the appropriate submit button.
@author Garret Wilson
*/
public class QTIAssessmentView extends XMLComponentBlockView	//TODO del class if not needed
{

	/**The button used for submitting the assessment.*/
//TODO del if not needed	private final JButton submitButton;

		/**@return The button used for submitting the assessment.*/
//TODO del if not needed		public JButton getSubmitButton() {return submitButton;}

	/**The list of item views. within the various sections.*/
//TODO del	private final List itemViewList=new ArrayList();

		/**@return The list of item views. within the various sections.*/
//TODO del		public List getItemViewList() {return itemViewList;}

	/**Constructs an assessment view.
	@param element The element this view is responsible for.
	@param axis The tiling axis, either <code>View.X_AXIS</code> or
		<code>View.Y_AXIS</code>.
	@param assessment The assessment this view represents
	*/
	public QTIAssessmentView(final Element element, final int axis, final Assessment assessment)
	{
		super(element, axis); //construct the parent TODO fix or del: and allowing expansion in both direction
/*TODO fix
		final String ident=(String)XMLStyleConstants.getDefinedAttribute(element.getAttributes(), ATTRIBUTE_IDENT); //get the ident, if available
		if(responseLIDView!=null) //if we have an enclosing logical response view
		{
			responseLIDView.getResponseLabelViewMap().put(ident, this); //store ourselves in the logical ID response view map, keyed by ident
			responseLIDView.getButtonGroup().add(getRadioButton()); //add our radio button to the enclosing logical ID button group
		}
		add(getRadioButton(), BorderLayout.BEFORE_LINE_BEGINS); //add the radio button before this line begins
		final JButton submitButton=new JButton(submitAction); //create a new button for the submission
		add(submitButton, BorderLayout.AFTER_LAST_LINE); //add the button to the bottom of the form
*/
	}

}