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

package com.globalmentor.swing.qti;

import java.awt.*;
import javax.swing.*;
import com.globalmentor.mentoract.qti.*;
import com.globalmentor.swing.*;

/**Provides a visual editing environment for a QTI choice rendering.
@author Garret Wilson
*/
public class QTIRenderChoicePanel extends QTIRenderPanel //TODO del implements ListSelectionListener
{
	/**Default constructor.*/
	public QTIRenderChoicePanel()
	{
		super();  //create the parent class
		jbInit(); //TODO fix
	}

	/**Initializes the user interface.*/
  private void jbInit()
  {
    label.setText("Choices"); //TODO fix
  }

	/**@return An empty choice render object.*/
	protected Render createRender()
	{
		return new RenderChoice();  //create a choice render object
	}


	/**Called when the selected choice changes.
	@param listSelectionEvent The event that gives information about the list
		selection.
	@see #updateActions
	*/
/*TODO del
  public void valueChanged(final ListSelectionEvent listSelectionEvent)
  {
		updateActions();  //update the actions
  }
*/

	/**Creates the actions that this component will use.*/
	public void createActions()
	{
		super.createActions();  //create the default actions
		  //update the add action properties to be specific to the type of rendering
		getAddResponseLabelAction().putValue(Action.NAME, "Add Choice...");
		getAddResponseLabelAction().putValue(Action.SHORT_DESCRIPTION, "Add choice");
		getAddResponseLabelAction().putValue(Action.LONG_DESCRIPTION, "Add a new choice.");
//TODO fix putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_P));  //set the mnemonic key TODO i18n
		  //update the remove action properties to be specific to the type of rendering
		getRemoveResponseLabelAction().putValue(Action.NAME, "Remove Choice...");
		getRemoveResponseLabelAction().putValue(Action.SHORT_DESCRIPTION, "Remove choice");
		getRemoveResponseLabelAction().putValue(Action.LONG_DESCRIPTION, "Remove the selected choice.");
//TODO fix putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_P));  //set the mnemonic key TODO i18n
		  //update the edit action properties to be specific to the type of rendering
		getEditResponseLabelAction().putValue(Action.NAME, "Edit Choice...");
		getEditResponseLabelAction().putValue(Action.SHORT_DESCRIPTION, "Edit choice");
		getEditResponseLabelAction().putValue(Action.LONG_DESCRIPTION, "Edit the selected choice.");
//TODO fix putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_P));  //set the mnemonic key TODO i18n
	}

	/**Creates a new response label.
	@return The new default response label.
	*/
	protected ResponseLabel createResponseLabel()
	{
		final ResponseLabel responseLabel=super.createResponseLabel();  //create the default response label
		final Render render=getRender();  //get the render object being represented
		responseLabel.setIdent(String.valueOf((char)('A'+render.getResponseLabelList().size()))); //set the ident for the new choice TODO fix to some standard method
		responseLabel.setMaterial(new Material());  //add default material
		return responseLabel;  //return the default response label we constructed
	}

	/**Brings up a dialog for editing the response label.
	@param responseLabel The response label to edit.
	@return The new, modified response label or <code>null</code> if editing was
		cancelled.
	*/
	protected ResponseLabel editResponseLabel(final ResponseLabel responseLabel)
	{
		final QTIResponseLabelPanel responseLabelPanel=new QTIResponseLabelPanel();  //create a new panel to edit the response label
		responseLabelPanel.setResponseLabel(responseLabel, null); //set the response label
		responseLabelPanel.setPreferredSize(new Dimension(300, 200));  //TODO fix preferred size QTI
		  //show the response label panel
		final int result=BasicOptionPane.showConfirmDialog(this, responseLabelPanel, "Response Label", JOptionPane.OK_CANCEL_OPTION);  //TODO i18n; comment
		return result==JOptionPane.OK_OPTION ? responseLabelPanel.getResponseLabel() : null; //return their response, or null if they cancelled
	}

	/**A custom renderer for the choices in the list.*/
/*TODO fix
	protected static class ChoiceListCellRenderer extends StringListCellRenderer
	{
*/
		/**@return The correct text for this choice.
		@param list The list component.
		@param value The value of this list item.
		@param index The index of this list item.
		@param cellHasFocus Whether the list item has the focus.
		*/
/*TODO fix
		protected String getListCellRendererString(final Object value)
		{
			final ChoiceLabel choiceLabel=(ChoiceLabel)value; //get the choice label
			return "["+choiceLabel.getIdent()+"] "+choiceLabel.getMaterial(); //return "[id] material"
		}
	}
*/

}
