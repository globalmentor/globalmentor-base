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

package com.garretwilson.swing.qti;

import java.awt.*;
import java.beans.*;
import javax.swing.*;
import com.globalmentor.mentoract.qti.Item;
import com.globalmentor.mentoract.qti.Presentation;
import com.globalmentor.mentoract.qti.Response;
import com.globalmentor.mentoract.qti.ResponseLID;
import com.globalmentor.mentoract.qti.ResponseProcessing;
import com.globalmentor.model.Modifiable;
import com.globalmentor.model.Verifiable;

/**Provides a visual editing environment for a QTI item.
@author Garret Wilson
*/
public class QTIItemPanel extends JPanel implements Verifiable
{
  GridBagLayout gridBagLayout = new GridBagLayout();
  JTabbedPane tabbedPane = new JTabbedPane();
//TODO fix  JPanel presentationPanel = new JPanel();
  JLabel itemLabel = new JLabel();
  JLabel identLabel = new JLabel();
  JTextField identTextField = new JTextField();
  BorderLayout presentationBorderLayout = new BorderLayout();
  JPanel presentationPanel = new JPanel();
  JPanel responseProcessingPanel = new JPanel();
  JPanel metadataPanel = new JPanel();

	/**Default constructor.*/
	public QTIItemPanel()
	{
		jbInit();
	}

	/**Constructs a panel with the information from the given item.
	@param item The item to display in the panel.
	*/
	public QTIItemPanel(final Item item)
	{
		this(); //do the default construction
		setItem(item);  //set the item
	}

	/**Setup the user interface.*/
  private void jbInit()
  {
    this.setLayout(gridBagLayout);
    itemLabel.setText("Item");
    identLabel.setText("ID");
    identTextField.setText("identTextField");
    identTextField.setColumns(16);
//TODO fix    presentationPanel.setLayout(presentationBorderLayout);
    this.add(tabbedPane,                  new GridBagConstraints(0, 1, 3, 1, 1.0, 0.25
            ,GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
    tabbedPane.add(presentationPanel,  "Presentation");
    this.add(itemLabel,        new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    this.add(identLabel,    new GridBagConstraints(1, 0, 1, 1, 1.0, 0.0
            ,GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    this.add(identTextField,          new GridBagConstraints(2, 0, 1, 1, 0.2, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
    tabbedPane.add(responseProcessingPanel,  "Response Processing");
    tabbedPane.add(metadataPanel,  "Metadata");
  }

	/**@return The item being edited in the panel.*/
	public Item getItem()
	{
		final Item item=new Item();  //create a new item
		item.setIdent(identTextField.getText()); //get the ident
		//TODO set title and such
			//get the presentation
		final Presentation presentation=((QTIPresentationPanel)tabbedPane.getComponentAt(0)).getPresentation();
//TODO del		final Presentation presentation=((QTIPresentationPanel)presentationPanel.getComponent(0)).getPresentation();
//TODO del Log.trace("Getting presentation with first response label: ", ((RenderChoice)((ResponseLID)presentation.getResponseList().get(0)).getRenderList().get(0)).getResponseLabelList().get(0)); //TODO del
		item.setPresentation(presentation);  //set the presentation
			//get the response processing
		final ResponseProcessing responseProcessing=getResponseProcessing();
//TODO del		final Presentation presentation=((QTIPresentationPanel)presentationPanel.getComponent(0)).getPresentation();
//TODO del Log.trace("Getting presentation with first response label: ", ((RenderChoice)((ResponseLID)presentation.getResponseList().get(0)).getRenderList().get(0)).getResponseLabelList().get(0)); //TODO del
		item.setResponseProcessing(responseProcessing);  //set the response processing
		//G**update the other properties
		return item;  //return the item
	}

	/**@return The response processing currently being edited.*/
	protected ResponseProcessing getResponseProcessing()
	{
			//TODO make sure there is a responseLID answer panel
		return ((QTIResponseLIDAnswerPanel)tabbedPane.getComponentAt(1)).getResponseProcessing(); //TODO use a constant here
	}

	/**Sets the item that appears in the panel.
	@param item The QTI item that should be represented by the panel.
	*/
	public void setItem(final Item item)
	{
		//TODO what if this item has no presentation
	  identTextField.setText(item.getIdent()); //set the ID
		final QTIPresentationPanel presentationPanel=new QTIPresentationPanel();  //create a panel for the presentation
		presentationPanel.setPresentation(item.getPresentation()); //set the presentation shown in the panel

		  //TODO testing; fix for other things besides logical ID responses
		assert (QTIResponseLIDPanel)presentationPanel.getResponsePanel(0) instanceof QTIResponseLIDPanel : "Response panel is not a QTIResponseLIDPanel";
		final QTIResponseLIDPanel responseLIDPanel=(QTIResponseLIDPanel)presentationPanel.getResponsePanel(0);
		final QTIRenderPanel renderPanel=responseLIDPanel.getRenderPanel(0);  //get the first render panel TODO fix to work with all render panels
		  //add a property listener so that we can update the answer list when the choices are modified
		renderPanel.addPropertyChangeListener(Modifiable.MODIFIED_PROPERTY, new java.beans.PropertyChangeListener()
    {
      public void propertyChange(final PropertyChangeEvent propertyChangeEvent) //if the "modified" property changes in the explore panel
      {
				if(Boolean.TRUE.equals(propertyChangeEvent.getNewValue()))  //if the panel has been modified
				{
					updateResponseProcessing();  //update the answers
					renderPanel.setModified(false); //show that we've recognized the modifications, and we don't consider the panel modified anymore
				}
      }
    });


		tabbedPane.setComponentAt(tabbedPane.indexOfComponent(this.presentationPanel), presentationPanel);  //TODO testing
//TODO fix		this.presentationPanel.removeAll();  //remove all components from the main presentation panel
//TODO fix    this.presentationPanel.add(presentationPanel, BorderLayout.CENTER);  //add the presentation panel containing the information to the main presentation panel TODO do we want to just replace the render choice panel, or perhaps use a borderlayout?
		if(item.getPresentation().getResponseList().size()>0) //if there is at least one response TODO fix for multiple responses
		{
		  final Response response=(Response)item.getPresentation().getResponseList().get(0);  //get the first response
			if(response instanceof ResponseLID)  //if this is a logical ID response
			{
				final ResponseLID responseLID=(ResponseLID)response; //cast the response to a logical ID response
				final QTIResponseLIDAnswerPanel responseProcessingPanel=new QTIResponseLIDAnswerPanel(); //create an answer panel for the logical ID response
				responseProcessingPanel.setResponseProcessing(responseLID, item.getResponseProcessing()); //set the response processing in the panel
				tabbedPane.setComponentAt(tabbedPane.indexOfComponent(this.responseProcessingPanel), responseProcessingPanel);  //TODO testing
			}
			//TODO fix for other response types
		}
	}

/*TODO fix
	protected void setResponseProcessing(final ResponseProcessing responseProcessing)
	{


	}
*/

	/**Updates the response processing to match the available choices.*/
	protected void updateResponseProcessing()
	{
//TODO del Log.trace("updating response processing");  //TODO del
		final Item item=getItem();  //get the item being edited
		if(item.getPresentation().getResponseList().size()>0) //if there is at least one response TODO fix for multiple responses
		{
//TODO del Log.trace("found response");
		  final Response response=(Response)item.getPresentation().getResponseList().get(0);  //get the first response
			if(response instanceof ResponseLID)  //if this is a logical ID response
			{
//TODO del Log.trace("found response LID");
				final ResponseLID responseLID=(ResponseLID)response; //cast the response to a logical ID response
				final QTIResponseLIDAnswerPanel responseProcessingPanel=((QTIResponseLIDAnswerPanel)tabbedPane.getComponentAt(1));  //TODO testing; use a constant here; comment
				responseProcessingPanel.setResponseProcessing(responseLID, item.getResponseProcessing()); //set the response processing in the panel
			}
			//TODO fix for other response types
		}
			//get the response processing
//TODO del		final ResponseProcessing responseProcessing=getResponseProcessing();
	}

	/**Verifies the component.
	@return <code>true</code> if the component contents are valid, <code>false</code>
		if not.
	*/
	public boolean verify()
	{
		if(identTextField.getText().length()==0)  //if there is no ID
		{
			JOptionPane.showMessageDialog(this, "Each item must have a unique identifier.", "Missing ident", JOptionPane.ERROR_MESSAGE);	//TODO i18n
			identTextField.requestFocus(); //focus on the ID text field
			return false; //show that verification failed
		}
		return true;  //if we couldn't find any problems, verification succeeded
//TODO change this panel to inherit from BasicPanel, and verify the parent class (removing the implements Verifiable)		return super.verify();  //if we couldn't find any problems, verify the parent class
	}

}