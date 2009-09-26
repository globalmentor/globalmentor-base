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
import java.util.*;

import com.globalmentor.log.Log;
import com.globalmentor.mentoract.qti.*;
import com.globalmentor.swing.*;

/**Allows input of the answer to a logical ID response type.
@author Garret Wilson
*/
public class QTIResponseLIDAnswerPanel extends JPanel
{

	/**The logical ID response with which the response processing is associated.*/
	protected ResponseLID responseLID=null;

  GridBagLayout gridBagLayout = new GridBagLayout();
  JLabel answerLabel = new JLabel();
  ButtonGroup selectButtonGroup = new ButtonGroup();
  JScrollPane answerScrollPane = new JScrollPane();
  JList answerList = new JList();
	DefaultListModel answerListModel=new DefaultListModel();
  JPanel selectPanel = new JPanel();
  GridBagLayout selectBagLayout = new GridBagLayout();
  JRadioButton selectAllRadioButton = new JRadioButton();
  JRadioButton selectAnyRadioButton = new JRadioButton();
  JLabel scoreLabel = new JLabel();
  JTextField scoreTextField = new JTextField();  //TODO testing

	/**Default constructor.*/
	public QTIResponseLIDAnswerPanel()
	{
		jbInit();
	}

	/**Initializes the user interface.*/
  private void jbInit()
  {
    this.setLayout(gridBagLayout);
    answerLabel.setText("Answer");
		answerList.setModel(new DefaultListModel());  //use a default list model in the choice list
		answerList.setUI(new ToggleListUI()); //allow the answers to be toggled on and off
		answerList.setCellRenderer(new CheckBoxListCellRenderer());  //display the answers with checkboxes
/*TODO fix

		answerListModel.addElement(new JRadioButton("Test radio button 1"));  //TODO testing
		answerListModel.addElement(new JRadioButton("Test radio button 2"));
		answerListModel.addElement(new JRadioButton("Test radio button 3"));
		answerList.setModel(answerListModel);
		answerList.setUI(new ToggleListUI()); //TODO testing
		answerList.setCellRenderer(new CheckBoxListCellRenderer());  //TODO testing
*/

    selectPanel.setLayout(selectBagLayout);
    selectAllRadioButton.setText("All");
    selectAnyRadioButton.setText("Any");
    scoreLabel.setText("Score");
    scoreTextField.setColumns(4);
    this.add(answerLabel,      new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    this.add(answerScrollPane,                new GridBagConstraints(0, 1, 2, 1, 1.0, 1.0
            ,GridBagConstraints.NORTHWEST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
    this.add(selectPanel,      new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 8, 0, 0), 0, 0));
    answerScrollPane.getViewport().add(answerList, null);
    selectPanel.add(selectAllRadioButton,     new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    selectPanel.add(selectAnyRadioButton,      new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    this.add(scoreLabel,    new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 4, 0, 0), 0, 0));
    selectButtonGroup.add(selectAnyRadioButton);
    selectButtonGroup.add(selectAllRadioButton);
    this.add(scoreTextField,    new GridBagConstraints(2, 1, 1, 1, 0.0, 0.0
            ,GridBagConstraints.NORTHWEST, GridBagConstraints.HORIZONTAL, new Insets(0, 4, 0, 0), 0, 0));
  }

	/**@return The response label being edited in the panel.*/
/*TODO fix
	public ResponseLabel getResponseLabel()
	{
		final ResponseLabel responseLabel=new ResponseLabel();  //create a new response label
		responseLabel.setIdent(identTextField.getText()); //get the ident
		responseLabel.setMaterial(materialPanel.getMaterial()); //get the material
		//G**update the other properties
		return responseLabel;  //return the response label
	}
*/

	/**Sets the response label that appears in the panel.
	@param responseLabel The response label that should be represented by the panel.
	*/
/*TODO fix
	public void setResponseLabel(final ResponseLabel responseLabel)
	{
		identTextField.setText(responseLabel.getIdent()); //set the ident text
		materialPanel.setMaterial(responseLabel.getMaterial()); //set the material
	}
*/

	/**@return The response processing being edited in the panel.*/
	public ResponseProcessing getResponseProcessing()
	{
//TODO del		final ResponseProcessing responseProcessing;  //we'll create response processing if there are selections
		if(answerList.getSelectedIndex()>=0) //if there is at least one answer selected
		{
			final ResponseProcessing responseProcessing=new ResponseProcessing();  //create new response processing information
			final DecVar scoreDecVar=new DecVar(); //create a variable declaration for the score
			responseProcessing.getOutcomes().getDecVarList().add(scoreDecVar);  //add the score variable declaration TODO check all this
			final ResponseCondition responseCondition=new ResponseCondition("correct"); //create a new response condition section TODO i18n
			final ConditionVar conditionVar=new ConditionVar();  //create a new condition group
			for(int i=0; i<answerList.getModel().getSize(); ++i) //look at each of the response labels listed in the model
			{
				final ResponseLabel responseLabel=(ResponseLabel)answerList.getModel().getElementAt(i); //get this response label from the model
				if(answerList.isSelectedIndex(i)) //if this response label is selected
				{
					final VarEqual varEqual=new VarEqual(); //create a new variable equality
					if(responseLID!=null) //if we know the logical ID response with which this condition is associated
						varEqual.setResponseIdent(responseLID.getIdent());  //associate the condition with the response
					varEqual.setValue(responseLabel.getIdent());  //show which response label this answer refers to
					conditionVar.getConditionList().add(varEqual);  //add this variable equality to the condition group
				}
			}
				//if the "select all" radio button is selected, show that all the conditions of the condition group must be met
			conditionVar.setAllConditionsRequired(selectAllRadioButton.isSelected());
			responseCondition.getConditionVarList().add(conditionVar);  //add the condition group to the response condition
			final SetVar setVar=new SetVar(); //create a new variable setting
			setVar.setAction(SetVar.SET_ACTION);  //show that the variable should be set
				//TODO check the value of the setvar, and decide what we'll do about all this string stuff---shouldn't our data model be more typesafe?
			setVar.setValue(scoreTextField.getText());  //show the amount to set this variable
			responseCondition.getSetVarList().add(setVar);  //add the variable setting to the condition section
			responseProcessing.getResponseConditionList().add(responseCondition); //add the response condition to the responsen processing information
			//G**update the other properties
			return responseProcessing;  //return the response processing we created
		}
		else  //if there is no answer selected
			return null;  //show that there is no response processing
	}

	/**Sets the response processing that appears in the panel.
	@param responseLID The logical ID response with which the response processing
		is associated.
	@param responseProcessing The response processing to edit.
	*/  //TODO eventually we may want to pass the entire presentation, as it may contain multiple responseLIDs
	public void setResponseProcessing(final ResponseLID responseLID, final ResponseProcessing responseProcessing)
	{
		this.responseLID=responseLID; //store the logical ID response for later so that we can correctly set the respident when retrieving the response processing TODO if we later allow the user to change the responseLID ident, this will need to be changed
		final DefaultListModel answerListModel=(DefaultListModel)answerList.getModel();  //get the list model of answers
		answerListModel.clear();  //clear all the values in the list model
		if(responseLID.getRenderList().size()>0)  //if there is at least one render type
		{
			final Render render=(Render)responseLID.getRenderList().get(0); //get the first render type TODO assert there is at least one
//TODO del				final RenderChoice renderChoice=(RenderChoice)render; //cast the render to a render choice
			final Iterator responseLabelIterator=render.getResponseLabelList().iterator(); //get an iterator to the response labels
			while(responseLabelIterator.hasNext()) //while there are more response labels
			{
				answerListModel.addElement(responseLabelIterator.next());  //add the next response label to the list model
			}
			if(responseProcessing!=null)  //if there is response processing information present
			{
					//we'll match up all response condition variables with the choices
					//get an iterator to look through all the response conditions
				final Iterator responseConditionIterator=responseProcessing.getResponseConditionList().iterator();
				while(responseConditionIterator.hasNext())  //while there are more response conditions
				{
					final ResponseCondition responseCondition=(ResponseCondition)responseConditionIterator.next();  //get the next response condition
		//TODO check the setvar condition to make sure this is for the score
					if(responseCondition.getSetVarList().size()>0) //if there are variables being set in response to these conditions
					{
						final SetVar setVar=(SetVar)responseCondition.getSetVarList().get(0); //get the first setvar TODO change to iterate through the setvars to find the one for SCORE
						scoreTextField.setText(setVar.getValue());  //show the value of the score in the score text field
					}
					final Iterator conditionVarIterator=responseCondition.getConditionVarList().iterator(); //get an iterator to look through the conditions
					while(conditionVarIterator.hasNext())  //while there are more conditions sets TODO clarify with QTI WG why there could be multiple condition sets
					{
						//TODO fix; we only use one conditionvar group
						final ConditionVar conditionVar=(ConditionVar)conditionVarIterator.next();  //get the next condition group
						if(conditionVar.isAllConditionsRequired())  //if all the conditions are required
							selectAllRadioButton.setSelected(true); //show that all conditions must be met
						else  //if not all conditions are required
							selectAnyRadioButton.setSelected(true); //show that any conditions can be met
						final Iterator conditionIterator=conditionVar.getConditionList().iterator();  //get an iterator to look through the list of conditions
						while(conditionIterator.hasNext())  //while there are more conditions
						{
							final Condition condition=(Condition)conditionIterator.next(); //get the next condition
	//TODO make sure this is a varequal
							final String responseLabelID=condition.getValue(); //get the condition value
	Log.trace("found condition variable with response label ID: ", responseLabelID);  //TODO del
								//try to locate a matching response
							final ResponseLabel responseLabel=render.getResponseLabel(responseLabelID);
							if(responseLabel!=null) //if this condition matches a response label
							{
								final int responseLabelIndex=answerListModel.indexOf(responseLabel);  //get the index of the response label
								answerList.addSelectionInterval(responseLabelIndex, responseLabelIndex);  //TODO testing
//TODO fix								  answerList.setSelectedValue(responseLabel, true); //TODO testing
							}
						}
					}
				}
			}
			else  //if there is no response processing
			{
				selectAnyRadioButton.setSelected(true); //default to allowing any of the conditions to be met
				scoreTextField.setText("1");  //default to a score of one TODO maybe use a constant here
			}
		}
	}
}