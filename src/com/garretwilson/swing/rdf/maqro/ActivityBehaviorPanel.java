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

package com.garretwilson.swing.rdf.maqro;

import java.awt.*;
import java.io.IOException;
import javax.swing.*;
import com.garretwilson.awt.BasicGridBagLayout;
import com.garretwilson.awt.Containers;

import static com.globalmentor.urf.maqro.MAQRO.*;

import com.garretwilson.swing.ModelPanel;
import com.garretwilson.swing.border.Borders;
import com.globalmentor.net.ResourceModel;
import com.globalmentor.rdf.RDFResource;
import com.globalmentor.rdf.xmlschema.BooleanLiteral;
import com.globalmentor.rdf.xmlschema.IntegerLiteral;
import com.globalmentor.urf.maqro.*;

/**Panel for editing behavior-related MAQRO resource properties.
In most cases, the resource edited will be an activity.
@author Garret Wilson
@see Activity
*/
public class ActivityBehaviorPanel extends ModelPanel<ResourceModel<? extends RDFResource>>
{

	private final PermissionsPanel permissionsPanel;
	private final ProcessPanel processPanel;
	private final FeedbackPanel feedbackPanel;

	/**Sets the data model.
	@param newModel The data model for which this component provides a view.
	*/
	public void setModel(final ResourceModel<? extends RDFResource> newModel)
	{
		permissionsPanel.setModel(newModel);	//update the models of the child panels
		processPanel.setModel(newModel);	//update the models of the child panels
		feedbackPanel.setModel(newModel);	//update the models of the child panels
		super.setModel(newModel);	//set the model in the parent class
	}

	/**Model constructor.
	@param model The data model for which this component provides a view.
	*/
	public ActivityBehaviorPanel(final ResourceModel<? extends RDFResource> model)
	{
		this(model, true);	//construct the panel and initialize it
	}

	/**Constructor with optional initialization.
	@param model The data model for which this component provides a view.
	@param initialize <code>true</code> if the panel should initialize itself by
		calling the initialization methods.
	*/
	public ActivityBehaviorPanel(final ResourceModel<? extends RDFResource> model, final boolean initialize)
	{
		super(new BasicGridBagLayout(), model, false);	//construct the panel using a grid bag layout, but don't initialize the panel
		permissionsPanel=new PermissionsPanel(model);
		processPanel=new ProcessPanel(model);
		feedbackPanel=new FeedbackPanel(model);
		setDefaultFocusComponent(permissionsPanel);	//set the default focus component
		if(initialize)  //if we should initialize
			initialize();   //initialize the panel
	}
	
	/**Initializes the user interface.*/
	public void initializeUI()
	{
		super.initializeUI();	//do the default user interface initialization
		permissionsPanel.setBorder(Borders.createDefaultTitledBorder());
		permissionsPanel.setTitle("Permissions");	//TODO i18n
		processPanel.setBorder(Borders.createDefaultTitledBorder());
		processPanel.setTitle("Process");	//TODO i18n
		feedbackPanel.setBorder(Borders.createDefaultTitledBorder());
		feedbackPanel.setTitle("Feedback");	//TODO i18n
		add(permissionsPanel, new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.BOTH, Containers.NO_INSETS, 0, 0));
		add(processPanel, new GridBagConstraints(0, 1, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.BOTH, Containers.NO_INSETS, 0, 0));
		add(feedbackPanel, new GridBagConstraints(1, 0, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.BOTH, Containers.NO_INSETS, 0, 0));
	}

	/**Updates the states of the actions, including enabled/disabled status,
		proxied actions, etc.
	*/
/*TODO fix
	public void updateStatus()
	{
		super.updateStatus();	//do the default updating
			//only allow entry of a question count if the only questions radio button is selected
		questionCountTextField.setEnabled(onlyQuestionsRadioButton.isSelected());
			//only allow selection of categories if there are categories to select
		getSelectCategoriesAction().setEnabled(getAvailableCategorySet().size()>0);
	}
*/

	/**Loads the data from the model to the view, if necessary.
	@exception IOException Thrown if there was an error loading the model.
	*/
	public void loadModel() throws IOException
	{
		super.loadModel();	//do the default loading
		permissionsPanel.loadModel();	//load the child panels
		processPanel.loadModel();
		feedbackPanel.loadModel();
	}

	/**Stores the current data being edited to the model, if necessary.
	@exception IOException Thrown if there was an error saving the model.
	*/
	public void saveModel() throws IOException
	{
		super.saveModel();	//do the default saving
		permissionsPanel.saveModel();	//save the child panels
		processPanel.saveModel();
		feedbackPanel.saveModel();		
	}

	/**Panel for editing the activity permissions.
	@author Garret Wilson
	*/
	public static class PermissionsPanel extends ModelPanel<ResourceModel<? extends RDFResource>>
	{

		private final JCheckBox allowHintCheckBox;
		private final JCheckBox allowPreviousCheckBox;
		private final JCheckBox allowCancelCheckBox;
		private final JCheckBox allowSubmitCheckBox;

		/**Model constructor.
		@param model The data model for which this component provides a view.
		*/
		public PermissionsPanel(final ResourceModel<? extends RDFResource> model)
		{
			super(new BasicGridBagLayout(), model, false);	//construct the parent class but don't initialize it
			allowHintCheckBox=new JCheckBox();
			allowPreviousCheckBox=new JCheckBox();
			allowCancelCheckBox=new JCheckBox();
			allowSubmitCheckBox=new JCheckBox();
			initialize();   //initialize the panel
			setDefaultFocusComponent(allowHintCheckBox);
		}

		/**Initialize the user interface.*/
		protected void initializeUI()
		{
			super.initializeUI(); //do the default UI initialization
			allowHintCheckBox.setText("Allow hints");	//TODO i18n
			allowHintCheckBox.addItemListener(getModifyItemListener());
			allowPreviousCheckBox.setText("Allow previous");	//TODO i18n		
			allowPreviousCheckBox.addItemListener(getModifyItemListener());
			allowCancelCheckBox.setText("Allow cancel");	//TODO i18n
			allowCancelCheckBox.addItemListener(getModifyItemListener());
			allowSubmitCheckBox.setText("Allow submit");	//TODO i18n
			allowSubmitCheckBox.addItemListener(getModifyItemListener());
			add(allowHintCheckBox, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, Containers.NO_INSETS, 0, 0));
			add(allowPreviousCheckBox, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, Containers.NO_INSETS, 0, 0));
			add(allowCancelCheckBox, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, Containers.NO_INSETS, 0, 0));
			add(allowSubmitCheckBox, new GridBagConstraints(0, 3, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, Containers.NO_INSETS, 0, 0));
		}

		/**Loads the data from the model to the view, if necessary.
		@exception IOException Thrown if there was an error loading the model.
		*/
		public void loadModel() throws IOException
		{
			super.loadModel();	//do the default loading
			final RDFResource resource=getModel().getResource();	//get the resource
			if(resource!=null)	//if we have a resource
			{
				allowHintCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(ALLOW_HINT_PROPERTY_URI)));	//TODO convert to URF
				allowPreviousCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(ALLOW_PREVIOUS_PROPERTY_URI)));
				allowCancelCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(ALLOW_CANCEL_PROPERTY_URI)));
				allowSubmitCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(ALLOW_SUBMIT_PROPERTY_URI)));
			}
		}

		/**Stores the current data being edited to the model, if necessary.
		@exception IOException Thrown if there was an error saving the model.
		*/
		public void saveModel() throws IOException
		{
			super.saveModel();	//do the default saving
			final RDFResource resource=getModel().getResource();	//get the resource
			if(resource!=null)	//if we have a resource
			{
				resource.setProperty(ALLOW_HINT_PROPERTY_URI, new BooleanLiteral(allowHintCheckBox.isSelected()));
				resource.setProperty(ALLOW_PREVIOUS_PROPERTY_URI, new BooleanLiteral(allowPreviousCheckBox.isSelected()));
				resource.setProperty(ALLOW_CANCEL_PROPERTY_URI, new BooleanLiteral(allowCancelCheckBox.isSelected()));
				resource.setProperty(ALLOW_SUBMIT_PROPERTY_URI, new BooleanLiteral(allowSubmitCheckBox.isSelected()));
			}
		}
	}

	/**Panel for editing the activity process.
	@author Garret Wilson
	*/
	protected static class ProcessPanel extends ModelPanel<ResourceModel<? extends RDFResource>>
	{

		private final JCheckBox confirmCommitCheckBox;
		private final JCheckBox confirmSubmitCheckBox;
		private final JCheckBox requireResponseCheckBox;
		private final JCheckBox limitTimeCheckBox;
		private final JTextField maxTimeTextField;
		private final JLabel maxTimeUnitsLabel;

		/**Model constructor.
		@param model The data model for which this component provides a view.
		*/
		public ProcessPanel(final ResourceModel<? extends RDFResource> model)
		{
			super(new BasicGridBagLayout(), model, false);	//construct the parent class but don't initialize it
			confirmCommitCheckBox=new JCheckBox();
			confirmSubmitCheckBox=new JCheckBox();
			requireResponseCheckBox=new JCheckBox();
			limitTimeCheckBox=new JCheckBox();
			maxTimeTextField=new JTextField(8);
			maxTimeUnitsLabel=new JLabel();
			initialize();   //initialize the panel
			setDefaultFocusComponent(confirmCommitCheckBox);
		}

		/**Initialize the user interface.*/
		protected void initializeUI()
		{
			super.initializeUI(); //do the default UI initialization
			confirmCommitCheckBox.setText("Confirm commit");	//TODO i18n
			confirmCommitCheckBox.addItemListener(getModifyItemListener());
			confirmSubmitCheckBox.setText("Confirm submit");	//TODO i18n
			confirmSubmitCheckBox.addItemListener(getModifyItemListener());
			requireResponseCheckBox.setText("Require response");	//TODO i18n
			requireResponseCheckBox.addItemListener(getModifyItemListener());
			limitTimeCheckBox.setText("Limit time");	//TODO i18n
			limitTimeCheckBox.addItemListener(createUpdateStatusItemListener());
			limitTimeCheckBox.addItemListener(getModifyItemListener());
			maxTimeTextField.getDocument().addDocumentListener(getModifyDocumentListener());
			maxTimeUnitsLabel.setText("milliseconds");	//TODO i18n TODO use a special time selection control
			add(confirmCommitCheckBox, new GridBagConstraints(0, 0, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, Containers.NO_INSETS, 0, 0));
			add(confirmSubmitCheckBox, new GridBagConstraints(0, 1, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, Containers.NO_INSETS, 0, 0));
			add(requireResponseCheckBox, new GridBagConstraints(0, 2, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, Containers.NO_INSETS, 0, 0));
			add(limitTimeCheckBox, new GridBagConstraints(0, 3, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, Containers.NO_INSETS, 0, 0));
			add(maxTimeTextField, new GridBagConstraints(0, 4, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, Containers.NO_INSETS, 0, 0));
			add(maxTimeUnitsLabel, new GridBagConstraints(1, 4, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, Containers.NO_INSETS, 0, 0));
		}

		/**Updates the states of the actions, including enabled/disabled status,
			proxied actions, etc.
		*/
		public void updateStatus()
		{
			super.updateStatus();	//do the default updating
			maxTimeTextField.setEnabled(limitTimeCheckBox.isSelected());	//only enable the maximum time text field if the limit time checkbox is selected
		}

		/**Loads the data from the model to the view, if necessary.
		@exception IOException Thrown if there was an error loading the model.
		*/
		public void loadModel() throws IOException
		{
			super.loadModel();	//do the default loading
			final RDFResource resource=getModel().getResource();	//get the resource
			if(resource!=null)	//if we have a resource
			{
				confirmCommitCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(CONFIRM_COMMIT_PROPERTY_URI)));	//TODO conver to URF
				confirmSubmitCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(CONFIRM_SUBMIT_PROPERTY_URI)));
				requireResponseCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(REQUIRE_RESPONSE_PROPERTY_URI)));
				final long maxTime=IntegerLiteral.asLongValue(resource.getPropertyValue(MAX_TIME_PROPERTY_URI));
				if(maxTime>=0)	//if a maximum amount of time was specified
				{
					limitTimeCheckBox.setSelected(true);
					maxTimeTextField.setText(Long.toString(maxTime));	//show the maximum amount of time in the text field
				}
				else	//if no maximum time was specified
				{
					limitTimeCheckBox.setSelected(false);
					maxTimeTextField.setText("");
				}
			}
		}

		/**Stores the current data being edited to the model, if necessary.
		@exception IOException Thrown if there was an error saving the model.
		*/
		public void saveModel() throws IOException
		{
			super.saveModel();	//do the default saving
			final RDFResource resource=getModel().getResource();	//get the resource
			if(resource!=null)	//if we have a resource
			{
				resource.setProperty(CONFIRM_COMMIT_PROPERTY_URI, new BooleanLiteral(confirmCommitCheckBox.isSelected()));	//TODO convert to URF
				resource.setProperty(CONFIRM_SUBMIT_PROPERTY_URI, new BooleanLiteral(confirmSubmitCheckBox.isSelected()));
				resource.setProperty(REQUIRE_RESPONSE_PROPERTY_URI, new BooleanLiteral(requireResponseCheckBox.isSelected()));
				final IntegerLiteral maxTimeLiteral;
				if(limitTimeCheckBox.isSelected())	//if the user wishes to limit time
				{
					try
					{
						maxTimeLiteral=new IntegerLiteral(Integer.parseInt(maxTimeTextField.getText().trim()));	//get the entered time
					}
					catch(NumberFormatException numberFormatException)	//if there was an error converting the string to an integer
					{
						throw (IOException)new IOException(numberFormatException.getMessage()).initCause(numberFormatException);
					}					
				}
				else	//if the user doesn't want to limit time
				{
					maxTimeLiteral=null;	//don't limit the time
				}
				resource.setProperty(MAX_TIME_PROPERTY_URI, maxTimeLiteral);
			}
		}

		/**Verifies the component.
		@return <code>true</code> if the component contents are valid, <code>false</code>
			if not.
		*/
		public boolean verify()
		{
			if(limitTimeCheckBox.isSelected())	//if the user wants to limit time
			{
				try
				{
					final int time=Integer.parseInt(maxTimeTextField.getText().trim());	//see if we can convert the max time string to an integer
					if(time<=0)	//if the time limit is not positive
					{
						JOptionPane.showMessageDialog(this, "You must enter a positive amount of time in milliseconds.", "Invalid time limit", JOptionPane.ERROR_MESSAGE);	//TODO i18n
						maxTimeTextField.requestFocusInWindow(); //focus on the maximum time text field
						return false; //show that verification failed				
					}
				}
				catch(NumberFormatException numberFormatException)	//if there was an error converting the string to an integer
				{
					JOptionPane.showMessageDialog(this, "You must enter a valid amount of time in milliseconds.", "Invalid time limit", JOptionPane.ERROR_MESSAGE);	//TODO i18n
					maxTimeTextField.requestFocusInWindow(); //focus on the maximum time text field
					return false; //show that verification failed
				}
			}
			return super.verify();  //if we couldn't find any problems, verify the parent class
		}	
	}

	/**Panel for editing the activity feedback.
	@author Garret Wilson
	*/
	protected static class FeedbackPanel extends ModelPanel<ResourceModel<? extends RDFResource>>
	{

		private final JCheckBox showEachResultCheckBox;
		private final JCheckBox showFinalResultCheckBox;
		private final JCheckBox showResultProgressCheckBox;
		private final JCheckBox showProgressCheckBox;
		private final JCheckBox showTimeCheckBox;

		/**Model constructor.
		@param model The data model for which this component provides a view.
		*/
		public FeedbackPanel(final ResourceModel<? extends RDFResource> model)
		{
			super(new BasicGridBagLayout(), model, false);	//construct the parent class but don't initialize it
			showEachResultCheckBox=new JCheckBox();
			showFinalResultCheckBox=new JCheckBox();
			showResultProgressCheckBox=new JCheckBox();
			showProgressCheckBox=new JCheckBox();
			showTimeCheckBox=new JCheckBox();
			initialize();   //initialize the panel
			setDefaultFocusComponent(showEachResultCheckBox);
		}

		/**Initialize the user interface.*/
		protected void initializeUI()
		{
			super.initializeUI(); //do the default UI initialization
			showEachResultCheckBox.setText("Show each interaction result");	//TODO i18n
			showEachResultCheckBox.addItemListener(getModifyItemListener());
			showFinalResultCheckBox.setText("Show final activity result");	//TODO i18n
			showFinalResultCheckBox.addItemListener(getModifyItemListener());
			showResultProgressCheckBox.setText("Continuously show results");	//TODO i18n
			showResultProgressCheckBox.addItemListener(getModifyItemListener());
			showProgressCheckBox.setText("Show progress");	//TODO i18n
			showProgressCheckBox.addItemListener(getModifyItemListener());
			showTimeCheckBox.setText("Show time");	//TODO i18n
			showTimeCheckBox.addItemListener(getModifyItemListener());
			add(showEachResultCheckBox, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, Containers.NO_INSETS, 0, 0));
			add(showFinalResultCheckBox, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, Containers.NO_INSETS, 0, 0));
			add(showResultProgressCheckBox, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, Containers.NO_INSETS, 0, 0));
			add(showProgressCheckBox, new GridBagConstraints(0, 3, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, Containers.NO_INSETS, 0, 0));
			add(showTimeCheckBox, new GridBagConstraints(0, 4, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, Containers.NO_INSETS, 0, 0));
		}

		/**Loads the data from the model to the view, if necessary.
		@exception IOException Thrown if there was an error loading the model.
		*/
		public void loadModel() throws IOException
		{
			super.loadModel();	//do the default loading
			final RDFResource resource=getModel().getResource();	//get the resource
			if(resource!=null)	//if we have a resource
			{
				showEachResultCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(REPORT_EACH_RESULT_PROPERTY_URI)));	//TODO convert to URF
				showFinalResultCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(REPORT_FINAL_RESULT_PROPERTY_URI)));
				showResultProgressCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(REPORT_RESULT_PROGRESS_PROPERTY_URI)));
				showProgressCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(REPORT_PROGRESS_PROPERTY_URI)));
				showTimeCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(REPORT_TIME_PROPERTY_URI)));
			}
		}

		/**Stores the current data being edited to the model, if necessary.
		@exception IOException Thrown if there was an error saving the model.
		*/
		public void saveModel() throws IOException
		{
			super.saveModel();	//do the default saving
			final RDFResource resource=getModel().getResource();	//get the resource
			if(resource!=null)	//if we have a resource
			{
				resource.setProperty(REPORT_EACH_RESULT_PROPERTY_URI, new BooleanLiteral(showEachResultCheckBox.isSelected()));	//TODO convert to URF
				resource.setProperty(REPORT_FINAL_RESULT_PROPERTY_URI, new BooleanLiteral(showFinalResultCheckBox.isSelected()));
				resource.setProperty(REPORT_RESULT_PROGRESS_PROPERTY_URI, new BooleanLiteral(showResultProgressCheckBox.isSelected()));
				resource.setProperty(REPORT_PROGRESS_PROPERTY_URI, new BooleanLiteral(showProgressCheckBox.isSelected()));
				resource.setProperty(REPORT_TIME_PROPERTY_URI, new BooleanLiteral(showTimeCheckBox.isSelected()));
			}
		}
	}

}
