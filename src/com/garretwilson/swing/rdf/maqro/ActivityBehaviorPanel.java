package com.garretwilson.swing.rdf.maqro;

import java.awt.*;
import java.io.IOException;
import javax.swing.*;
import com.garretwilson.awt.BasicGridBagLayout;
import com.garretwilson.model.ResourceModel;

import static com.globalmentor.rdf.maqro.MAQRO.*;

import com.garretwilson.swing.ModelPanel;
import com.garretwilson.swing.border.BorderUtilities;
import com.globalmentor.rdf.RDFResource;
import com.globalmentor.rdf.maqro.*;
import com.globalmentor.rdf.xmlschema.BooleanLiteral;
import com.globalmentor.rdf.xmlschema.IntegerLiteral;

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
		permissionsPanel.setBorder(BorderUtilities.createDefaultTitledBorder());
		permissionsPanel.setTitle("Permissions");	//G***i18n
		processPanel.setBorder(BorderUtilities.createDefaultTitledBorder());
		processPanel.setTitle("Process");	//G***i18n
		feedbackPanel.setBorder(BorderUtilities.createDefaultTitledBorder());
		feedbackPanel.setTitle("Feedback");	//G***i18n
		add(permissionsPanel, new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.BOTH, NO_INSETS, 0, 0));
		add(processPanel, new GridBagConstraints(0, 1, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.BOTH, NO_INSETS, 0, 0));
		add(feedbackPanel, new GridBagConstraints(1, 0, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.BOTH, NO_INSETS, 0, 0));
	}

	/**Updates the states of the actions, including enabled/disabled status,
		proxied actions, etc.
	*/
/*G***fix
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
			allowHintCheckBox.setText("Allow hints");	//G***i18n
			allowHintCheckBox.addItemListener(getModifyItemListener());
			allowPreviousCheckBox.setText("Allow previous");	//G***i18n		
			allowPreviousCheckBox.addItemListener(getModifyItemListener());
			allowCancelCheckBox.setText("Allow cancel");	//G***i18n
			allowCancelCheckBox.addItemListener(getModifyItemListener());
			allowSubmitCheckBox.setText("Allow submit");	//G***i18n
			allowSubmitCheckBox.addItemListener(getModifyItemListener());
			add(allowHintCheckBox, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(allowPreviousCheckBox, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(allowCancelCheckBox, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(allowSubmitCheckBox, new GridBagConstraints(0, 3, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
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
				allowHintCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(MAQRO_NAMESPACE_URI, ALLOW_HINT_PROPERTY_NAME)));
				allowPreviousCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(MAQRO_NAMESPACE_URI, ALLOW_PREVIOUS_PROPERTY_NAME)));
				allowCancelCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(MAQRO_NAMESPACE_URI, ALLOW_CANCEL_PROPERTY_NAME)));
				allowSubmitCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(MAQRO_NAMESPACE_URI, ALLOW_SUBMIT_PROPERTY_NAME)));
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
				resource.setProperty(MAQRO_NAMESPACE_URI, ALLOW_HINT_PROPERTY_NAME, new BooleanLiteral(allowHintCheckBox.isSelected()));
				resource.setProperty(MAQRO_NAMESPACE_URI, ALLOW_PREVIOUS_PROPERTY_NAME, new BooleanLiteral(allowPreviousCheckBox.isSelected()));
				resource.setProperty(MAQRO_NAMESPACE_URI, ALLOW_CANCEL_PROPERTY_NAME, new BooleanLiteral(allowCancelCheckBox.isSelected()));
				resource.setProperty(MAQRO_NAMESPACE_URI, ALLOW_SUBMIT_PROPERTY_NAME, new BooleanLiteral(allowSubmitCheckBox.isSelected()));
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
			confirmCommitCheckBox.setText("Confirm commit");	//G***i18n
			confirmCommitCheckBox.addItemListener(getModifyItemListener());
			confirmSubmitCheckBox.setText("Confirm submit");	//G***i18n
			confirmSubmitCheckBox.addItemListener(getModifyItemListener());
			requireResponseCheckBox.setText("Require response");	//G***i18n
			requireResponseCheckBox.addItemListener(getModifyItemListener());
			limitTimeCheckBox.setText("Limit time");	//G***i18n
			limitTimeCheckBox.addItemListener(createUpdateStatusItemListener());
			limitTimeCheckBox.addItemListener(getModifyItemListener());
			maxTimeTextField.getDocument().addDocumentListener(getModifyDocumentListener());
			maxTimeUnitsLabel.setText("milliseconds");	//G***i18n TODO use a special time selection control
			add(confirmCommitCheckBox, new GridBagConstraints(0, 0, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(confirmSubmitCheckBox, new GridBagConstraints(0, 1, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(requireResponseCheckBox, new GridBagConstraints(0, 2, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(limitTimeCheckBox, new GridBagConstraints(0, 3, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(maxTimeTextField, new GridBagConstraints(0, 4, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(maxTimeUnitsLabel, new GridBagConstraints(1, 4, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
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
				confirmCommitCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(MAQRO_NAMESPACE_URI, CONFIRM_COMMIT_PROPERTY_NAME)));
				confirmSubmitCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(MAQRO_NAMESPACE_URI, CONFIRM_SUBMIT_PROPERTY_NAME)));
				requireResponseCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(MAQRO_NAMESPACE_URI, REQUIRE_RESPONSE_PROPERTY_NAME)));
				final long maxTime=IntegerLiteral.asLongValue(resource.getPropertyValue(MAQRO_NAMESPACE_URI, MAX_TIME_PROPERTY_NAME));
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
				resource.setProperty(MAQRO_NAMESPACE_URI, CONFIRM_COMMIT_PROPERTY_NAME, new BooleanLiteral(confirmCommitCheckBox.isSelected()));
				resource.setProperty(MAQRO_NAMESPACE_URI, CONFIRM_SUBMIT_PROPERTY_NAME, new BooleanLiteral(confirmSubmitCheckBox.isSelected()));
				resource.setProperty(MAQRO_NAMESPACE_URI, REQUIRE_RESPONSE_PROPERTY_NAME, new BooleanLiteral(requireResponseCheckBox.isSelected()));
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
				resource.setProperty(MAQRO_NAMESPACE_URI, MAX_TIME_PROPERTY_NAME, maxTimeLiteral);
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
						JOptionPane.showMessageDialog(this, "You must enter a positive amount of time in milliseconds.", "Invalid time limit", JOptionPane.ERROR_MESSAGE);	//G***i18n
						maxTimeTextField.requestFocusInWindow(); //focus on the maximum time text field
						return false; //show that verification failed				
					}
				}
				catch(NumberFormatException numberFormatException)	//if there was an error converting the string to an integer
				{
					JOptionPane.showMessageDialog(this, "You must enter a valid amount of time in milliseconds.", "Invalid time limit", JOptionPane.ERROR_MESSAGE);	//G***i18n
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
			showEachResultCheckBox.setText("Show each interaction result");	//G***i18n
			showEachResultCheckBox.addItemListener(getModifyItemListener());
			showFinalResultCheckBox.setText("Show final activity result");	//G***i18n
			showFinalResultCheckBox.addItemListener(getModifyItemListener());
			showResultProgressCheckBox.setText("Continuously show results");	//G***i18n
			showResultProgressCheckBox.addItemListener(getModifyItemListener());
			showProgressCheckBox.setText("Show progress");	//G***i18n
			showProgressCheckBox.addItemListener(getModifyItemListener());
			showTimeCheckBox.setText("Show time");	//G***i18n
			showTimeCheckBox.addItemListener(getModifyItemListener());
			add(showEachResultCheckBox, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(showFinalResultCheckBox, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(showResultProgressCheckBox, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(showProgressCheckBox, new GridBagConstraints(0, 3, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(showTimeCheckBox, new GridBagConstraints(0, 4, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
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
				showEachResultCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(MAQRO_NAMESPACE_URI, SHOW_EACH_RESULT_PROPERTY_NAME)));
				showFinalResultCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(MAQRO_NAMESPACE_URI, SHOW_FINAL_RESULT_PROPERTY_NAME)));
				showResultProgressCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(MAQRO_NAMESPACE_URI, SHOW_RESULT_PROGRESS_PROPERTY_NAME)));
				showProgressCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(MAQRO_NAMESPACE_URI, SHOW_PROGRESS_PROPERTY_NAME)));
				showTimeCheckBox.setSelected(BooleanLiteral.asBooleanValue(resource.getPropertyValue(MAQRO_NAMESPACE_URI, SHOW_TIME_PROPERTY_NAME)));
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
				resource.setProperty(MAQRO_NAMESPACE_URI, SHOW_EACH_RESULT_PROPERTY_NAME, new BooleanLiteral(showEachResultCheckBox.isSelected()));
				resource.setProperty(MAQRO_NAMESPACE_URI, SHOW_FINAL_RESULT_PROPERTY_NAME, new BooleanLiteral(showFinalResultCheckBox.isSelected()));
				resource.setProperty(MAQRO_NAMESPACE_URI, SHOW_RESULT_PROGRESS_PROPERTY_NAME, new BooleanLiteral(showResultProgressCheckBox.isSelected()));
				resource.setProperty(MAQRO_NAMESPACE_URI, SHOW_PROGRESS_PROPERTY_NAME, new BooleanLiteral(showProgressCheckBox.isSelected()));
				resource.setProperty(MAQRO_NAMESPACE_URI, SHOW_TIME_PROPERTY_NAME, new BooleanLiteral(showTimeCheckBox.isSelected()));
			}
		}
	}

}
