package com.garretwilson.swing.rdf.maqro;

import java.awt.*;
import javax.swing.*;
import com.garretwilson.awt.BasicGridBagLayout;
import com.garretwilson.model.Model;
import com.garretwilson.rdf.maqro.*;
import com.garretwilson.swing.*;
import com.garretwilson.swing.border.BorderUtilities;

/**Panel for editing the behavior of an activity.
@author Garret Wilson
*/
public class ActivityBehaviorPanel extends ModelPanel
{

	private final PermissionsPanel permissionsPanel;
	private final ProcessPanel processPanel;
	private final FeedbackPanel feedbackPanel;

	/**@return The data model for which this component provides a view.
	@see ModelPanel#getModel()
	*/
	public ActivityModel getActivityModel() {return (ActivityModel)getModel();}

	/**Sets the data model.
	@param model The data model for which this component provides a view.
	@see #setModel(Model)
	*/
	public void setActivityModel(final ActivityModel model)
	{
		setModel(model);	//set the model
	}

	/**Sets the data model.
	@param newModel The data model for which this component provides a view.
	@exception ClassCastException Thrown if the model is not an <code>ActivityModel</code>.
	*/
	public void setModel(final Model newModel)
	{
		super.setModel((ActivityModel)newModel);	//set the model in the parent class
	}
	
	/**Default constructor.*/
	public ActivityBehaviorPanel()
	{
		this(new ActivityModel());	//construct the panel with a default model
	}

	/**Model constructor.
	@param model The data model for which this component provides a view.
	*/
	public ActivityBehaviorPanel(final ActivityModel model)
	{
		this(model, true);	//construct the panel and initialize it
	}

	/**Constructor with optional initialization.
	@param model The data model for which this component provides a view.
	@param initialize <code>true</code> if the panel should initialize itself by
		calling the initialization methods.
	*/
	public ActivityBehaviorPanel(final ActivityModel model, final boolean initialize)
	{
		super(new BasicGridBagLayout(), model, false);	//construct the panel using a grid bag layout, but don't initialize the panel
		permissionsPanel=new PermissionsPanel();
		processPanel=new ProcessPanel();
		feedbackPanel=new FeedbackPanel();
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

	/**Verifies the component.
	@return <code>true</code> if the component contents are valid, <code>false</code>
		if not.
	*/
/*G***fix
	public boolean verify()
	{
		if(onlyQuestionsRadioButton.isSelected())	//if the user wants to limit the number of questions
		{
			try
			{
				final int questionCount=Integer.parseInt(questionCountTextField.getText().trim());	//see if we can convert the question count string to an integer
				if(questionCount==0)	//if there are no questions requested
				{
					JOptionPane.showMessageDialog(this, "You must enter a positive number of questions.", "Invalid number of questions", JOptionPane.ERROR_MESSAGE);	//G***i18n
					questionCountTextField.requestFocusInWindow(); //focus on the question count text field
					return false; //show that verification failed				
				}
			}
			catch(NumberFormatException numberFormatException)	//if there was an error converting the string to an integer
			{
				JOptionPane.showMessageDialog(this, "You must enter a valid number of questions.", "Invalid number of questions", JOptionPane.ERROR_MESSAGE);	//G***i18n
				questionCountTextField.requestFocusInWindow(); //focus on the question count text field
				return false; //show that verification failed
			}
		}
		return super.verify();  //if we couldn't find any problems, verify the parent class
	}
*/

	/**Panel for editing the activity permissions.
	@author Garret Wilson
	*/
	protected static class PermissionsPanel extends ModifiablePanel
	{

		private final JCheckBox allowHintCheckBox;
		private final JCheckBox allowPreviousCheckBox;
		private final JCheckBox allowCancelCheckBox;

		/**Default constructor.*/
		public PermissionsPanel()
		{
			super(new BasicGridBagLayout(), false);	//construct the parent class but don't initialize it
			allowHintCheckBox=new JCheckBox();
			allowPreviousCheckBox=new JCheckBox();
			allowCancelCheckBox=new JCheckBox();
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
			add(allowHintCheckBox, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(allowPreviousCheckBox, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(allowCancelCheckBox, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		}
	}

	/**Panel for editing the activity process.
	@author Garret Wilson
	*/
	protected static class ProcessPanel extends ModifiablePanel
	{

		private final JCheckBox confirmCommitCheckBox;
		private final JCheckBox confirmSubmitCheckBox;
	//G***del if not needed; maybe replace with question:minResponseCount	private final JCheckBox requireResponseCheckBox;
		private final JCheckBox limitTimeCheckBox;
		private final JTextField maxTimeTextField;
		private final JLabel maxTimeUnitsLabel;

		/**Default constructor.*/
		public ProcessPanel()
		{
			super(new BasicGridBagLayout(), false);	//construct the parent class but don't initialize it
			confirmCommitCheckBox=new JCheckBox();
			confirmSubmitCheckBox=new JCheckBox();
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
			limitTimeCheckBox.setText("Limit time");	//G***i18n
			limitTimeCheckBox.addItemListener(createUpdateStatusItemListener());
			limitTimeCheckBox.addItemListener(getModifyItemListener());
			maxTimeTextField.getDocument().addDocumentListener(getModifyDocumentListener());
			maxTimeUnitsLabel.setText("milliseconds");	//G***i18n TODO use a special time selection control
			add(confirmCommitCheckBox, new GridBagConstraints(0, 0, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(confirmSubmitCheckBox, new GridBagConstraints(0, 1, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(limitTimeCheckBox, new GridBagConstraints(0, 2, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(maxTimeTextField, new GridBagConstraints(0, 3, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(maxTimeUnitsLabel, new GridBagConstraints(1, 3, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		}

		/**Updates the states of the actions, including enabled/disabled status,
			proxied actions, etc.
		*/
		public void updateStatus()
		{
			super.updateStatus();	//do the default updating
			maxTimeTextField.setEnabled(limitTimeCheckBox.isSelected());	//only enable the maximum time text field if the limit time checkbox is selected
		}

	}

	/**Panel for editing the activity feedback.
	@author Garret Wilson
	*/
	protected static class FeedbackPanel extends ModifiablePanel
	{

		private final JCheckBox showEachResultCheckBox;
		private final JCheckBox showFinalResultCheckBox;
		private final JCheckBox showResultProgressCheckBox;
		private final JCheckBox showProgressCheckBox;
		private final JCheckBox showTimeCheckBox;

		/**Default constructor.*/
		public FeedbackPanel()
		{
			super(new BasicGridBagLayout(), false);	//construct the parent class but don't initialize it
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

	}

}
