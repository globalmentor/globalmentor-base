package com.garretwilson.swing.rdf.maqro;

import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.event.ActionListener;
import java.awt.event.ItemListener;
import java.io.IOException;
import java.util.List;
import javax.swing.*;
import com.garretwilson.awt.BasicGridBagLayout;
import com.garretwilson.rdf.maqro.*;
import com.garretwilson.resources.icon.IconResources;
import com.garretwilson.swing.*;
import com.garretwilson.swing.border.BorderUtilities;

/**Panel for editing a MAQRO question.
@author Garret Wilson
*/
public class QuestionPanel extends TabbedViewPanel
{
	/**The view in which the query and choices and/or answers are shown.*/
	public final static int QUERY_MODEL_VIEW=-1;

	/**The default model views supported by this panel.*/
	private final int[] DEFAULT_SUPPORTED_MODEL_VIEWS=new int[]{QUERY_MODEL_VIEW};

	/**The default default model view of this panel.*/
	private final int DEFAULT_DEFAULT_MODEL_VIEW=QUERY_MODEL_VIEW;

	/**The tab in which the query and choices and/or answers are shown.*/
	private final QueryAnswerPanel queryAnswerPanel;

		/**@return The tab in which the query and choices and/or answers are shown.*/
		private QueryAnswerPanel getQueryAnswerPanel() {return queryAnswerPanel;}

	/**@return The data model for which this component provides a view.
	@see ModelViewablePanel#getModel()
	*/
	public QuestionModel getQuestionModel() {return (QuestionModel)getModel();}

	/**Sets the data model.
	@param model The data model for which this component provides a view.
	@see ModelViewablePanel#setModel(Model)
	*/
	public void setQuestionModel(final QuestionModel model) {setModel(model);}

	/**Model constructor.
	@param model The data model for which this component provides a view.
	*/
	public QuestionPanel(final QuestionModel model)
	{
		this(model, true);	//construct and initialize the panel
	}

	/**Model constructor with optional initialization.
	@param model The data model for which this component provides a view.
	@param initialize <code>true</code> if the panel should initialize itself by
		calling the initialization methods.
	*/
	public QuestionPanel(final QuestionModel model, final boolean initialize)
	{
		super(model, false);	//construct the parent class without initializing the panel
		setSupportedModelViews(DEFAULT_SUPPORTED_MODEL_VIEWS);	//set the model views we support
		setDefaultDataView(DEFAULT_DEFAULT_MODEL_VIEW);	//set the default data view
		queryAnswerPanel=new QueryAnswerPanel();	//create the query/answer
		if(initialize)  //if we should initialize
			initialize();   //initialize the panel
	}

	/**Initialize the user interface.*/
	protected void initializeUI()
	{
		setBorder(BorderUtilities.createDefaultTitledBorder());	//set a titled border
		setTitle("Question");	//G***i18n
		addView(QUERY_MODEL_VIEW, "Query and Response", IconResources.getIcon(IconResources.QUESTION_ICON_FILENAME), queryAnswerPanel);	//add the query view G***i18n
		super.initializeUI(); //do the default UI initialization
		getTabbedPane().setTabPlacement(JTabbedPane.TOP);	//put the tabs on the top



//G***del when works		queryAnswerPanel.addPropertyChangeListener(createModifyModifiedChangeListener());	//show that this panel is modified whenever the query/answer panel is modified 
	}

	/**Updates the states of the actions, including enabled/disabled status,
		proxied actions, etc.
	*/
/*G***del if not needed
	protected void updateStatus()
	{
		super.updateStatus();	//do the default updating
//G***fix for expectation		choiceList.setEnabled(choicesRadioButton.isSelected());	//only enable the choice list if the choices radio button is selected
	}
*/

	/**Loads the data from the model to the view, if necessary.
	@exception IOException Thrown if there was an error loading the model.
	*/
	protected void loadModel() throws IOException
	{
		super.loadModel();	//do the default loading
		final QuestionModel model=getQuestionModel();	//get the data model
		final Question question=model.getQuestion();	//get the question, if there is one
		switch(getModelView())	//see which view of data we should load
		{
			case QUERY_MODEL_VIEW:	//if we're changing to the query view
				if(question!=null)	//if there is a question
				{
					final Dialogue query=question.getQuery();	//get the query
					queryAnswerPanel.queryPanel.setDialogueModel(new DialogueModel(query, model.getBaseURI(), model.getURIInputStreamable()));	//set the query in the panel
					final List choices=question.getChoices();	//get the question choices
					if(choices!=null)	//if there are choices
					{
						queryAnswerPanel.choicesRadioButton.setSelected(true);	//show that we're selecting choices
						queryAnswerPanel.choiceList.setModel(new ListListModel(choices));	//set the choices in the panel
					}
				}
				break;
		}
	}

	/**Stores the current data being edited to the model, if necessary.
	@exception IOException Thrown if there was an error loading the model.
	*/
	protected void saveModel() throws IOException
	{
		super.saveModel();	//do the default saving
		final QuestionModel model=getQuestionModel();	//get the data model
		switch(getModelView())	//see which view of data we have, in order to get the current RDF
		{
			case QUERY_MODEL_VIEW:	//if we should store the query
				break;
		}
	}

	/**Indicates that the view of the data has changed.
	@param oldView The view before the change.
	@param newView The new view of the data
	*/
	protected void onModelViewChange(final int oldView, final int newView)
	{
		super.onModelViewChange(oldView, newView);	//perform the default functionality
		final QuestionModel model=getQuestionModel();	//get the data model
		switch(oldView)	//see which view we're changing from
		{
			case QUERY_MODEL_VIEW:	//if we're changing from the query view
//G***fix				getSourceTextPane().getDocument().removeDocumentListener(getModifyDocumentListener());	//don't listen for changes to the source text pane any more
				queryAnswerPanel.queryPanel.setDialogueModel(new DialogueModel(model.getBaseURI(), model.getURIInputStreamable()));	//clear the query panel
				break;
		}
		switch(newView)	//see which view we're changing to
		{
/*G***fix
			case SOURCE_MODEL_VIEW:	//if we're changing to the source view
				getSourceTextPane().getDocument().addDocumentListener(getModifyDocumentListener());	//add ourselves as a document listener to see if the source pane is modified
				break;
*/
		}
	}

	/**Panel for editing the query and answer of a MAQRO question.
	Usually used as one tab on a question panel.
	@author Garret Wilson
	@see com.garretwilson.swing.rdf.maqro.QuestionPanel
	*/
	protected class QueryAnswerPanel extends ModifiablePanel
	{
		private final JLabel queryLabel;
		private final DialoguePanel queryPanel;
		private final ButtonGroup expectButtonGroup;
		private final JRadioButton choicesRadioButton;
		private final JList choiceList;
		private final ListPanel choicePanel;
		private final JRadioButton expectRadioButton;

		/**Default constructor.*/
		public QueryAnswerPanel()
		{
			super(new BasicGridBagLayout(), false);	//construct the parent class but don't initialize it
			queryLabel=new JLabel();
			expectButtonGroup=new ButtonGroup();
			choicesRadioButton=new JRadioButton();
			choiceList=new JList();
			choicePanel=new ListPanel(choiceList);
			expectRadioButton=new JRadioButton();
			queryPanel=new DialoguePanel(new DialogueModel(getQuestionModel().getBaseURI(), getQuestionModel().getURIInputStreamable())); 
			initialize();   //initialize the panel
		}

		/**Initialize the user interface.*/
		protected void initializeUI()
		{
			super.initializeUI(); //do the default UI initialization
			final ItemListener updateStatusItemListener=createUpdateStatusItemListener();	//create an item listener that will update the status			
			queryLabel.setText("Query");	//G***i18n
			expectButtonGroup.add(choicesRadioButton);
			expectButtonGroup.add(expectRadioButton);
			choicesRadioButton.setText("Provide Choices");	//G***i18n
			choicesRadioButton.addItemListener(updateStatusItemListener); 
			choiceList.setUI(new ToggleListUI()); //allow the choices to be toggled on and off
			choiceList.setCellRenderer(new CheckBoxListCellRenderer());  //display the choices with checkboxes
			choiceList.setEnabled(false);	//default to disabling the choice list; it will be enabled if the corresponding radio button is selected
			choicePanel.setBorder(BorderUtilities.createDefaultTitledBorder());	//set a titled border for the choice panel
			choicePanel.setTitle("Choices");	//G***i18n
			choicePanel.setEditor(new ChoiceEditor());	//set the editor for choices
			choicePanel.setEditable(true);	//allow the choices to be edited
			expectRadioButton.setText("Expect Response Type");	//G***i18n
			expectRadioButton.addItemListener(updateStatusItemListener); 
			add(queryLabel, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.SOUTHWEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(queryPanel, new GridBagConstraints(0, 1, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.BOTH, NO_INSETS, 0, 0));
			add(choicesRadioButton, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0, GridBagConstraints.SOUTHWEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(choicePanel, new GridBagConstraints(0, 3, 1, 1, 0.0, 0.0, GridBagConstraints.NORTHWEST, GridBagConstraints.BOTH, NO_INSETS, 0, 0));
			add(expectRadioButton, new GridBagConstraints(0, 4, 1, 1, 0.0, 0.0, GridBagConstraints.SOUTHWEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		}

		/**Updates the states of the actions, including enabled/disabled status,
			proxied actions, etc.
		*/
		protected void updateStatus()
		{
			super.updateStatus();	//do the default updating
			choiceList.setEnabled(choicesRadioButton.isSelected());	//only enable the choice list if the choices radio button is selected
//	G***fix for expectation		choiceList.setEnabled(choicesRadioButton.isSelected());	//only enable the choice list if the choices radio button is selected
		}
	}

	/**The editor that allows editing of choices from a list.
	@author Garret Wilson
	*/
	protected static class ChoiceEditor extends ListPanel.AbstractEditor
	{
		/**Default constructor.*/
		public ChoiceEditor()
		{
			super(Dialogue.class);	//construct the parent class
		}

		/**Edits an object from the list.
		@param parentComponent The component to use as a parent for any editing
			components.
		@param item The item to edit in the list.
		@return The object with the modifications from the edit, or
			<code>null</code> if the edits should not be accepted.
		*/
		public Object edit(final Component parentComponent, final Object item)
		{
			if(item instanceof Dialogue)	//if this is dialogue to be edited
			{
				final Dialogue dialogueClone=(Dialogue)((Dialogue)item).clone();	//create a clone of the dialogue
				final DialogueModel dialogueModel=new DialogueModel(dialogueClone);	//create a model containing the dialogue
				final DialoguePanel dialoguePanel=new DialoguePanel(dialogueModel);	//construct a panel in which to edit the dialogue
					//allow the dialogue to be edited in a dialog box; if the user accepts the changes
				if(OptionPane.showConfirmDialog(parentComponent, dialoguePanel, "Choice", OptionPane.OK_CANCEL_OPTION)==OptionPane.OK_OPTION)	//G***i18n
				{
					return dialogueClone;	//return the new dialogue
				}
			}
			return null;	//show that editing did not succeed
		}
	}

}
