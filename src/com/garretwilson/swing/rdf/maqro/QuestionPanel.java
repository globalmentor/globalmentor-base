package com.garretwilson.swing.rdf.maqro;

import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.event.ItemListener;
import java.io.IOException;
import java.util.*;
import javax.swing.*;
import com.garretwilson.awt.BasicGridBagLayout;
import com.garretwilson.model.Resource;
import com.garretwilson.rdf.*;
import com.garretwilson.rdf.maqro.*;
import com.garretwilson.resources.icon.IconResources;
import com.garretwilson.swing.*;
import com.garretwilson.swing.border.BorderUtilities;
import com.garretwilson.util.CollectionUtilities;

/**Panel for editing a MAQRO question.
@author Garret Wilson
*/
public class QuestionPanel extends TabbedViewPanel
{
	/**The view in which the query and choices and/or answers are shown.*/
	public final static int QUERY_MODEL_VIEW=-1;
	/**The view in which the explanations are shown.*/
	public final static int EXPLANATION_MODEL_VIEW=-2;

	/**The default model views supported by this panel.*/
	private final int[] DEFAULT_SUPPORTED_MODEL_VIEWS=new int[]{QUERY_MODEL_VIEW, EXPLANATION_MODEL_VIEW};

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

	private final JList explanationSwingList;
	private final ListPanel explanationPanel;

	/**The list of explanations for this question.*/
	private final ListListModel explanationList;

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
		explanationList=new ListListModel(new ArrayList());	//create a list in which to store the explanations
		explanationSwingList=new JList(explanationList);
		explanationPanel=new ListPanel(explanationSwingList, new ExplanationEditStrategy());
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
		addView(EXPLANATION_MODEL_VIEW, "Explanations", IconResources.getIcon(IconResources.INFO_ICON_FILENAME), explanationPanel);	//add the explanation view G***i18n
		super.initializeUI(); //do the default UI initialization
		getTabbedPane().setTabPlacement(JTabbedPane.TOP);	//put the tabs on the top
		explanationSwingList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);	//only allow one explanation to be selected at a time
		explanationPanel.setEditable(true);	//allow the choices to be edited
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

	/**Loads the data from the model to the specified view, if necessary.
	@param modelView The view of the data, such as <code>SUMMARY_MODEL_VIEW</code>.
	@exception IOException Thrown if there was an error loading the model.
	*/
	protected void loadModel(final int modelView) throws IOException
	{
		super.loadModel(modelView);	//do the default loading
		final QuestionModel model=getQuestionModel();	//get the data model
		final Question question=model.getQuestion();	//get the question, if there is one
		switch(modelView)	//see which view of data we should load
		{
			case QUERY_MODEL_VIEW:	//if we're changing to the query view
				if(question!=null)	//if there is a question
				{
					final Dialogue query=question.getQuery();	//get the query
					queryAnswerPanel.queryPanel.setDialogueModel(new DialogueModel(query, model.getBaseURI(), model));	//set the query in the panel
					final Resource expectation=question.getExpect();	//get the expectation
					queryAnswerPanel.answerPanel.setExpectation(expectation);	//show the expectation in the combo box, even if there isn't an expectation
					final List choices=question.getChoices();	//get the question choices
					if(choices!=null)	//if there are choices
					{
						queryAnswerPanel.choicesRadioButton.setSelected(true);	//show that we're expecting choices
						final int maxResponseCount=question.getMaxResponseCount();	//find out the maximum responses allowed
						queryAnswerPanel.mutuallyExclusiveCheckBox.setSelected(maxResponseCount==1);	//set mutual exclusivity if only one response is allowed
						queryAnswerPanel.choiceList.setModel(new ListListModel(choices));	//set the choices in the panel
						boolean requireAll=false;	//start out assuming we won't require all the answers
						final Iterator answerIterator=question.getAnswerIterator();	//get an iterator to the answers
						while(!requireAll && answerIterator.hasNext())	//while there are other answers, and we haven't yet discovered that we require all the answers
						{
							final Object answer=answerIterator.next();	//get the first answer
							if(answer instanceof Dialogue)	//if the answer is dialogue, as we expect
							{
								final int index=choices.indexOf(answer);	//see which of the choices this answer is
								if(index>=0)	//if this answer is one of the choices
								{
									queryAnswerPanel.choiceList.addSelectionInterval(index, index);	//select this choice									
								}
							}
							else if(answer instanceof RDFListResource)	//if the answer is a list, forget about the other answers (if there were any) and require all of these answers
							{
								requireAll=true;	//we now know that we require all the answers
								queryAnswerPanel.choiceList.clearSelection();	//clear all selections from the list						
								final Iterator requiredAnswerIterator=((RDFListResource)answer).iterator();	//get an iterator to the required answers
								while(requiredAnswerIterator.hasNext())	//while there are other required answers
								{
									final Object requiredAnswer=requiredAnswerIterator.next();	//get the next required answer
									if(requiredAnswer instanceof Dialogue)	//if the answer is dialogue, as we expect
									{
										final int index=choices.indexOf(requiredAnswer);	//see which of the choices this answer is
										if(index>=0)	//if this answer is one of the choices
										{
											queryAnswerPanel.choiceList.addSelectionInterval(index, index);	//select this choice									
										}
									}
								}
							}
						}
					}
					else	//if there are no choices, set the first answer in the answer panel
					{
						queryAnswerPanel.choiceList.setModel(new ListListModel(new RDFListResource(RDFConstants.NIL_RESOURCE_URI)));	//create a default empty list for the choices
						queryAnswerPanel.expectRadioButton.setSelected(true);	//show that we're expecting an answer
						final Iterator answerIterator=question.getAnswerIterator();	//get an iterator to the answers
						if(answerIterator.hasNext())	//if there is at least one answer
						{
							final Object answerObject=answerIterator.next();	//get the first answer
							if(answerObject instanceof Dialogue)	//if the answer is dialogue, as we expect
							{
								queryAnswerPanel.answerPanel.setAnswer((Dialogue)answerObject);	//show the answer in the answer panel
							}
							queryAnswerPanel.answerPanel.answerCheckBox.setSelected(true);	//enable the answer
						}
						else	//if there is no answer
						{
							queryAnswerPanel.answerPanel.answerCheckBox.setSelected(false);	//disable the answer
						}
					}
				}
				break;
			case EXPLANATION_MODEL_VIEW:	//if we're changing to the explanation view
				if(question!=null)	//if there is a question
				{			
					explanationList.clear();	//clear the list of explanations
					CollectionUtilities.addAll(explanationList, question.getExplanationIterator());	//add the explanations to the list
				}
		}
	}

	/**Stores the current data being edited to the model, if necessary.
	@param modelView The view of the data, such as <code>SUMMARY_MODEL_VIEW</code>.
	@exception IOException Thrown if there was an error saving the model.
	*/
	protected void saveModel(final int modelView) throws IOException
	{
		super.saveModel(modelView);	//do the default saving
		final QuestionModel model=getQuestionModel();	//get the data model
		final Question question=model.getQuestion()!=null ? model.getQuestion() : new Question();	//get the question, if there is one; if not, create one
		switch(modelView)	//see which view of data we have, in order to get the current RDF
		{
			case QUERY_MODEL_VIEW:	//if we should store the query
				final Dialogue query=queryAnswerPanel.queryPanel.getDialogueModel().getDialogue();	//get the query from the panel
				question.setQuery(query);	//update the query
				question.removeAnswers();	//remove all answers from the question
				int maxResponseCount=-1;	//default to unlimited responses allowed
				if(queryAnswerPanel.choicesRadioButton.isSelected())	//if we should expect choices
				{
					question.setExpect(null);	//show that we don't expect any answer type in particular
					question.setChoices((RDFListResource)((ListListModel)queryAnswerPanel.choiceList.getModel()).getList());	//make sure the question knows of the choices
					final Object[] answers=queryAnswerPanel.choiceList.getSelectedValues();	//get the selected answers
					if(queryAnswerPanel.requireAllCheckBox.isSelected())	//see if we should require all the answers
					{
						final RDFListResource answerList=new RDFListResource();	//create an empty list for the answers
						CollectionUtilities.addAll(answerList, answers);	//add all the selected answers to the answer list
						question.addAnswer(answerList);	//add the list of answers to the question
					}
					else	//if we shouldn't require all the answers
					{
						for(int i=answers.length-1; i>=0; --i)	//look at each selected answer (order doesn't matter)
						{
							final Object answer=answers[i];	//get this answer
							if(answer instanceof Dialogue)	//if this answer is dialogue (it always should be)
							{
								question.addAnswer((Dialogue)answer);	//add this answer to the question
							}
						}
					}
					maxResponseCount=queryAnswerPanel.mutuallyExclusiveCheckBox.isSelected() ? 1 : -1;	//determine the maximum response count---mutual exclusivity allows only one answer
				}
				else if(queryAnswerPanel.expectRadioButton.isSelected())	//if we should expect a particular answer type
				{
					question.setChoices(null);	//remove all choices
					question.setExpect(queryAnswerPanel.answerPanel.getExpectation());	//update the expectation TODO make sure the expectation has been removed
					final Dialogue answer=queryAnswerPanel.answerPanel.getAnswer();	//get the answer
					if(answer!=null)	//if an answer is specified
						question.addAnswer(answer);	//add the answer (we cleared all the answers earlier)
					maxResponseCount=1;	//only allow one response for an expected type G***allow more later
				}
				question.setMaxResponseCount(maxResponseCount);	//update the maximum responses allowed (which may remove the limit altogether)
				model.setQuestion(question);	//put the question in the model, if it isn't there already
				break;
			case EXPLANATION_MODEL_VIEW:	//if we should store the explanations
				question.removeExplanations();	//remove all explanations from the question
				final Iterator explanationIterator=explanationList.iterator();	//get an iterator to the entered explanations
				while(explanationIterator.hasNext())	//while there are more explanations
				{
					final Object explanation=explanationIterator.next();	//get the next explanation
					if(explanation instanceof Dialogue)	//if this explanation is dialogue (it always should be)
					{
						question.addExplanation((Dialogue)explanation);	//add this explanation dialogue
					}
				}
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
				queryAnswerPanel.queryPanel.setDialogueModel(new DialogueModel(model.getBaseURI(), model));	//clear the query panel
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
	@see QuestionPanel
	*/
	protected class QueryAnswerPanel extends ModifiablePanel
	{

		private final JLabel queryLabel;
		private final DialoguePanel queryPanel;
		private final ButtonGroup expectButtonGroup;
		private final JRadioButton choicesRadioButton;
		private final JCheckBox mutuallyExclusiveCheckBox;
		private final JCheckBox requireAllCheckBox;
		private final JList choiceList;
		private final ListPanel choicePanel;
		private final JRadioButton expectRadioButton;
		private final AnswerPanel answerPanel;

		/**Default constructor.*/
		public QueryAnswerPanel()
		{
			super(new BasicGridBagLayout(), false);	//construct the parent class but don't initialize it
			queryLabel=new JLabel();
			queryPanel=new DialoguePanel(new DialogueModel(getQuestionModel().getBaseURI(), getQuestionModel())); 
			expectButtonGroup=new ButtonGroup();
			choicesRadioButton=new JRadioButton();
			mutuallyExclusiveCheckBox=new JCheckBox();
			requireAllCheckBox=new JCheckBox();
			choiceList=new JList(new ListListModel(new RDFListResource(RDFConstants.NIL_RESOURCE_URI)));	//create a default empty list for the choices
			choicePanel=new ListPanel(choiceList, new ChoiceEditStrategy());
			expectRadioButton=new JRadioButton();
			answerPanel=new AnswerPanel();
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
			choicesRadioButton.addItemListener(getModifyItemListener());
			mutuallyExclusiveCheckBox.setText("Mutually exclusive");	//G***i18n
			mutuallyExclusiveCheckBox.setSelected(true);	//default to mutually exclusive choices
			mutuallyExclusiveCheckBox.addItemListener(updateStatusItemListener);
			mutuallyExclusiveCheckBox.addItemListener(getModifyItemListener());
			requireAllCheckBox.setText("Require all correct choices");	//G***i18n
			requireAllCheckBox.addItemListener(updateStatusItemListener);
			requireAllCheckBox.addItemListener(getModifyItemListener());
			choiceList.setUI(new ToggleListUI()); //allow the choices to be toggled on and off
			choiceList.setCellRenderer(new CheckBoxListCellRenderer());  //display the choices with checkboxes
			choiceList.setEnabled(false);	//default to disabling the choice list; it will be enabled if the corresponding radio button is selected
			choicePanel.setBorder(BorderUtilities.createDefaultTitledBorder());	//set a titled border for the choice panel
			choicePanel.setTitle("Choices and Answers");	//G***i18n
			choicePanel.setEditable(true);	//allow the choices to be edited
			expectRadioButton.setText("Expect Response Type");	//G***i18n
			expectRadioButton.addItemListener(updateStatusItemListener); 
			choicesRadioButton.addItemListener(getModifyItemListener()); 
			answerPanel.setBorder(BorderUtilities.createDefaultTitledBorder());	//set a titled border for the answer panel
			answerPanel.setTitle("Expectation and Answer");	//G***i18n
			final Insets choiceAnswerInsets=new Insets(0, 32, 0, 0);
			add(queryLabel, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(queryPanel, new GridBagConstraints(0, 1, 3, 1, 1.0, 1.0, GridBagConstraints.WEST, GridBagConstraints.BOTH, NO_INSETS, 0, 0));
			add(choicesRadioButton, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(mutuallyExclusiveCheckBox, new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(requireAllCheckBox, new GridBagConstraints(2, 2, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(choicePanel, new GridBagConstraints(0, 3, 3, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, choiceAnswerInsets, 0, 0));
			add(expectRadioButton, new GridBagConstraints(0, 4, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
			add(answerPanel, new GridBagConstraints(0, 5, 3, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, choiceAnswerInsets, 0, 0));
		}

		/**Updates the states of the actions, including enabled/disabled status,
			proxied actions, etc.
		*/
		public void updateStatus()
		{
			super.updateStatus();	//do the default updating
			final boolean isChoicesSelected=choicesRadioButton.isSelected();
			mutuallyExclusiveCheckBox.setEnabled(isChoicesSelected);
			requireAllCheckBox.setEnabled(isChoicesSelected && ! mutuallyExclusiveCheckBox.isSelected());	//don't allow requiring all if only one choice can be selected

			choicePanel.setVisible(isChoicesSelected);	//G***testing
			
			choiceList.setEnabled(isChoicesSelected);	//only enable the choice list if the choices radio button is selected
			if(mutuallyExclusiveCheckBox.isSelected())	//if the choices should be mutually exclusive
			{
				choiceList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);	//only allow a single selection
				if(!(choiceList.getCellRenderer() instanceof RadioButtonListCellRenderer))	//make sure the choice list uses radio buttons
				{
					choiceList.setCellRenderer(new RadioButtonListCellRenderer());  //display the choices with radio buttons					
				}
				if(choiceList.getMinSelectionIndex()!=choiceList.getMaxSelectionIndex())	//if more than one choice is selected
				{
					choiceList.setSelectedIndex(choiceList.getSelectedIndex());	//select the first choice to clear the others
				}
			}
			else	//if the choices allow multiple selections
			{
				choiceList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);	//only multiple selections over multiple intervals
				if(!(choiceList.getCellRenderer() instanceof CheckBoxListCellRenderer))	//make sure the choice list uses checkboxes
				{
					choiceList.setCellRenderer(new CheckBoxListCellRenderer());  //display the choices with checkboxes
				}
			}
			choicePanel.updateStatus();	//update the status of the choice panel, in case we changed the selection mode
			answerPanel.setEnabled(expectRadioButton.isSelected());	//only enable the answer panel if the expect radio button is selected

			answerPanel.setVisible(expectRadioButton.isSelected());	//G***testing
			
		}

		/**The edit strategy that allows editing of choices from a list.
		@author Garret Wilson
		*/
		protected class ChoiceEditStrategy extends ListEditStrategy
		{
			/**Default constructor.*/
			public ChoiceEditStrategy()
			{
				super(choiceList, QueryAnswerPanel.this);	//construct the parent class
			}

			/**Creates a new default object to be edited.
			@return The new default object.
			@exception IllegalAccessException Thrown if the class or its nullary 
				constructor is not accessible.
			@exception InstantiationException Thrown if a class represents an abstract
				class, an interface, an array class, a primitive type, or void;
				or if the class has no nullary constructor; or if the instantiation fails
				for some other reason.
			*/
			protected Object createItem() throws InstantiationException, IllegalAccessException
			{
				return new Dialogue();
			}

			/**Edits an object from the list.
			@param parentComponent The component to use as a parent for any editing
				components.
			@param item The item to edit in the list.
			@return The object with the modifications from the edit, or
				<code>null</code> if the edits should not be accepted.
			*/
			protected Object editItem(final Object item)
			{
				if(item instanceof Dialogue)	//if this is dialogue to be edited
				{
					final Dialogue dialogueClone=(Dialogue)((Dialogue)item).clone();	//create a clone of the dialogue
					final DialogueModel dialogueModel=new DialogueModel(dialogueClone);	//create a model containing the dialogue
					final DialoguePanel dialoguePanel=new DialoguePanel(dialogueModel);	//construct a panel in which to edit the dialogue
						//allow the dialogue to be edited in a dialog box; if the user accepts the changes
					if(OptionPane.showConfirmDialog(getParentComponent(), dialoguePanel, "Choice", OptionPane.OK_CANCEL_OPTION)==OptionPane.OK_OPTION)	//G***i18n
					{
						return dialogueClone;	//return the new dialogue
					}
				}
				return null;	//show that editing did not succeed
			}
		}

	}

	/**The edit strategy that allows editing of explanations from a list.
	@author Garret Wilson
	*/
	protected class ExplanationEditStrategy extends ListEditStrategy
	{
		/**Default constructor.*/
		public ExplanationEditStrategy()
		{
			super(explanationSwingList, QuestionPanel.this);	//construct the parent class
		}

		/**Creates a new default object to be edited.
		@return The new default object.
		@exception IllegalAccessException Thrown if the class or its nullary 
			constructor is not accessible.
		@exception InstantiationException Thrown if a class represents an abstract
			class, an interface, an array class, a primitive type, or void;
			or if the class has no nullary constructor; or if the instantiation fails
			for some other reason.
		*/
		protected Object createItem() throws InstantiationException, IllegalAccessException
		{
			return new Dialogue();
		}

		/**Edits an object from the list.
		@param item The item to edit in the list.
		@return The object with the modifications from the edit, or
			<code>null</code> if the edits should not be accepted.
		*/
		protected Object editItem(final Object item)
		{
			if(item instanceof Dialogue)	//if this is dialogue to be edited
			{
				final Dialogue dialogueClone=(Dialogue)((Dialogue)item).clone();	//create a clone of the dialogue
				final DialogueModel dialogueModel=new DialogueModel(dialogueClone);	//create a model containing the dialogue
				final DialoguePanel dialoguePanel=new DialoguePanel(dialogueModel);	//construct a panel in which to edit the dialogue
					//allow the dialogue to be edited in a dialog box; if the user accepts the changes
				if(OptionPane.showConfirmDialog(getParentComponent(), dialoguePanel, "Explanation", OptionPane.OK_CANCEL_OPTION)==OptionPane.OK_OPTION)	//G***i18n
				{
					return dialogueClone;	//return the new dialogue
				}
			}
			return null;	//show that editing did not succeed
		}
	}

}
