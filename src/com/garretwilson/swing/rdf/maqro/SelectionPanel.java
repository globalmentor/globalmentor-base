package com.garretwilson.swing.rdf.maqro;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;
import javax.swing.*;
import com.garretwilson.awt.BasicGridBagLayout;
import com.garretwilson.rdf.maqro.*;
import com.garretwilson.resources.icon.IconResources;
import com.garretwilson.swing.*;
import com.garretwilson.swing.border.BorderUtilities;
import com.garretwilson.util.*;

/**Panel for editing a MAQRO activity selection.
@author Garret Wilson
*/
public class SelectionPanel extends BasicPanel
{

	private final JLabel questionCountLabel;
	private final JCheckBox randomCheckBox;
	private final ButtonGroup questionCountButtonGroup;
	private final JRadioButton allQuestionsRadioButton;
	private final JRadioButton onlyQuestionsRadioButton;
	private final JTextField questionCountTextField;
	private final JButton categoriesButton;

	/**The action for selecting categories.*/
	private final Action selectCategoriesAction;

		/**@return The action for selecting categories.*/
		protected Action getSelectCategoriesAction() {return selectCategoriesAction;}

	/**@return Selection criteria for selecting interactions.*/
	public Selection getSelection()
	{
		final Selection selection=new Selection();	//create new selection criteria
		selection.setRandom(randomCheckBox.isSelected());	//specify whether the selection should be random
		if(onlyQuestionsRadioButton.isSelected())	//if the user wants to limit the number of questions
		{
			selection.setQuestionCount(Integer.parseInt(questionCountTextField.getText().trim()));	//get the interaction count
		}
		return selection;	//return the selection criteria we constructed
	}

	/**Sets the selection criteria to show in the panel.
	@param selection The selection criteria.
	*/
	public void setSelection(final Selection selection)
	{
		randomCheckBox.setSelected(selection.isRandom());	//if this is a random selection, select the random checkbox
		final int questionCount=selection.getQuestionCount();	//get the questionCount
		if(questionCount>=0)	//if a valid question count was given
		{
			onlyQuestionsRadioButton.setSelected(true);	//select the only questions radio button
			questionCountTextField.setText(Integer.toString(questionCount));	//show the question count
		}
		else	//if all questions should be selected
		{
			allQuestionsRadioButton.setSelected(true);	//select the all questions radio button
			questionCountTextField.setText("");	//clear the question count
		}
		categorySet.clear();	//clear our categories
		CollectionUtilities.addAll(categorySet, selection.getCategoryIterator());	//add all selection categories to our set of categories
		updateStatus();	//update our status to reflect the new selection
	}

	/**The set of avaiable categories.*/
	private final Set availableCategorySet;

		/**@return The set of available categories.*/
		public Set getAvailableCategorySet() {return availableCategorySet;}

		/**Sets the available categories, keeping the categories but not the given set.
		@param set The set of available categories.
		*/
		public void setAvailableCategorySet(final Set set)
		{
			availableCategorySet.clear();	//clear our available categories
			availableCategorySet.addAll(set);	//add all the given categories
			categorySet.retainAll(set);	//throw out all selected categories that aren't available
			updateStatus();	//update our status to reflect whether we now have available categories
		}

	/**The set of categories to use.*/
	private final Set categorySet;

		/**@return The set of categories to use.*/
		protected Set getCategorySet() {return categorySet;}

		/**Sets the categories to use, keeping the categories but not the given set.
		@param set The set of categories to use.
		*/
/*G***del when works
		protected void setCategorySet(final Set set)
		{
			categorySet.clear();	//clear our categories
			categorySet.addAll(set);	//all all the given categories
		}
*/

	/**Default constructor.*/
	public SelectionPanel()
	{
		super(new BasicGridBagLayout(), false);	//construct the panel using a grid bag layout, but don't initialize the panel
		randomCheckBox=new JCheckBox();
		questionCountLabel=new JLabel();
		questionCountButtonGroup=new ButtonGroup();
		allQuestionsRadioButton=new JRadioButton();
		onlyQuestionsRadioButton=new JRadioButton();
		questionCountTextField=new JTextField();
		selectCategoriesAction=new SelectCategoriesAction();
		categoriesButton=new JButton(selectCategoriesAction);
		availableCategorySet=new HashSet();	//default to no available categories
		categorySet=new HashSet();	//default to no categories
		setDefaultFocusComponent(allQuestionsRadioButton);	//set the default focus component
		initialize();	//initialize the panel
	}
	
	/**Initializes the user interface.*/
	public void initializeUI()
	{
		super.initializeUI();	//do the default user interface initialization
		setBorder(BorderUtilities.createDefaultTitledBorder());
		setTitle("Selection");	//G***i18n
		randomCheckBox.setText("Random selection");	//G***i18n
		randomCheckBox.setSelected(true);
		questionCountLabel.setText("Number of questions");	//G***i18n
		questionCountButtonGroup.add(allQuestionsRadioButton);
		questionCountButtonGroup.add(onlyQuestionsRadioButton);

		final ActionListener updateStatusModifyListener=createUpdateStatusActionListener();	//create an action listener to update the status		
		allQuestionsRadioButton.setText("All");	//G***i18n
		allQuestionsRadioButton.setSelected(true);
		allQuestionsRadioButton.addActionListener(updateStatusModifyListener);	//update the status if this button is pressed
		questionCountTextField.setColumns(4);
		onlyQuestionsRadioButton.setText("Only");	//G***i18n
		onlyQuestionsRadioButton.addActionListener(updateStatusModifyListener);	//update the status if this button is pressed
		categoriesButton.setText("Categories...");	//G***i18n

		add(questionCountLabel, new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		add(allQuestionsRadioButton, new GridBagConstraints(1, 0, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		add(onlyQuestionsRadioButton, new GridBagConstraints(1, 1, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHEAST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		add(questionCountTextField, new GridBagConstraints(2, 1, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		add(randomCheckBox, new GridBagConstraints(0, 1, 3, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		add(categoriesButton, new GridBagConstraints(0, 2, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
	}

	/**Updates the states of the actions, including enabled/disabled status,
		proxied actions, etc.
	*/
	protected void updateStatus()
	{
		super.updateStatus();	//do the default updating
			//only allow entry of a question count if the only questions radio button is selected
		questionCountTextField.setEnabled(onlyQuestionsRadioButton.isSelected());
			//only allow selection of categories if there are categories to select
		getSelectCategoriesAction().setEnabled(getAvailableCategorySet().size()>0);
	}

	/**Verifies the component.
	@return <code>true</code> if the component contents are valid, <code>false</code>
		if not.
	*/
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

	/**Asks the user to select categories and in response selects those categories.*/
	public void selectCategories()
	{
		final Set categorySet=getCategorySet();	//get the set of selected categories
		final JList categoryList=new JList();	//create a list of categories
		categoryList.setUI(new ToggleListUI()); //allow the answers to be toggled on and off
		categoryList.setCellRenderer(new CheckBoxListCellRenderer());  //display the answers with checkboxes
		final List availableCategoryList=new ArrayList(getAvailableCategorySet());	//create a list of available categories
		Collections.sort(availableCategoryList);	//sort the list of available categories
		categoryList.setListData(availableCategoryList.toArray());	//put the available categories in the list
		final Iterator categoryIterator=categorySet.iterator();	//get an iterator to the selected categories
		while(categoryIterator.hasNext())	//while there are more categories 
		{
			categoryList.setSelectedValue(categoryIterator.next(), false);	//select this category without scrolling
		}
			//show the categories; if the user accepts the new selections 
		if(OptionPane.showConfirmDialog(this, new ListPanel(categoryList), "Selected categories", OptionPane.OK_CANCEL_OPTION, OptionPane.QUESTION_MESSAGE)==OptionPane.OK_OPTION)	//G***i18n
		{
			categorySet.clear();	//clear our set of categories
			CollectionUtilities.addAll(categorySet, categoryList.getSelectedValues());	//add the selected categories
		}
	}

	/**Action for selecting categories.*/
	class SelectCategoriesAction extends AbstractAction
	{
		/**Default constructor.*/
		public SelectCategoriesAction()
		{
			super("Categories...");	//create the base class G***i18n
			putValue(SHORT_DESCRIPTION, "Select categories...");	//set the short description G***i18n
			putValue(LONG_DESCRIPTION, "Select the categories of entries to include.");	//set the long description G***i18n
			putValue(MNEMONIC_KEY, new Integer('c'));  //set the mnemonic key G***i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.GROUP_ICON_FILENAME)); //load the correct icon
		}
	
		/**Called when the action should be performed.
		@param actionEvent The event causing the action.
		*/
		public void actionPerformed(final ActionEvent actionEvent)
		{
			selectCategories();	//ask the user to select categories
		}
	}
}
