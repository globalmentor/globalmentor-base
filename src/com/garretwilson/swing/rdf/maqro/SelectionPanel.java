package com.garretwilson.swing.rdf.maqro;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;
import static java.util.Collections.*;
import javax.swing.*;
import com.garretwilson.awt.BasicGridBagLayout;
import com.garretwilson.rdf.RDFListResource;
import com.garretwilson.rdf.RDFLiteral;
import com.garretwilson.rdf.RDFObject;
import com.garretwilson.rdf.maqro.*;
import com.garretwilson.resources.icon.IconResources;
import com.garretwilson.swing.*;
import com.garretwilson.swing.border.BorderUtilities;
import com.garretwilson.util.*;

/**Panel for editing a MAQRO activity selection.
<p>This implementation does not support multiple selector filters of the same type.</p>
@author Garret Wilson
*/
public class SelectionPanel extends BasicPanel	//TODO eventually separate SelectionPanel from individual SelectorPanels
{

	private final JLabel questionCountLabel;
	private final JCheckBox randomSelectionCheckBox;
	private final JCheckBox randomOrderCheckBox;
	private final ButtonGroup questionCountButtonGroup;
	private final JRadioButton allQuestionsRadioButton;
	private final JRadioButton onlyQuestionsRadioButton;
	private final JTextField questionCountTextField;
	private final JButton categoriesButton;

	/**The action for selecting categories.*/
	private final Action selectCategoriesAction;

		/**@return The action for selecting categories.*/
		protected Action getSelectCategoriesAction() {return selectCategoriesAction;}

	/**@return New default selection criteria.*/
	protected Selection createSelection()
	{
		if(randomSelectionCheckBox.isSelected())	//if there should be random selection
		{
			return new RandomSelection();	//create a new random selection
		}
		else	//if there shouldn't be random selection
		{
			return new SequentialSelection();	//create a new sequential selection
		}
	}

	/**@return Selection criteria for selecting interactions.*/
	public Selection getSelection()
	{
		final Selection selection=createSelection();	//create new selection criteria
		final RDFListResource selectors=new RDFListResource();	//create a list of selectors
		final Selector selector=new Selector();	//create a new selector
		if(onlyQuestionsRadioButton.isSelected())	//if the user wants to limit the number of questions
		{
			selector.setCount(Integer.parseInt(questionCountTextField.getText().trim()));	//set the interaction count
		}
		final RDFListResource filters=new RDFListResource();	//create a list of filters
			//copy the selected categories to the selector
		final Iterator categoryIterator=getCategorySet().iterator();	//get an iterator to look at all the selected categories
		if(categoryIterator.hasNext())	//if there are any categories specified
		{
			final CategoryFilter categoryFilter=new CategoryFilter();	//create a category filter (assuming the categories are ORed selections)
			do	//look at each category requested (we already did a check, so use a post-check in the loop) 
			{
				final RDFObject category=(RDFObject)categoryIterator.next();	//get the next category
				if(category instanceof RDFLiteral)	//if this category is a literal
				{
						//copy this category to the selection criteria
					categoryFilter.addCategory((RDFLiteral)category);	//G***it would be best to clone the category
				}
			}
			while(categoryIterator.hasNext());	//keep adding the categories while there are more categories
			filters.add(categoryFilter);	//add this category filter to the list of filters
		}
		if(filters.size()>0)	//if filters are specified
		{
			selector.setFilters(filters);	//specify the filters
		}
		selectors.add(selector);	//add the selector to the list of selectors
		selection.setSelectors(selectors);	//set the list of the selection's selectors
		final Order order;	//determine the order
		if(randomOrderCheckBox.isSelected())	//if there should be random ordering
		{
			order=new RandomOrder();	//use random ordering
		}
		else	//if there shouldn't be random ordering
		{
			order=new SequentialOrder();	//use sequential ordering	
		}
		selection.setOrder(order);	//set the order of the selection criteria
		return selection;	//return the selection criteria we constructed
	}

	/**Sets the selection criteria to show in the panel.
	@param selectDescription The selection criteria.
	*/
	public void setSelection(final Selection selection)
	{
		randomSelectionCheckBox.setSelected(selection instanceof RandomSelection);	//if this is random selection, select the random selection checkbox
		final Order order=selection.getOrder();	//get the order
		randomOrderCheckBox.setSelected(order!=null && order instanceof RandomOrder);	//if this is random ordering, select the random order checkbox
		final RDFListResource selectors=selection.getSelectors();	//get the selectors
		final int count;	//we'll determine the count to show
		categorySet.clear();	//clear our categories
		if(selectors!=null && selectors.size()>0)	//if there is at least one selector, we'll use the first one and ignore the others
		{
			final Selector selector=(Selector)selectors.get(0);	//get the first selector G***should we really assume that this is a selector
			count=selector.getCount();	//see if a count is specified
			final RDFListResource filters=selector.getFilters();	//get any filters of this selector
			if(filters!=null)	//if we have filters
			{
				final Iterator filterIterator=filters.iterator();	//get an iterator to the filters
				while(filterIterator.hasNext())	//while there are more filters
				{
					final RDFObject filter=(RDFObject)filterIterator.next();	//get the next filter
					if(filter instanceof CategoryFilter)	//if this is a filter iterator
					{
						final CategoryFilter categoryFilter=(CategoryFilter)filter;	//cast the filter to a category filter
						CollectionUtilities.addAll(categorySet, categoryFilter.getCategoryIterator());	//add all selector categories to our set of categories G***we should really create clones of these
						break;	//this implementation only recognizes the first category filter
					}
				}
			}			
		}
		else	//if there are no selectors
		{
			count=-1;	//use all interactions
		}
		if(count>=0)	//if a valid count was given
		{
			onlyQuestionsRadioButton.setSelected(true);	//select the count radio button
			questionCountTextField.setText(Integer.toString(count));	//show the count count
		}
		else	//if all interactions should be selected
		{
			allQuestionsRadioButton.setSelected(true);	//select the all radio button
			questionCountTextField.setText("");	//clear the count
		}
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
		this(true);	//construct the panel and initialize it
	}

	/**Constructor with optional initialization.
	@param initialize <code>true</code> if the panel should initialize itself by
		calling the initialization methods.
	*/
	public SelectionPanel(final boolean initialize)
	{
		super(new BasicGridBagLayout(), false);	//construct the panel using a grid bag layout, but don't initialize the panel
		randomSelectionCheckBox=new JCheckBox();
		randomOrderCheckBox=new JCheckBox();
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
		if(initialize)  //if we should initialize
			initialize();   //initialize the panel
	}
	
	/**Initializes the user interface.*/
	public void initializeUI()
	{
		super.initializeUI();	//do the default user interface initialization
		setBorder(BorderUtilities.createDefaultTitledBorder());
		setTitle("Selection");	//G***i18n
		randomSelectionCheckBox.setText("Random selection");	//G***i18n
		randomSelectionCheckBox.setSelected(true);
		randomOrderCheckBox.setText("Random order");	//G***i18n
		randomOrderCheckBox.setSelected(true);
		questionCountLabel.setText("Number of questions");	//G***i18n
		questionCountButtonGroup.add(allQuestionsRadioButton);
		questionCountButtonGroup.add(onlyQuestionsRadioButton);

		final ItemListener updateStatusItemListener=createUpdateStatusItemListener();	//create an item listener to update the status		
		allQuestionsRadioButton.setText("All");	//G***i18n
		allQuestionsRadioButton.setSelected(true);
		allQuestionsRadioButton.addItemListener(updateStatusItemListener);	//update the status if this button is pressed
		questionCountTextField.setColumns(4);
		onlyQuestionsRadioButton.setText("Only");	//G***i18n
		onlyQuestionsRadioButton.addItemListener(updateStatusItemListener);	//update the status if this button is pressed
		categoriesButton.setText("Categories...");	//G***i18n

		add(questionCountLabel, new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		add(allQuestionsRadioButton, new GridBagConstraints(1, 0, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		add(onlyQuestionsRadioButton, new GridBagConstraints(1, 1, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHEAST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		add(questionCountTextField, new GridBagConstraints(2, 1, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		add(randomSelectionCheckBox, new GridBagConstraints(0, 1, 3, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		add(randomOrderCheckBox, new GridBagConstraints(0, 2, 3, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		add(categoriesButton, new GridBagConstraints(0, 3, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
	}

	/**Updates the states of the actions, including enabled/disabled status,
		proxied actions, etc.
	*/
	public void updateStatus()
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
		final JList categorySwingList=new JList();	//create a list of categories
		categorySwingList.setUI(new ToggleListUI()); //allow the answers to be toggled on and off
		categorySwingList.setCellRenderer(new CheckBoxListCellRenderer());  //display the answers with checkboxes
		final List availableCategoryList=new ArrayList(getAvailableCategorySet());	//create a list of available categories
		Collections.sort(availableCategoryList);	//sort the list of available categories
		categorySwingList.setListData(availableCategoryList.toArray());	//put the available categories in the list
		final Iterator categoryIterator=categorySet.iterator();	//get an iterator to the selected categories
		while(categoryIterator.hasNext())	//while there are more categories 
		{
			final int availableCategoryIndex=availableCategoryList.indexOf(categoryIterator.next());	//get the index of this selected category
			if(availableCategoryIndex>=0)	//if the selected category is in our list
			{
				categorySwingList.addSelectionInterval(availableCategoryIndex, availableCategoryIndex);	//select that category
			}
		}
			//show the categories; if the user accepts the new selections 
		if(BasicOptionPane.showConfirmDialog(this, new ListPanel(categorySwingList), "Selected categories", BasicOptionPane.OK_CANCEL_OPTION, BasicOptionPane.QUESTION_MESSAGE)==BasicOptionPane.OK_OPTION)	//G***i18n
		{
			categorySet.clear();	//clear our set of categories
			addAll(categorySet, categorySwingList.getSelectedValues());	//add the selected categories
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
			putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_C));  //set the mnemonic key G***i18n
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
