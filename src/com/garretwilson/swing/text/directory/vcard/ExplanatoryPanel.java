package com.garretwilson.swing.text.directory.vcard;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.beans.PropertyChangeListener;
import java.net.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import com.garretwilson.lang.*;
import com.garretwilson.text.directory.vcard.*;
import com.garretwilson.swing.*;
import com.garretwilson.util.*;

/**A panel allowing entry of the explanatory types of a vCard <code>text/directory</code>
	profile as defined in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>,
	"vCard MIME Directory Profile".
@author Garret Wilson
*/
public class ExplanatoryPanel extends BasicVCardPanel
{

	/**The label of the categories list.*/
	private final JLabel categoryLabel;

	/**The action for selecting the language of the categories.*/
	private final SelectLanguageAction selectCategoryLanguageAction;

		/**@return The action for selecting the language of the category.*/
		public SelectLanguageAction getSelectCategoryLanguageAction() {return selectCategoryLanguageAction;}

	/**The categories list.*/
	private final JList categorySwingList;

		/**@return The categories list.*/
		public JList getCategoryList() {return categorySwingList;}
	
	/**The label of the note text pane.*/
	private final JLabel noteLabel;

	/**The action for selecting the language of the note.*/
	private final SelectLanguageAction selectNoteLanguageAction;

		/**@return The action for selecting the language of the note.*/
		public SelectLanguageAction getSelectNoteLanguageAction() {return selectNoteLanguageAction;}

	/**The note text pane.*/
	private final JTextPane noteTextPane;

		/**@return The note text pane.*/
		public JTextPane getNoteTextPane() {return noteTextPane;}

	/**The label of the URL.*/
	private final JLabel urlLabel;

	/**The URL text field.*/
	private final JTextField urlTextField;

		/**@return The URL text field.*/
		public JTextField getURLTextField() {return urlTextField;}
	
	/**Sets the application categories.
	@param categories An array of application categories that should be selected.
	*/
	public void setCategories(final LocaleText[] categories)
	{
		final ModifiableSet availableCategorySet=Categories.getAvailableCategorySet();	//get the available categores
		CollectionUtilities.addAll(availableCategorySet, categories);	//add all the categories to the available category set, which will only add the ones that aren't there already
		final List availableCategoryList=new ArrayList(availableCategorySet);	//create a list of available categories
		Collections.sort(availableCategoryList);	//sort the list of available categories
		categorySwingList.setListData(availableCategoryList.toArray());	//put the available categories in the list
		for(int i=categories.length-1; i>=0; --i)	//look at each category
		{
			final int availableCategoryIndex=availableCategoryList.indexOf(categories[i]);	//get the index of this selected category
			if(availableCategoryIndex>=0)	//if the selected category is in our list
			{
				categorySwingList.addSelectionInterval(availableCategoryIndex, availableCategoryIndex);	//select that category
			}
		}
	}
	
	/**@return An array of application categories selected.*/
	public LocaleText[] getCategories()
	{
		final Object[] selectedObjects=categorySwingList.getSelectedValues();	//get the selected categories
		final LocaleText[] selectedCategories=new LocaleText[selectedObjects.length];	//create a locale text array into which to place the selected categories
		System.arraycopy(selectedObjects, 0, selectedCategories, 0, selectedObjects.length);	//copy the categories into our string array
		return selectedCategories;	//return the selected categories
	}

	/**Sets the supplemental information.
	@param note The supplemental information, or <code>null</code> for no information.
	*/
	public void setNote(final LocaleText note)
	{
		if(note!=null)	//if there is a note
		{
			noteTextPane.setText(note.getText());	//set the text of the note text pane
			selectNoteLanguageAction.setLocale(note.getLocale());
		}
		else	//if there is no note, clear the fields
		{
			noteTextPane.setText("");
			selectNoteLanguageAction.setLocale(null);
		}
	}
	
	/**@return The edited supplemental information, or <code>null</code> for no information.*/
	public LocaleText getNote()
	{
		final String note=StringUtilities.getNonEmptyString(noteTextPane.getText().trim());
		return note!=null ? new LocaleText(note, selectNoteLanguageAction.getLocale()) : null;
	}

	/**Places the URL into the field.
	@param url The URL information to place in the field, or
		<code>null</code> if there is no URL.
	*/
	public void setURL(final URI url)
	{
		getURLTextField().setText(url!=null ? url.toString() : "");	//show the URL, if there is one
	}

	/**@return An object representing the URL entered, or
		<code>null</code> if no URL number was entered or the text is not a valid
		URI.
	*/
	public URI getURL()
	{
		final String urlString=getURLTextField().getText().trim();	//get the URL text entered
		if(urlString.length()>0)	//if there is URL information entered
		{
			try
			{
				return new URI(urlString);	//create and return a URI from the entered information
			}
			catch(URISyntaxException uriSyntaxExceptoin)	//if the information isn't a valid URI
			{
				return null;	//show that we don't understand the entered information
			}
		}
		else	//if no URL was entered
		{
			return null;	//nothing was entered
		}
	}

	/**Default constructor.*/
	public ExplanatoryPanel()
	{
		this((LocaleText)null);	//construct a panel with no categories and no note
	}

	/**Categories constructor.
	@param categories An array of application categories that should be selected.
	*/
	public ExplanatoryPanel(final LocaleText[] categories)
	{
		this(categories, null);	//construct the class with no notes
	}
	
	/**Note constructor.
	@param note The supplemental information, or <code>null</code> for no information.
	*/
	public ExplanatoryPanel(final LocaleText note)
	{
		this(new LocaleText[]{}, note);	//construct a panel with no categories
	}

	/**Categories and note constructor.
	@param categories An array of application categories that should be selected.
	@param note The supplemental information, or <code>null</code> for no information.
	*/
	public ExplanatoryPanel(final LocaleText[] categories, final LocaleText note)
	{
		this(categories, note, null);	//create a panel with no URL
	}

	/**Full constructor.
	@param categories An array of application categories that should be selected.
	@param note The supplemental information, or <code>null</code> for no information.
	@param url The associated URL, or <code>null</code> for no URL.
	*/
	public ExplanatoryPanel(final LocaleText[] categories, final LocaleText note, final URI url)
	{
		super(new GridBagLayout(), false);	//construct the panel using a grid bag layout, but don't initialize the panel
		categoryLabel=new JLabel();
		categorySwingList=new JList();
		selectCategoryLanguageAction=new SelectCategoryLanguageAction();
		noteLabel=new JLabel();
		noteTextPane=new JTextPane();
		selectNoteLanguageAction=new SelectLanguageAction(null, noteTextPane);
		urlLabel=new JLabel();
		urlTextField=new JTextField();
		setDefaultFocusComponent(noteTextPane);	//set the default focus component
		initialize();	//initialize the panel
		setCategories(categories);	//set the given categories
		setNote(note);	//set the given note
		setURL(url);	//set the given URL
	}
	
	/**Initializes the user interface.*/
	public void initializeUI()
	{
		super.initializeUI();	//do the default user interface initialization
		final PropertyChangeListener modifyLocalePropertyChangeListener=createModifyPropertyChangeListener(LocaleConstants.LOCALE_PROPERTY_NAME);	//create a property change listener to change the modified status when the locale property changes		
		categoryLabel.setText("Categories");	//G***i18n
		getSelectCategoryLanguageAction().addPropertyChangeListener(modifyLocalePropertyChangeListener);
		final JButton selectCategoryLanguageButton=createSelectLanguageButton(getSelectCategoryLanguageAction());
		categorySwingList.setUI(new ToggleListUI()); //allow the answers to be toggled on and off
		categorySwingList.setCellRenderer(new CheckBoxListCellRenderer());  //display the answers with checkboxes
		categorySwingList.addListSelectionListener(getModifyListSelectionListener());
		final JScrollPane categoryScrollPane=new JScrollPane(categorySwingList);
		noteLabel.setText("Note");	//G***i18n
		noteTextPane.getDocument().addDocumentListener(getModifyDocumentListener());
		getSelectNoteLanguageAction().addPropertyChangeListener(modifyLocalePropertyChangeListener);
		final JButton selectNoteLanguageButton=createSelectLanguageButton(getSelectNoteLanguageAction());
		final JScrollPane noteScrollPane=new JScrollPane(noteTextPane);
		urlLabel.setText("Web Site URL");	//G***i18n
		urlTextField.getDocument().addDocumentListener(getModifyDocumentListener());
		add(categoryLabel, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		add(selectCategoryLanguageButton, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		add(categoryScrollPane, new GridBagConstraints(0, 1, 2, 3, 0.0, 1.0, GridBagConstraints.NORTH, GridBagConstraints.BOTH, NO_INSETS, 0, 0));
		add(noteLabel, new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		add(selectNoteLanguageButton, new GridBagConstraints(3, 0, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		add(noteScrollPane, new GridBagConstraints(2, 1, 2, 1, 1.0, 1.0, GridBagConstraints.NORTH, GridBagConstraints.BOTH, NO_INSETS, 0, 0));
		add(urlLabel, new GridBagConstraints(2, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		add(urlTextField, new GridBagConstraints(2, 3, 2, 1, 1.0, 0.0, GridBagConstraints.NORTHWEST, GridBagConstraints.HORIZONTAL, NO_INSETS, 0, 0));
	}

	/**Verifies the component.
	@return <code>true</code> if the component contents are valid, <code>false</code>
		if not.
	*/
	public boolean verify()
	{
		final String urlString=urlTextField.getText().trim();	//get the URL text entered
		if(urlString.length()>0)	//if there is URL information entered
		{
			try
			{
				new URI(urlString);	//try to create a URI from the entered information
			}
			catch(URISyntaxException uriSyntaxException)	//if the information isn't a valid URI
			{
				JOptionPane.showMessageDialog(this, "The URL you entered is inavlid: "+uriSyntaxException.getMessage(), "Invalid URL", JOptionPane.ERROR_MESSAGE);	//G***i18n
				urlTextField.requestFocusInWindow(); //focus on the URL text field
				return false; //show that verification failed
			}
		}
		return super.verify();  //if we couldn't find any problems, verify the parent class
	}

	/**Action for selecting a language for the selected category.
	@author Garret Wilson
	@see LanguagePanel
	@see Locale
	*/
	class SelectCategoryLanguageAction extends SelectLanguageAction
	{
		/**@return The locale that represents the language, or
			<code>null</code> if no language is indicated.
		*/
		public Locale getLocale()
		{
			final int leadSelectionIndex=categorySwingList.getLeadSelectionIndex();	//get the last selected category index
			final Object selectedValue=leadSelectionIndex>=0 ? categorySwingList.getModel().getElementAt(leadSelectionIndex) : null;	//get the last selected value
			return selectedValue instanceof LocaleText ? ((LocaleText)selectedValue).getLocale() : null;	//return the locale if locale text is selected
		}

		/**Sets the language.
		@param newLocale The locale that represents the language, or
			<code>null</code> if no language should be indicated.
		*/
		public void setLocale(final Locale newLocale)
		{
			final int leadSelectionIndex=categorySwingList.getLeadSelectionIndex();	//get the last selected category index
			final Object selectedValue=leadSelectionIndex>=0 ? categorySwingList.getModel().getElementAt(leadSelectionIndex) : null;	//get the last selected value
//G***del			final Object selectedValue=categoryList.getSelectedValue();	//get the selected value
			if(selectedValue instanceof LocaleText)	//if locale text is selected
			{
				final LocaleText localeText=(LocaleText)selectedValue;	//cast the selected object to locale text
				final Locale oldLocale=localeText.getLocale(); //get the old locale
				if(!ObjectUtilities.equals(oldLocale, newLocale))  //if the value is really changing
				{
					localeText.setLocale(newLocale); //update the value
					firePropertyChange(LocaleConstants.LOCALE_PROPERTY_NAME, oldLocale, newLocale);	//show that the locale property has changed
				}
			}
		}

		/**Default constructor.*/
		public SelectCategoryLanguageAction()
		{
			super(null, categorySwingList);	//construct the parent class with no locale using the category list as the parent component
		}

		/**Called when the action should be performed.
		@param actionEvent The event causing the action.
		*/
		public void actionPerformed(final ActionEvent actionEvent)
		{
			final int leadSelectionIndex=categorySwingList.getLeadSelectionIndex();	//get the last selected category index
			final Object selectedValue=leadSelectionIndex>=0 ? categorySwingList.getModel().getElementAt(leadSelectionIndex) : null;	//get the last selected value
			if(selectedValue instanceof LocaleText)	//if locale text is selected (really we're just checking to make sure something is selected---it should always be locale text, if anything)
			{
				super.actionPerformed(actionEvent);	//do the default language selection
			}
			
		}
	}
}
