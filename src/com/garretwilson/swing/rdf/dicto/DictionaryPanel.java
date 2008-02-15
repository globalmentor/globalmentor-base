package com.garretwilson.swing.rdf.dicto;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.IOException;
import java.util.*;
import javax.mail.internet.ContentType;
import javax.swing.*;
import com.garretwilson.io.ContentTypeConstants;
import com.garretwilson.swing.*;
import com.garretwilson.swing.rdf.RDFPanel;
import com.garretwilson.text.xml.XMLUtilities;
import com.garretwilson.text.xml.xhtml.*;
import com.garretwilson.text.xml.xlink.*;
import com.garretwilson.model.ResourceModel;
import com.garretwilson.rdf.*;
import com.garretwilson.rdf.dicto.*;
import com.garretwilson.rdf.dicto.Dictionary;
import com.garretwilson.rdf.dublincore.DCUtilities;
import com.garretwilson.rdf.maqro.*;
import com.garretwilson.rdf.xpackage.XPackageUtilities;
import com.garretwilson.resources.icon.IconResources;
import com.globalmentor.java.Characters;
import com.globalmentor.marmot.resource.dicto.*;	//TODO GlobalMentor-specific items up to some other class if possible
import com.globalmentor.mentoract.activity.maqro.MAQROActivityEngine;
import com.globalmentor.mentoract.activity.maqro.MAQROActivityPanel;
import org.w3c.dom.*;

/**A panel to view and edit a Dictionary Ontology (Dicto) dictionary.
@author Garret Wilson
*/
public class DictionaryPanel extends RDFPanel<Dictionary, ResourceModel<Dictionary>>
{

	/**The action for performing a quiz.*/
	private final Action quizAction;

		/**@return The action for performing a quiz.*/
		public Action getQuizAction() {return quizAction;}

	/**The book for the WYSIWYG view.*/
	protected final Book book;

	/**Sets the data model.
	@param newModel The data model for which this component provides a view.
	@exception ClassCastException Thrown if the model is not a <code>DictionaryModel</code>.
	*/
	public void setModel(final ResourceModel<Dictionary> newModel)
	{
		book.getXMLTextPane().setURIInputStreamable(newModel);	//make sure the text pane knows from where to get input streams
		super.setModel(newModel);	//set the model in the parent class
	}

	/**Default constructor.*/
	public DictionaryPanel()
	{
		this(new ResourceModel<Dictionary>());	//construct the panel with a default model
	}

	/**Model constructor.
	@param model The data model for which this component provides a view.
	*/
	public DictionaryPanel(final ResourceModel<Dictionary> model)
	{
		this(model, true);	//construct and initialize the panel
	}

	/**Model constructor with optional initialization.
	@param model The data model for which this component provides a view.
	@param initialize <code>true</code> if the panel should initialize itself by
		calling the initialization methods.
	*/
	public DictionaryPanel(final ResourceModel<Dictionary> model, final boolean initialize)
	{
		super(model, false);	//construct the parent class without initializing it
		addSupportedModelView(WYSIWYG_MODEL_VIEW);	//show that we now support WYSIWYG data views, too
		quizAction=new QuizAction();	//create an action for taking a quiz
		book=new Book(2);	//create a new book for the WYSIWYG view, showing two pages at a time
		if(initialize)  //if we should initialize
			initialize();   //initialize the panel
	}

	/**Initialize the user interface.*/
	protected void initializeUI()
	{
		addView(WYSIWYG_MODEL_VIEW, "Dictionary", book, null);	//add the book component as the WYSIWYG view G***i18n
		setDefaultDataView(WYSIWYG_MODEL_VIEW);	//set the WYSIWYG view as the default view
		super.initializeUI(); //do the default UI initialization
//TODO set the book to be not editable
		final ActionManager actionManager=getActionManager();	//get our action manager and set up tool actions
		actionManager.addToolAction(getQuizAction());
		add(getActionManager().addToolComponents(new ApplicationToolBar()), BorderLayout.NORTH);	//put a toolbar in the north with our tool actions
	}

	/**Loads the data from the model to the specified view, if necessary.
	@param modelView The view of the data, such as <code>SUMMARY_MODEL_VIEW</code>.
	@exception IOException Thrown if there was an error loading the model.
	*/
	protected void loadModel(final int modelView) throws IOException
	{
		super.loadModel(modelView);	//do the default loading
		final ResourceModel<Dictionary> model=getModel();	//get the data model
		switch(modelView)	//see which view of data we should load
		{
			case WYSIWYG_MODEL_VIEW:	//if we're changing to the WYSIWYG view
				book.getXMLTextPane().setURIInputStreamable(model);	//make sure the text pane knows from where to get input streams
				if(model.getResource()!=null)	//if we have a dictionary
				{
					final Dictionary dictionary=model.getResource();	//get the dictionary represented by the model
					final Document xhtmlDocument=XHTMLUtilities.createXHTMLDocument();	//create an XHTML document
					final Element bodyElement=XHTMLUtilities.getBodyElement(xhtmlDocument);	//get the body element
					assert bodyElement!=null : "Missing <body> element in default XHTML document.";
						//set the title
					final Locale dictionaryLanguage=dictionary.getDictionaryLanguage();	//get the language of the entries
					final String languageTitle=dictionaryLanguage!=null ? dictionaryLanguage.getDisplayLanguage()+" " : "";	//get the language part of the title
					final Element h1Element=XMLUtilities.appendElement(bodyElement, XHTMLConstants.XHTML_NAMESPACE_URI.toString(), XHTMLConstants.ELEMENT_H1, languageTitle+"Dictionary");	//G***i18n
					if(dictionary.getEntries()!=null)	//if we have a dictionary and it has entries
					{
						final Element dlElement=XMLUtilities.appendElement(bodyElement, XHTMLConstants.XHTML_NAMESPACE_URI.toString(), XHTMLConstants.ELEMENT_DL);
						final Iterator entryIterator=dictionary.getEntries().iterator();	//get an iterator to look at all the dictionary entries
						while(entryIterator.hasNext())	//while there are more dictionary entries
						{
								//TODO check for null for each of the entry properties
							final Entry entry=(Entry)entryIterator.next();	//get the next entry
							final RDFPlainLiteral orthography=entry.getOrthography();	//get the entry orthography
							RDFPlainLiteral plainLiteralPronunciation=null;	//we'll see if there is pronunciation text
							RDFResource resourcePronunciation=null;	//we'll see if there is a pronunciation object
							for(final RDFObject pronunciation:entry.getPronunciations())	//for each pronunciation
							{
								if(pronunciation instanceof RDFPlainLiteral && plainLiteralPronunciation==null)	//if this is text pronunciation and we don't have any, yet
								{
									plainLiteralPronunciation=(RDFPlainLiteral)pronunciation;	//save the text pronunciation
								}
								else if(pronunciation instanceof RDFResource && resourcePronunciation==null)	//if this is a resouce pronunciation and we don't have one, yet
								{
									resourcePronunciation=(RDFResource)pronunciation;	//save the resource pronunciation
								}
							}
							final StringBuffer orthographyStringBuffer=new StringBuffer(orthography.toString());	//deterine the orthography HTML
/*G***del
							if(resourcePronunciation!=null)	//if we have a pronunciation resource
							{
								final String href=XPackageUtilities.getLocationHRef(resourcePronunciation);	//see if the resource has a link to a resource
								if(href!=null)	//if there is a link to a resouce
								{
									orthographyStringBuffer.insert(0, "<a href="+href+">");	//prepend the orthography with a link TODO use constants
									orthographyStringBuffer.append("</a>");	//postpend the orthography with a link TODO use constants
								}
							}
*/
							final Element dtElement=XMLUtilities.appendElement(dlElement, XHTMLConstants.XHTML_NAMESPACE_URI.toString(),
									XHTMLConstants.ELEMENT_DT, orthography.toString());	//show the entry orthography TODO add xml:lang to all of these terms
							if(resourcePronunciation!=null)	//if we have a pronunciation resource
							{
								final String href=XPackageUtilities.getLocationHRef(resourcePronunciation);	//see if the resource has a link to a resource
								if(href!=null)	//if there is a link to a resouce
								{
									XLinkUtilities.setXLink(dtElement, XLinkConstants.SIMPLE_TYPE, href);	//link the term to the linked pronunciation resource
								}
							}
							final Element ddElement=XMLUtilities.appendElement(dlElement, XHTMLConstants.XHTML_NAMESPACE_URI.toString(),
									XHTMLConstants.ELEMENT_DD);	//create the definition element
							final RDFPlainLiteral transliteration=entry.getTransliteration();	//get the entry transliteration
							if(transliteration!=null)	//if there is a transliteration
							{
								XMLUtilities.appendElement(ddElement, XHTMLConstants.XHTML_NAMESPACE_URI.toString(),
										XHTMLConstants.ELEMENT_EM, transliteration.toString());	//show the entry transliteration TODO add xml:lang to all of these terms
								XMLUtilities.appendText(ddElement, Characters.SPACE_CHAR);	//add a space
							}
							if(entry instanceof Word)	//if this is a word
							{
								final Word word=(Word)entry;	//cast the entry to a word
								final RDFPlainLiteral speechPart=word.getSpeechPart();	//get the word part of speech
								final RDFPlainLiteral gender=word.getGender();	//get the word gender
								if(speechPart!=null || gender!=null)	//if there is a part of speech or gender
								{
									XMLUtilities.appendText(ddElement, '(');	//add a left parenthesis
									if(speechPart!=null)	//if there is a part of speech
									{
										XMLUtilities.appendText(ddElement, speechPart.toString());	//add the part of speech TODO add xml:lang to all of these terms
									}
									if(gender!=null)	//if there is a gender
									{
										if(speechPart!=null)	//if there was a part of speech
										{
											XMLUtilities.appendText(ddElement, ": ");	//separate the part of speech and gender
										}
										XMLUtilities.appendText(ddElement, gender.toString());	//add the gender TODO add xml:lang to all of these terms
									}
									XMLUtilities.appendText(ddElement, ')');	//add a right parenthesis
									XMLUtilities.appendText(ddElement, Characters.SPACE_CHAR);	//add a space
								}
							}
							final RDFPlainLiteral translation=entry.getTranslation();	//get the entry translation
							if(translation!=null)	//if there is a translation
							{
								XMLUtilities.appendText(ddElement, translation.toString());	//show the translation
								XMLUtilities.appendText(ddElement, Characters.SPACE_CHAR);	//add a space
							}
							final RDFPlainLiteral definition=entry.getDefinition();	//get the entry definition
							if(definition!=null)	//if there is a definition
							{
								XMLUtilities.appendText(ddElement, Characters.EM_DASH_CHAR);	//add an em-dash
								XMLUtilities.appendText(ddElement, definition.toString());	//show the definition
								XMLUtilities.appendText(ddElement, Characters.SPACE_CHAR);	//add a space
							}
						}
					}
						//show the XML in the book, specifying the base URI of the RDF data model
					book.setXML(xhtmlDocument, model.getBaseURI(), XHTMLConstants.XHTML_CONTENT_TYPE);
				}
				else	//if we don't have a dictionary
				{
					book.close();	//remove the content from the book					
				}
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
		switch(oldView)	//see what view we're changing from
		{
			case WYSIWYG_MODEL_VIEW:	//if we're changing from the WYSIWYG view
				book.close();	//to conserve memory, remove the content from the book
				break;
		}
	}

	/**Performs a quiz over the contents of the dictionary.*/
	public void quiz()
	{
		//TODO correctly enable or disable the corresponding action based upon the presence of a dictionary
		final Dictionary dictionary=getModel().getResource();
		if(dictionary!=null)	//if we have a dictionary
		{
			final Locale dictionaryLanguage=dictionary.getDictionaryLanguage();	//get the language of the entries
			final Locale translationLanguage=dictionary.getLanguage();	//get the language of any translations
			final Set availableCategorySet=new HashSet();	//create a set to hold our categories
			final List entryList=dictionary.getEntries();	//get the list of entry
			if(entryList!=null)	//if this dictionary has entries
			{
				final Iterator entryIterator=entryList.iterator();	//get an iterator to the entries
				while(entryIterator.hasNext())	//while there are more entries
				{
					final Entry entry=(Entry)entryIterator.next();	//get the next entry
					int entryCategoryCount=0;	//keep track of how many categories we retrieve for this category
					for(final RDFObject cat:MAQROUtilities.getCategories(entry))	//for each of the categories for this entry
					{
						availableCategorySet.add(cat);	//get the next category (which should be a literal) and add it to our set
						++entryCategoryCount;	//show that we added another category for this entry
					}
					if(entryCategoryCount==0)	//if this entry had no categories
					{
						availableCategorySet.add(MAQROConstants.NO_CATEGORY);	//add the constant object representing to category specified					
					}
				}
			}
			final DictionaryActivityOptionsPanel optionsPanel=new DictionaryActivityOptionsPanel();	//create a new options panel
			final Selection selection=new RandomSelection();	//create random selection criteria
			selection.setOrder(new RandomOrder());	//set the order of the selection to random
			optionsPanel.setAvailableCategorySet(availableCategorySet);	//set the available categories in the options panel
/*G***del; no selected categories selects all categories
			//select all available categories
			final Iterator availableCategoryIterator=availableCategorySet.iterator();	//get an iterator to the available categories
			while(availableCategoryIterator.hasNext())	//while there are more available categories
			{
					//add the next category to our selection
				selection.addProperty(MAQROConstants.MAQRO_NAMESPACE_URI, MAQROConstants.CATEGORY_PROPERTY_NAME, (RDFLiteral)availableCategoryIterator.next());
			}
*/
			if(translationLanguage!=null && !translationLanguage.equals(dictionaryLanguage))	//if this dictionary is a translation dictionary
				optionsPanel.setChoicesProperty(DictionaryActivity.TRANSLATION_PROPERTY);	//default to using translations instead of definitions for choices

			optionsPanel.setSelection(selection);	//set the panel selection criteria

				//show the options; if the user accepts the options 
			if(BasicOptionPane.showConfirmDialog(this, optionsPanel, "Dictionary Quiz Options", BasicOptionPane.OK_CANCEL_OPTION, BasicOptionPane.QUESTION_MESSAGE)==BasicOptionPane.OK_OPTION)	//G***i18n
			{
					//create a Mentoract activity adapter that will create questions based upon dictionary entries
				final DictionaryActivity dictionaryActivity=new DictionaryActivity(dictionary);
				dictionaryActivity.setAllowHint(true);
				dictionaryActivity.setShowResultProgress(true);
				dictionaryActivity.setShowEachResult(true);
				dictionaryActivity.setShowFinalResult(true);
				DCUtilities.addTitle(dictionaryActivity, dictionaryLanguage.getDisplayLanguage()+" Quiz");	//add a title showing the language G***i18n
				dictionaryActivity.setSelection(optionsPanel.getSelection());	//set the activity's selection criteria
/*G***fix and del
				dictionaryActivity.setQuestionCount(optionsPanel.getQuestionCount());	//show how many questions to use
				dictionaryActivity.setChoiceCount(optionsPanel.getChoiceCount());	//show how many questions to use
				dictionaryActivity.setQueryProperty(optionsPanel.getQueryProperty());	//show which property to use for the query
				dictionaryActivity.setChoicesProperty(optionsPanel.getChoicesProperty());	//show which property to use for the choices
*/
				final MAQROActivityEngine activityEngine=new MAQROActivityEngine(dictionaryActivity);	//create an engine for the activity
				activityEngine.setBaseURI(getModel().getBaseURI());	//set the base URI of the engine TODO probably make the resource application panel URIAccessible
				activityEngine.setURIInputStreamable(getModel());	//tell the activity engine to use our URI sourcefor reading
				final MAQROActivityPanel activityPanel=new MAQROActivityPanel(activityEngine);	//create a new activity panel for the engine

				/*TODO fix with non-ResourceApplicationFrame

				final ResourceApplicationFrame activityFrame=new ResourceApplicationFrame(activityPanel, false);	//construct an activity frame without initializing it
*/

	//TODO should we just make ApplicationFrame a concrete class?
	/*G***fix
				resourceApplicationFrame.setApplicationName(getLabel());	//set the type of resource as the application name of the frame
				resourceApplicationFrame.setIconImage(getIcon().getImage());	//set the resource type icon as the frame icon
				resourceApplicationFrame.setFileMenuInclusions(ResourceApplicationFrame.MENU_FILE_SAVE|ResourceApplicationFrame.MENU_FILE_EXIT);	//only show file|save and file|exit
				resourceApplicationFrame.setFileExitAction(resourceApplicationFrame.getCloseAction());	//show the closing action for file|exit
				resourceApplicationFrame.setHelpMenuInclusions(ResourceApplicationFrame.MENU_HELP_NONE);	//don't show the help menu
	*/

	/*TODO fix with non-ResourceApplicationFrame

				activityFrame.initialize();	//initialize the frame
				activityFrame.setVisible(true);	//show the activity frame
				activityEngine.start();	//start the interaction
*/
final ApplicationFrame activityFrame=new ApplicationFrame(activityPanel);	//construct a frame for the activity
activityFrame.setVisible(true);	//show the activity frame
			}
		}
	}

	/**Activity action that allows quizing on the dictionary's contents.*/
	protected class QuizAction extends AbstractAction
	{
		/**Default constructor.*/
		public QuizAction()
		{
			super("Quiz");	//create the base class G***i18n
			putValue(SHORT_DESCRIPTION, "Interactive Quiz");	//set the short description G***i18n
			putValue(LONG_DESCRIPTION, "Display an interactive quiz on the contents of the dictionary.");	//set the long description G***i18n
			putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_Q));  //set the mnemonic key; for some reason, 's' causes the action to be activated when Alt+F4 is pressed G***i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.ANIMATION_ICON_FILENAME)); //load the correct icon
//G***del			putValue(SMALL_ICON, IconResources.getIcon(IconResources.DOCUMENT_QUESTION_ICON_FILENAME)); //load the correct icon
			putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_Q, KeyEvent.CTRL_MASK)); //add the accelerator G***i18n
		}

		/**Called when the action should be performed.
		@param actionEvent The event causing the action.
		*/
		public void actionPerformed(final ActionEvent actionEvent)
		{
			quiz();	//perform the quiz
		}
	}

}