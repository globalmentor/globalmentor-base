package com.garretwilson.swing.rdf.dicto;

import java.io.IOException;
import java.util.Iterator;
import java.util.Locale;
import com.garretwilson.swing.*;
import com.garretwilson.swing.rdf.RDFPanel;
import com.garretwilson.text.CharacterConstants;
import com.garretwilson.text.xml.XMLUtilities;
import com.garretwilson.text.xml.xhtml.XHTMLConstants;
import com.garretwilson.text.xml.xhtml.XHTMLUtilities;
import com.garretwilson.text.xml.xlink.XLinkConstants;
import com.garretwilson.text.xml.xlink.XLinkUtilities;
import com.garretwilson.util.Debug;
import com.garretwilson.io.MediaType;
import com.garretwilson.rdf.*;
import com.garretwilson.rdf.dicto.*;
import com.garretwilson.rdf.xpackage.XPackageUtilities;

import org.w3c.dom.*;

/**A panel to view and edit a Dictionary Ontology (Dicto) dictionary.
@author Garret Wilson
*/
public class DictionaryPanel extends RDFPanel
{

	/**@return The dictionary being displayed.*/
	public Dictionary getDictionary() {return (Dictionary)getResource();}

	/**The scroll pane for the dictionary WYSIWYG view.*/
//G***del	protected final JScrollPane wysiwygScrollPane;

	/**The text pane for the WYSIWYG view.*/
//G***del	protected final JTextPane wysiwygTextPane;

	/**The book for the WYSIWYG view.*/
	protected final Book book;

	/**Default constructor with default dictionary.*/
	public DictionaryPanel()
	{
		this(new Dictionary());  //create a panel with a default dictionary
	}

	/**Dictionary constructor with a default RDF data model.
	@param dictionary The dictionary to display.
	@exception IllegalArgumentException Thrown if a valid dictionary is not given.
	*/
	public DictionaryPanel(final Dictionary dictionary)
	{
		this(new RDF(), dictionary);	//construct and initialize the panel with a default RDF data model
	}

	/**Dictionary constructor.
	@param rdf The RDF data model in which the dictionary lies.
	@param dictionary The dictionary to display.
	@exception IllegalArgumentException Thrown if a valid dictionary is not given.
	*/
	public DictionaryPanel(final RDF rdf, final Dictionary dictionary)
	{
		this(rdf, dictionary, true);	//construct and initialize the panel
	}

	/**Dictionary constructor with optional initialization.
	@param rdf The RDF data model in which the dictionary lies.
	@param dictionary The dictionary to display.
	@param initialize <code>true</code> if the panel should initialize itself by
		calling the initialization methods.
	@exception IllegalArgumentException Thrown if a valid dictionary is not given.
	*/
	public DictionaryPanel(final RDF rdf, final Dictionary dictionary, final boolean initialize)
	{
		super(rdf, dictionary, false);	//construct the parent class without initializing it
		setSupportedDataViews(getSupportedModelViews()|WYSIWYG_MODEL_VIEW);	//show that we now support WYSIWYG data views, too
//G***del		wysiwygTextPane=new XMLTextPane();	//create a new XML text pane for the WYSIWYG view
		book=new Book(1);	//create a new book for the WYSIWYG view, showing only one page
//G***del		wysiwygScrollPane=new JScrollPane(wysiwygTextPane);	//create a new scroll pane with the dictionary text pane inside
		if(initialize)  //if we should initialize
			initialize();   //initialize the panel
	}

	/**Initialize the user interface.*/
	protected void initializeUI()
	{
		addView(WYSIWYG_MODEL_VIEW, "Dictionary", book, null, 0);	//add the WYSIWYG component as the tree view G***i18n
		setDefaultDataView(WYSIWYG_MODEL_VIEW);	//set the WYSIWYG view as the default view
		super.initializeUI(); //do the default UI initialization
//G***del		wysiwygTextPane.setContentType(MediaType.APPLICATION_XHTML_XML);	//set the text pane content type to "application/xhtml+xml"
//G***del		wysiwygTextPane.setEditable(false);	//don't let the WYSIWYG text pane be edited
//TODO set the book to be not editable
	}

	/**Loads the data from the model to the given view.
	@param modelView The view of the data that should be loaded.
	@exception IOException Thrown if there was an error loading the model.
	*/
	protected void loadModel(final int modelView) throws IOException
	{
		super.loadModel(modelView);	//do the default loading
		switch(modelView)	//see which view of data we should load
		{
			case WYSIWYG_MODEL_VIEW:	//if we're changing to the WYSIWYG view
				{
					final Document xhtmlDocument=XHTMLUtilities.createXHTMLDocument();	//create an XHTML document
					final Element bodyElement=XHTMLUtilities.getBodyElement(xhtmlDocument);	//get the body element
					Debug.assert(bodyElement!=null, "Missing <body> element in default XHTML document.");
						//set the title
					final Locale dictionaryLanguage=getDictionary().getDictionaryLanguage();	//get the language of the entries
					final String languageTitle=dictionaryLanguage!=null ? dictionaryLanguage.getDisplayLanguage()+" " : "";	//get the language part of the title
					final Element h1Element=XMLUtilities.appendElement(bodyElement, XHTMLConstants.XHTML_NAMESPACE_URI.toString(), XHTMLConstants.ELEMENT_H1, languageTitle+"Dictionary");	//G***i18n
					if(getDictionary()!=null && getDictionary().getEntries()!=null)	//if we have a dictionary and it has entries
					{
						final Element dlElement=XMLUtilities.appendElement(bodyElement, XHTMLConstants.XHTML_NAMESPACE_URI.toString(), XHTMLConstants.ELEMENT_DL);
						final Iterator entryIterator=getDictionary().getEntries().iterator();	//get an iterator to look at all the dictionary entries
						while(entryIterator.hasNext())	//while there are more dictionary entries
						{
								//TODO check for null for each of the entry properties
							final Entry entry=(Entry)entryIterator.next();	//get the next entry
							final RDFPlainLiteral orthography=entry.getOrthography();	//get the entry orthography
							RDFPlainLiteral plainLiteralPronunciation=null;	//we'll see if there is pronunciation text
							RDFResource resourcePronunciation=null;	//we'll see if there is a pronunciation object
							final Iterator pronunciationIterator=entry.getPronunciationIterator();	//get an iterator to all pronunciations
							while(pronunciationIterator.hasNext())	//while there are more pronunciations
							{
								final Object pronunciation=pronunciationIterator.next();	//get the next pronunciation
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
								XMLUtilities.appendText(ddElement, CharacterConstants.SPACE_CHAR);	//add a space
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
									XMLUtilities.appendText(ddElement, CharacterConstants.SPACE_CHAR);	//add a space
								}
							}
							final RDFPlainLiteral translation=entry.getTranslation();	//get the entry translation
							if(translation!=null)	//if there is a translation
							{
								XMLUtilities.appendText(ddElement, translation.toString());	//show the translation
								XMLUtilities.appendText(ddElement, CharacterConstants.SPACE_CHAR);	//add a space
							}
							final RDFPlainLiteral definition=entry.getDefinition();	//get the entry definition
							if(definition!=null)	//if there is a definition
							{
								XMLUtilities.appendText(ddElement, CharacterConstants.EM_DASH_CHAR);	//add an em-dash
								XMLUtilities.appendText(ddElement, definition.toString());	//show the definition
								XMLUtilities.appendText(ddElement, CharacterConstants.SPACE_CHAR);	//add a space
							}
						}
					}
						//show the XML in the book, specifying the base URI of the RDF data model
					book.setXML(xhtmlDocument, getRDF().getBaseURI(), new MediaType(MediaType.APPLICATION_XHTML_XML));
//G***del					final XMLDocument swingDocument=(XMLDocument)wysiwygTextPane.getDocument();	//get the Swing XML document G***this may change if setXML() moves from the editor kit to the document 
						//put the XHTML into the WYSIWYG text pane
//G***del					((XMLEditorKit)wysiwygTextPane.getEditorKit()).setXML(xhtmlDocument, null, new MediaType(MediaType.APPLICATION_XHTML_XML), swingDocument);
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
//G***del				wysiwygTextPane.setDocument(wysiwygTextPane.getEditorKit().createDefaultDocument());	//to conserve memory, remove the content from the editor kit by installing a new document
				book.close();	//to conserve memory, remove the content from the book
				break;
		}
	}

}