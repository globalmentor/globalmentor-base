package com.garretwilson.swing.rdf.dicto;

import java.util.Iterator;

import javax.swing.*;
import com.garretwilson.swing.*;
import com.garretwilson.swing.rdf.RDFPanel;
import com.garretwilson.swing.text.xml.XMLDocument;
import com.garretwilson.swing.text.xml.XMLEditorKit;
import com.garretwilson.text.xml.XMLUtilities;
import com.garretwilson.text.xml.xhtml.XHTMLConstants;
import com.garretwilson.text.xml.xhtml.XHTMLUtilities;
import com.garretwilson.util.Debug;
import com.garretwilson.io.MediaType;
import com.garretwilson.rdf.*;
import com.garretwilson.rdf.dicto.*;
import org.w3c.dom.*;

/**A panel to view and edit a Dictionary Ontology (Dicto) dictionary.
@author Garret Wilson
*/
public class DictionaryPanel extends RDFPanel
{

	/**@return The dictionary being displayed.*/
	public Dictionary getDictionary() {return (Dictionary)getResource();}

	/**The scroll pane for the dictionary WYSIWYG view.*/
	protected final JScrollPane wysiwygScrollPane;

	/**The text pane for the WYSIWYG view.*/
	protected final JTextPane wysiwygTextPane;

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
		setSupportedDataViews(getSupportedDataViews()|WYSIWYG_DATA_VIEW);	//show that we now support WYSIWYG data views, too
		wysiwygTextPane=new XMLTextPane();	//create a new XML text pane for the WYSIWYG view
		wysiwygScrollPane=new JScrollPane(wysiwygTextPane);	//create a new scroll pane with the dictionary text pane inside
		if(initialize)  //if we should initialize
			initialize();   //initialize the panel
	}

	/**Initialize the user interface.*/
	protected void initializeUI()
	{
		super.initializeUI(); //do the default UI initialization
		wysiwygTextPane.setContentType(MediaType.APPLICATION_XHTML_XML);	//set the text pane content type to "application/xhtml+xml"
		wysiwygTextPane.setEditable(false);	//don't let the WYSIWYG text pane be edited
		getTabbedPane().insertTab("Dictionary", null, wysiwygScrollPane, null, 0);	//G***i18n	
		setViewComponent(WYSIWYG_DATA_VIEW, wysiwygScrollPane);	//associate the WYSIWYG component with the tree view
		setDataView(WYSIWYG_DATA_VIEW);	//set the default view
	}

	/**Indicates that the view of the data has changed.
	@param oldView The view before the change.
	@param newView The new view of the data
	*/
	protected void onViewChanged(final int oldView, final int newView)
	{
		super.onViewChanged(oldView, newView);	//do the default view updating
		switch(oldView)	//see what view we're changing from
		{
			case WYSIWYG_DATA_VIEW:	//if we're changing from the WYSIWYG view
				wysiwygTextPane.setDocument(wysiwygTextPane.getEditorKit().createDefaultDocument());	//to conserve memory, remove the content from the editor kit by installing a new document
				break;
		}
		switch(newView)	//see what view we're changing to
		{
			case WYSIWYG_DATA_VIEW:	//if we're changing to the WYSIWYG view
				{
					final Document xhtmlDocument=XHTMLUtilities.createXHTMLDocument();	//create an XHTML document
					final Element bodyElement=XHTMLUtilities.getBodyElement(xhtmlDocument);	//get the body element
					Debug.assert(bodyElement!=null, "Missing <body> element in default XHTML document.");
						//set the title
					final Element h1Element=XMLUtilities.appendElement(bodyElement, XHTMLConstants.XHTML_NAMESPACE_URI.toString(), XHTMLConstants.ELEMENT_H1, "Dictionary");	//TODO i18n
					if(getDictionary()!=null && getDictionary().getEntries()!=null)	//if we have a dictionary and it has entries
					{
						final Element dlElement=XMLUtilities.appendElement(bodyElement, XHTMLConstants.XHTML_NAMESPACE_URI.toString(), XHTMLConstants.ELEMENT_DL);
						final Iterator entryIterator=getDictionary().getEntries().iterator();	//get an iterator to look at all the dictionary entries
						while(entryIterator.hasNext())	//while there are more dictionary entries
						{
								//TODO check for null for each of the entry properties
							final Entry entry=(Entry)entryIterator.next();	//get the next entry
							final RDFPlainLiteral orthography=entry.getOrthography();	//get the entry orthography
							final Element dtElement=XMLUtilities.appendElement(dlElement, XHTMLConstants.XHTML_NAMESPACE_URI.toString(),
									XHTMLConstants.ELEMENT_DT, orthography.toString());	//show the entry orthography TODO add xml:lang to all of these terms
							final Element ddElement=XMLUtilities.appendElement(dlElement, XHTMLConstants.XHTML_NAMESPACE_URI.toString(),
									XHTMLConstants.ELEMENT_DT);	//create the definition element
							final RDFPlainLiteral transliteration=entry.getTransliteration();	//get the entry transliteration
							XMLUtilities.appendElement(ddElement, XHTMLConstants.XHTML_NAMESPACE_URI.toString(),
									XHTMLConstants.ELEMENT_EM, transliteration.toString());	//show the entry transliteration TODO add xml:lang to all of these terms
							final RDFPlainLiteral translation=entry.getTranslation();	//get the entry translation
							XMLUtilities.appendText(ddElement, " "+translation);	//show the translation TODO use a constant for the space
						}
					}
					final XMLDocument swingDocument=(XMLDocument)wysiwygTextPane.getDocument();	//get the Swing XML document G***this may change if setXML() moves from the editor kit to the document 
						//put the XHTML into the WYSIWYG text pane
					((XMLEditorKit)wysiwygTextPane.getEditorKit()).setXML(xhtmlDocument, null, new MediaType(MediaType.APPLICATION_XHTML_XML), swingDocument);
				}
				break;
		}

	}

}