package com.garretwilson.swing.rdf.maqro;

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
import com.garretwilson.rdf.maqro.*;
import com.garretwilson.rdf.xpackage.XPackageUtilities;

import org.w3c.dom.*;

/**A panel to view and edit a MAQRO activity.
@author Garret Wilson
*/
public class ActivityPanel extends RDFPanel
{

	/**The book for the WYSIWYG view.*/
	protected final Book book;

	/**The panel representing a sequence of ineractions.*/
	protected final InteractionSequencePanel interactionSequencePanel;

	/**@return The data model for which this component provides a view.
	@see RDFPanel#getRDFResourceModel()
	*/
	public ActivityModel getActivityModel() {return (ActivityModel)getRDFResourceModel();}

	/**Sets the data model.
	@param model The data model for which this component provides a view.
	@see RDFPanel#setRDFResourceModel(Model)
	*/
	public void setActivityModel(final ActivityModel model)
	{
		book.getXMLTextPane().setURIInputStreamable(model.getURIInputStreamable());	//make sure the text pane knows from where to get input streams
		interactionSequencePanel.setList(model.getActivity()!=null ? model.getActivity().getInteractions() : null);	//G***testing; fix
		setRDFResourceModel(model);	//set the model
	}

	/**Model constructor.
	@param model The data model for which this component provides a view.
	*/
	public ActivityPanel(final ActivityModel model)
	{
		this(model, true);	//construct and initialize the panel
	}

	/**Model constructor with optional initialization.
	@param model The data model for which this component provides a view.
	@param initialize <code>true</code> if the panel should initialize itself by
		calling the initialization methods.
	*/
	public ActivityPanel(final ActivityModel model, final boolean initialize)
	{
		super(model, false);	//construct the parent class without initializing it
		addSupportedModelViews(new int[]{WYSIWYG_MODEL_VIEW, SEQUENCE_MODEL_VIEW});	//show that we now support WYSIWYG and sequence data views, too
		book=new Book(1);	//create a new book for the WYSIWYG view, showing only one page
		interactionSequencePanel=new InteractionSequencePanel(model.getActivity()!=null ? model.getActivity().getInteractions() : null);	//G***make sure we want to possibly initialize without a list
		if(initialize)  //if we should initialize
			initialize();   //initialize the panel
	}

	/**Initialize the user interface.*/
	protected void initializeUI()
	{
		addView(WYSIWYG_MODEL_VIEW, "Activity", book, null);	//add the book component as the WYSIWYG view G***i18n
		addView(SEQUENCE_MODEL_VIEW, "Interactions", interactionSequencePanel, null);	//add the interaction sequence panel as the sequence view G***i18n
		setDefaultDataView(WYSIWYG_MODEL_VIEW);	//set the WYSIWYG view as the default view
		super.initializeUI(); //do the default UI initialization
//TODO set the book to be not editable
	}

	/**Loads the data from the model to the view, if necessary.
	@exception IOException Thrown if there was an error loading the model.
	*/
	protected void loadModel() throws IOException
	{
		super.loadModel();	//do the default loading
		final ActivityModel model=getActivityModel();	//get the data model
		switch(getModelView())	//see which view of data we should load
		{
			case WYSIWYG_MODEL_VIEW:	//if we're changing to the WYSIWYG view
				book.getXMLTextPane().setURIInputStreamable(model.getURIInputStreamable());	//make sure the text pane knows from where to get input streams
				if(model.getActivity()!=null)	//if we have an activity
				{
					final Activity activity=model.getActivity();	//get the activity represented by the model
					final Document xhtmlDocument=XHTMLUtilities.createXHTMLDocument();	//create an XHTML document
					final Element bodyElement=XHTMLUtilities.getBodyElement(xhtmlDocument);	//get the body element
					assert bodyElement!=null : "Missing <body> element in default XHTML document.";
						//TODO fix activity WYSIWYG view
					final Element h1Element=XMLUtilities.appendElement(bodyElement, XHTMLConstants.XHTML_NAMESPACE_URI.toString(), XHTMLConstants.ELEMENT_H1, "Test Activity");	//G***i18n
						//show the XML in the book, specifying the base URI of the RDF data model
					book.setXML(xhtmlDocument, model.getBaseURI(), new MediaType(MediaType.APPLICATION_XHTML_XML));
				}
				else	//if we don't have an activity
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

}