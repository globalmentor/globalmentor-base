package com.garretwilson.swing.rdf.maqro;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.io.IOException;
import javax.swing.*;
import com.garretwilson.swing.*;
import com.garretwilson.swing.rdf.RDFPanel;
import com.garretwilson.text.xml.XMLUtilities;
import com.garretwilson.text.xml.xhtml.XHTMLConstants;
import com.garretwilson.text.xml.xhtml.XHTMLUtilities;
import com.garretwilson.io.MediaType;
import com.garretwilson.rdf.RDFResourceModel;
import com.garretwilson.rdf.maqro.*;
import com.garretwilson.resources.icon.IconResources;
import com.globalmentor.mentoract.activity.maqro.*;

import org.w3c.dom.*;

/**A panel to view and edit a MAQRO activity.
@author Garret Wilson
*/
public class ActivityPanel extends RDFPanel
{

	/**The action for interacting with the activity.*/
	private final Action interactAction;

		/**@return The action for interacting with the activity.*/
		public Action getQuizAction() {return interactAction;}

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
	@see #setRDFResourceModel(Model)
	*/
	public void setActivityModel(final ActivityModel model)
	{
		setRDFResourceModel(model);	//set the model
	}

	/**Sets the data model.
	@param newModel The data model for which this component provides a view.
	@exception ClassCastException Thrown if the model is not an <code>ActivityModel</code>.
	*/
	public void setRDFResourceModel(final RDFResourceModel newModel)
	{
		final ActivityModel activityModel=(ActivityModel)newModel;	//cast the model to an activity model
		book.getXMLTextPane().setURIInputStreamable(activityModel);	//make sure the text pane knows from where to get input streams
		interactionSequencePanel.setList(activityModel.getActivity()!=null ? activityModel.getActivity().getInteractions() : null);	//G***testing; fix
		super.setRDFResourceModel(activityModel);	//set the model in the parent class
	}

	/**Default constructor.*/
	public ActivityPanel()
	{
		this(new ActivityModel());	//construct the panel with a default model
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
		interactAction=new InteractAction();	//create an action for interacting with the activity
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
		add(ToolBarUtilities.createToolBar(getToolBarActions()), BorderLayout.NORTH);	//put a toolbar in the north
		//TODO fix status bar
	}

	/**@return An array of actions to use in a toolbar, with any
		<code>null</code> actions representing separators.
	*/
	public Action[] getToolBarActions()
	{
		return new Action[]{getQuizAction()};	//return the toolbar actions for this panel
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
				book.getXMLTextPane().setURIInputStreamable(model);	//make sure the text pane knows from where to get input streams
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
			case SEQUENCE_MODEL_VIEW:	//if we're changing to the sequence view
					//if the sequence panel isn't showing our interactions
				if(model.getActivity().getInteractions()!=null && interactionSequencePanel.getList()!=model.getActivity().getInteractions())
				{
					interactionSequencePanel.setList(model.getActivity().getInteractions());	//show the list in the sequence panel
				}
				break;	//TODO reset this after we change from the view
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

	/**Interacts with the activity.*/
	public void interact()
	{
/*TODO fix with non-ResourceApplicationFrame
		final ActivityModel model=getActivityModel();	//get our model
		if(verify() && model.getActivity()!=null)	//verify the contents of the user interface; if things verify and we have an activity
		{
			final MAQROActivityEngine activityEngine=new MAQROActivityEngine(model.getActivity());	//create an engine for the activity
			activityEngine.setBaseURI(model.getBaseURI());	//set the base URI of the engine TODO probably make the resource application panel URIAccessible
			activityEngine.setURIInputStreamable(model);	//tell the activity engine to use our URI sourcefor reading
			final MAQROActivityPanel activityPanel=new MAQROActivityPanel(activityEngine);	//create a new activity panel for the engine
			final ResourceApplicationFrame activityFrame=new ResourceApplicationFrame(activityPanel);	//construct an activity frame without initializing it TODO eventually switch to a more generic application frame
			activityFrame.setVisible(true);	//show the activity frame
			activityEngine.start();	//start the interaction
		}
*/
	}

	/**Activity action that allows quizing on the dictionary's contents.*/
	protected class InteractAction extends AbstractAction
	{
		/**Default constructor.*/
		public InteractAction()
		{
			super("Interact");	//create the base class G***i18n
			putValue(SHORT_DESCRIPTION, "Interactive Activity");	//set the short description G***i18n
			putValue(LONG_DESCRIPTION, "Test the interactive activity.");	//set the long description G***i18n
			putValue(MNEMONIC_KEY, new Integer('i'));  //set the mnemonic key; for some reason, 's' causes the action to be activated when Alt+F4 is pressed G***i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.ANIMATION_ICON_FILENAME)); //load the correct icon
//G***del			putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_Q, KeyEvent.CTRL_MASK)); //add the accelerator G***i18n
		}

		/**Called when the action should be performed.
		@param actionEvent The event causing the action.
		*/
		public void actionPerformed(final ActionEvent actionEvent)
		{
			interact();	//interact with the activity
		}
	}

}