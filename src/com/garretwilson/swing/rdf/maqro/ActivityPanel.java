package com.garretwilson.swing.rdf.maqro;

import java.awt.BorderLayout;
import java.awt.event.*;
import java.io.IOException;
import java.util.*;
import javax.swing.*;
import com.garretwilson.swing.*;
import com.garretwilson.swing.rdf.RDFPanel;
import com.garretwilson.text.xml.XMLUtilities;
import com.garretwilson.text.xml.xhtml.XHTMLConstants;
import com.garretwilson.text.xml.xhtml.XHTMLUtilities;
import com.garretwilson.io.MediaType;
import com.garretwilson.model.Model;
import com.garretwilson.rdf.RDFListResource;
import com.garretwilson.rdf.RDFPlainLiteral;
import com.garretwilson.rdf.maqro.*;
import com.garretwilson.resources.icon.IconResources;
import com.globalmentor.mentoract.activity.maqro.*;

import org.w3c.dom.*;

/**A panel to view and edit a MAQRO activity.
@author Garret Wilson
*/
public class ActivityPanel extends RDFPanel
{

	/**The action for adding an interaction to the activity.*/
	private final Action addInteractionAction;

		/**@return The action for adding an interaction to the activity.*/
		public Action getAddInteractionAction() {return addInteractionAction;}

	/**The action for interacting with the activity.*/
	private final Action interactAction;

		/**@return The action for interacting with the activity.*/
		public Action getInteractAction() {return interactAction;}

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
	public void setModel(final Model newModel)
	{
		final ActivityModel activityModel=(ActivityModel)newModel;	//cast the model to an activity model
/*G***del when works
		book.getXMLTextPane().setURIInputStreamable(activityModel);	//make sure the text pane knows from where to get input streams
		if(activityModel.getActivity()==null)	//if there is no activity
		{
			activityModel.setActivity(new Activity());	//create a new activity
		}
		if(activityModel.getActivity().getInteractions()==null)	//if the activity has no interactions
		{
			activityModel.getActivity().setInteractions(new RDFListResource());	//set a default list of interactions
		}
		interactionSequencePanel.setListModel(new ListListModel(activityModel.getActivity().getInteractions()));	//create a list model from the interactions to show in the sequence panel
//G***del if not needed		interactionSequencePanel.setList(activityModel.getActivity()!=null ? activityModel.getActivity().getInteractions() : null);	//G***testing; fix
*/
		super.setModel(activityModel);	//set the model in the parent class
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
		addInteractionAction=new AddInteractionAction();	//create an action for adding an interaction to the activity
		interactAction=new InteractAction();	//create an action for interacting with the activity
		book=new Book(1);	//create a new book for the WYSIWYG view, showing only one page
		interactionSequencePanel=new InteractionSequencePanel();	//create a new interaction sequence panel
		if(initialize)  //if we should initialize
			initialize();   //initialize the panel
	}

	/**Initializes actions in the action manager.
	@param actionManager The implementation that manages actions.
	*/
	protected void initializeActions(final ActionManager actionManager)
	{
		super.initializeActions(actionManager);	//do the default initialization
		actionManager.addToolAction(getAddInteractionAction());
		actionManager.addToolAction(new ActionManager.SeparatorAction());
		actionManager.addToolAction(getInteractAction());
		add(ToolBarUtilities.createApplicationToolBar(getActionManager()), BorderLayout.NORTH);	//put a toolbar in the north with our tool actions
	}

	/**Initialize the user interface.*/
	protected void initializeUI()
	{
		addView(WYSIWYG_MODEL_VIEW, "Activity", book, null);	//add the book component as the WYSIWYG view G***i18n
		addView(SEQUENCE_MODEL_VIEW, "Interactions", interactionSequencePanel, null);	//add the interaction sequence panel as the sequence view G***i18n
		setDefaultDataView(WYSIWYG_MODEL_VIEW);	//set the WYSIWYG view as the default view
		super.initializeUI(); //do the default UI initialization
//TODO set the book to be not editable
		//TODO fix status bar
	}

	/**Loads the data from the model to the view, if necessary.
	@exception IOException Thrown if there was an error loading the model.
	*/
	protected void loadModel() throws IOException
	{
		super.loadModel();	//do the default loading
		final ActivityModel model=getActivityModel();	//get the data model

		book.getXMLTextPane().setURIInputStreamable(model);	//make sure the text pane knows from where to get input streams
		if(model.getActivity()==null)	//if there is no activity
		{
			model.setActivity(new Activity());	//create a new activity
		}
		if(model.getActivity().getInteractions()==null)	//if the activity has no interactions
		{
			model.getActivity().setInteractions(new RDFListResource());	//set a default list of interactions
		}
			//if the sequence panel isn't showing any interactions or it's not showing our interactions
//G***del when works		if(interactionSequencePanel.getListModel()==null || ((ListListModel)interactionSequencePanel.getListModel()).getList()!=model.getActivity().getInteractions())
		{
			interactionSequencePanel.setListModel(new ListListModel(model.getActivity().getInteractions()));	//create a list model from the interactions to show in the sequence panel
		}
	}

	/**Loads the data from the model to the specified view, if necessary.
	@param modelView The view of the data, such as <code>SUMMARY_MODEL_VIEW</code>.
	@exception IOException Thrown if there was an error loading the model.
	*/
	protected void loadModel(final int modelView) throws IOException
	{
		final ActivityModel model=getActivityModel();	//get the data model
		switch(modelView)	//see which view of data we should load
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
/*G***fix
			case SEQUENCE_MODEL_VIEW:	//if we're changing to the sequence view
				if(model.getActivity()==null || model.getActivity().getInteractions()==null)	//if we don't have an activity or the activity has no interactions
				{
					interactionSequencePanel.setListModel(null);	//remove any list shown G***won't we want to do this when the model changes?
				}
				else if(((ListListModel)interactionSequencePanel.getListModel()).getList()!=model.getActivity().getInteractions())	//if the sequence panel isn't showing our interactions
				{
					interactionSequencePanel.setListModel(new ListListModel(model.getActivity().getInteractions()));	//create a list model from the interactions to show in the sequence panel
				}
				break;	//TODO reset this after we change from the view
*/
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

	/**Adds an interaction to the activity.*/
	public void addInteraction()
	{
		final ListListModel interactionListModel=(ListListModel)interactionSequencePanel.getListModel();	//get the list model representing interactions TODO probably keep a single list model around somewhere, especially for when we have different views of the interactions
		final Question question=new Question();	//G***testing
		question.setQuery(new Dialogue(new RDFPlainLiteral("New question.")));
		interactionListModel.add(question);	//add the question to the list
	}

	/**Interacts with the activity.*/
	public void interact()
	{
		final ActivityModel model=getActivityModel();	//get our model
		if(verify() && model.getActivity()!=null)	//verify the contents of the user interface; if things verify and we have an activity
		{
			final MAQROActivityEngine activityEngine=new MAQROActivityEngine(model.getActivity());	//create an engine for the activity
			activityEngine.setBaseURI(model.getBaseURI());	//set the base URI of the engine TODO probably make the resource application panel URIAccessible
			activityEngine.setURIInputStreamable(model);	//tell the activity engine to use our URI sourcefor reading
			final MAQROActivityPanel activityPanel=new MAQROActivityPanel(activityEngine);	//create a new activity panel for the engine
			final ApplicationFrame activityFrame=new ApplicationFrame(activityPanel);	//construct a frame for the activity
			activityFrame.setVisible(true);	//show the activity frame
//G***del			activityEngine.start();	//start the interaction
		}
	}

	/**Action to add an interaction.*/
	protected class AddInteractionAction extends AbstractAction
	{
		/**Default constructor.*/
		public AddInteractionAction()
		{
			super("Add Interaction");	//create the base class G***i18n
			putValue(SHORT_DESCRIPTION, "Add interaction");	//set the short description G***i18n
			putValue(LONG_DESCRIPTION, "Add an interaction to the current group.");	//set the long description G***i18n
			putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_A));  //set the mnemonic key G***i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.ADD_ICON_FILENAME)); //load the correct icon
	//G***del			putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_Q, KeyEvent.CTRL_MASK)); //add the accelerator G***i18n
		}
	
		/**Called when the action should be performed.
		@param actionEvent The event causing the action.
		*/
		public void actionPerformed(final ActionEvent actionEvent)
		{
			addInteraction();	//add an interaction
		}
	}

	/**Activity action that allows quizing on the dictionary's contents.*/
	protected class InteractAction extends AbstractAction
	{
		/**Default constructor.*/
		public InteractAction()
		{
			super("Interact");	//create the base class G***i18n
			putValue(SHORT_DESCRIPTION, "Interactive activity");	//set the short description G***i18n
			putValue(LONG_DESCRIPTION, "Test the interactive activity.");	//set the long description G***i18n
			putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_I));  //set the mnemonic key G***i18n
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