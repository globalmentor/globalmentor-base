package com.garretwilson.swing.rdf.maqro;

import java.awt.BorderLayout;
import java.awt.Frame;
import java.awt.event.*;
import java.io.IOException;
import java.util.*;
import javax.mail.internet.ContentType;
import javax.swing.*;
import com.garretwilson.io.ContentTypeConstants;
import com.garretwilson.swing.*;
import com.garretwilson.swing.rdf.RDFPanel;
import com.garretwilson.text.xml.XMLUtilities;
import com.garretwilson.text.xml.xhtml.XHTMLConstants;
import com.garretwilson.text.xml.xhtml.XHTMLUtilities;
import com.garretwilson.model.Model;
import com.garretwilson.rdf.*;
import com.garretwilson.rdf.dublincore.DCUtilities;
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
		public Action getInteractAction() {return interactAction;}

	/**The book for the WYSIWYG view.*/
	protected final Book book;

	/**The strategy for editing interactions.*/
	private final InteractionEditStrategy interactionEditStrategy;

		/**@return The strategy for editing interactions.*/
		protected InteractionEditStrategy getInteractionEditStrategy() {return interactionEditStrategy;}

	/**The panel representing a sequence of ineractions.*/
	protected final InteractionSequencePanel interactionSequencePanel;

	/**The list of interactions in the list view.*/
	protected final JList interactionListComponent;

	/**The panel representing a list of ineractions.*/
	protected final ListPanel interactionListPanel;

	/**The panel that allows editing of the activity behavior settings.*/
	protected final ActivityBehaviorPanel activityBehaviorPanel;
	
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
		activityBehaviorPanel.setRDFResourceModel(activityModel);	//set the new model in the activity behavior panel
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
		addSupportedModelViews(new int[]{WYSIWYG_MODEL_VIEW, SEQUENCE_MODEL_VIEW, LIST_MODEL_VIEW, CONFIGURATION_MODEL_VIEW});	//show that we now support WYSIWYG, sequence, list model, and configuration views, too
		interactAction=new InteractAction();	//create an action for interacting with the activity
		book=new Book(1);	//create a new book for the WYSIWYG view, showing only one page
		interactionListComponent=new JList();	//create a new list for the interactions
		interactionEditStrategy=new InteractionEditStrategy();	//create the edit strategy for interactions
		interactionSequencePanel=new InteractionSequencePanel();	//create a new interaction sequence panel
		interactionListPanel=new ListPanel(interactionListComponent, interactionEditStrategy);	//create a new interaction list panel
		activityBehaviorPanel=new ActivityBehaviorPanel(model);	//create a panel for the behavior panel
		if(initialize)  //if we should initialize
			initialize();   //initialize the panel
	}

	/**Initializes actions in the action manager.
	@param actionManager The implementation that manages actions.
	*/
	protected void initializeActions(final ActionManager actionManager)
	{
		super.initializeActions(actionManager);	//do the default initialization
		actionManager.addToolAction(getInteractionEditStrategy().getAddAction());	//add an action for adding a new interaction
		actionManager.addToolAction(new ActionManager.SeparatorAction());	//--
		actionManager.addToolAction(getInteractAction());	//add an action for testing the activity
		add(getActionManager().addToolComponents(new ApplicationToolBar()), BorderLayout.NORTH);	//put a toolbar in the north with our tool actions
	}

	/**Initialize the user interface.*/
	protected void initializeUI()
	{
		addView(WYSIWYG_MODEL_VIEW, "Activity", book, null);	//add the book component as the WYSIWYG view G***i18n
		addView(SEQUENCE_MODEL_VIEW, "Interaction Sequence", interactionSequencePanel, null);	//add the interaction sequence panel as the sequence view G***i18n
		addView(LIST_MODEL_VIEW, "Interaction List", interactionListPanel, null);	//add the interaction list panel as the list view G***i18n
		addView(CONFIGURATION_MODEL_VIEW, "Behavior", activityBehaviorPanel, null);	//add the activity behavior panel as the configuration/settings view G***i18n
		setDefaultDataView(WYSIWYG_MODEL_VIEW);	//set the WYSIWYG view as the default view
		super.initializeUI(); //do the default UI initialization
//TODO set the book to be not editable
		//TODO fix status bar
		interactionListPanel.setEditable(true);	//allow the interactions to be edited in the list
	}

	/**Loads the data from the model to the view, if necessary.
	@exception IOException Thrown if there was an error loading the model.
	*/
	public void loadModel() throws IOException
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
		final ListModel interactionListModel=new ListListModel(model.getActivity().getInteractions());	//create a new list model from the interaction list G***this will change when we have nested groups, as it will be difficult to keep both the sequence and the list in synch; we may want to switch to load/save on view change
		interactionSequencePanel.setListModel(interactionListModel);	//put the interaction list model in the sequence panel
		interactionListComponent.setModel(interactionListModel);	//put the interaction list model in the interaction list component in the interaction list view G***make sure changing the model here keeps everything else in synch
	}

	/**Loads the data from the model to the specified view, if necessary.
	@param modelView The view of the data, such as <code>SUMMARY_MODEL_VIEW</code>.
	@exception IOException Thrown if there was an error loading the model.
	*/
	protected void loadModel(final int modelView) throws IOException
	{
		super.loadModel(modelView);	//do the default loading
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
						//set the title
					final RDFLiteral title=RDFUtilities.asLiteral(DCUtilities.getTitle(activity));	//get the activity's title
					if(title!=null)	//if there is a title
					{
						final Element h1Element=XMLUtilities.appendElement(bodyElement, XHTMLConstants.XHTML_NAMESPACE_URI.toString(), XHTMLConstants.ELEMENT_H1, title.toString());	//G***i18n
					}
					if(activity.getInteractions()!=null)	//if the activity has interactions
					{
						final Element olElement=XMLUtilities.appendElement(bodyElement, XHTMLConstants.XHTML_NAMESPACE_URI.toString(), XHTMLConstants.ELEMENT_OL);
						final Iterator interactionIterator=activity.getInteractions().iterator();	//get an iterator to look at all the activity interactions
						while(interactionIterator.hasNext())	//while there are more interactions
						{
							final Interaction interaction=(Interaction)interactionIterator.next();	//get the next interaction
							final Element interactionElement=createElement(xhtmlDocument, interaction);	//create an XML element from this interaction
							final Element liElement=XMLUtilities.appendElement(olElement, XHTMLConstants.XHTML_NAMESPACE_URI.toString(), XHTMLConstants.ELEMENT_LI);
							liElement.appendChild(interactionElement);	//append this interaction element to the list item element
						}
					}
						//show the XML in the book, specifying the base URI of the RDF data model
					book.setXML(xhtmlDocument, model.getBaseURI(), XHTMLConstants.XHTML_CONTENT_TYPE);
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
			case CONFIGURATION_MODEL_VIEW:	//if we're changing to the configuration view
				activityBehaviorPanel.loadModel();	//tell the configuration view to load the model
				break;
/*G***testing---why does the list view incorrectly size the list, exclusing the last item?					
			case LIST_MODEL_VIEW:	//G***testing
//G***testing				interactionListComponent.setVisibleRowCount(interactionListComponent.getModel().getSize());
				{
					final Frame frame=JOptionPane.getFrameForComponent(this);	//G***testing
					if(frame!=null)
					{
						frame.invalidate();	//G***testing
						frame.validate();	//G***testing
					}
				}
				break;
*/
		}
	}

	/**Stores the current data being edited to the model, if necessary.
	@param modelView The view of the data, such as <code>SUMMARY_MODEL_VIEW</code>.
	@exception IOException Thrown if there was an error saving the model.
	*/
	protected void saveModel(final int modelView) throws IOException
	{
		super.saveModel(modelView);	//do the default saving
		final ActivityModel model=getActivityModel();	//get the data model
		switch(modelView)	//see which view of data we should save
		{
			case CONFIGURATION_MODEL_VIEW:	//if we're saving the configuration view
				activityBehaviorPanel.saveModel();	//tell the configuration view to save the model
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

	/**Creates an XML element to represent the given interaction.
	@param document The XHTML document that serves as an element factory.
	@param interaction The interaction to represent in XHTML.
	@return An XML element representing the given interaction.
	*/
	protected Element createElement(final Document document, final Interaction interaction)
	{
		if(interaction instanceof Question)	//if this is a question
		{
			final Question question=(Question)interaction;	//cast the interaction to a question
			final Element questionElement=document.createElementNS(XHTMLConstants.XHTML_NAMESPACE_URI.toString(), XHTMLConstants.ELEMENT_DIV);	//create an element for the entire question
			final Dialogue query=question.getQuery();	//get the question's query
			if(query!=null)	//if we have a query
			{
				final Element queryElement=XMLUtilities.appendElement(questionElement, XHTMLConstants.XHTML_NAMESPACE_URI.toString(), XHTMLConstants.ELEMENT_DIV);	//append an element for the query
				appendDialogue(document, queryElement, query);	//append the query
			}
			final RDFListResource choices=question.getChoices();	//get the list of choices
			if(choices!=null)	//if there are choices
			{
				final Element olElement=XMLUtilities.appendElement(questionElement, XHTMLConstants.XHTML_NAMESPACE_URI.toString(), XHTMLConstants.ELEMENT_OL);
				final Iterator choiceIterator=choices.iterator();	//get an iterator to the choices
				while(choiceIterator.hasNext())	//while there are more choices
				{
					final RDFObject choice=(RDFObject)choiceIterator.next();	//get the next choice
					if(choice instanceof Dialogue)	//if the choice is dialogue
					{
						final Dialogue choiceDialogue=(Dialogue)choice;	//cast the choice to dialogue
						final Element choiceElement=XMLUtilities.appendElement(olElement, XHTMLConstants.XHTML_NAMESPACE_URI.toString(), XHTMLConstants.ELEMENT_LI);	//append an element for the choice
						appendDialogue(document, choiceElement, choiceDialogue);	//append the choice
					}
				}
			}
			return questionElement;	//return the element we constructed for the question
		}
		throw new AssertionError("Unrecognized interaction type.");	//TODO fix to ignore unrecognized interaction types
	}		

	/**Adds dialogue to an XML element.
	@param document The XHTML document that serves as an element factory.
	@param element The element to which the dialogue should be added.
	@param dialogue The The dialogue to add to the element.
	*/
	protected void appendDialogue(final Document document, final Element element, final Dialogue dialogue)
	{
		final RDFLiteral value=dialogue.getValue();	//get the dialogue value
		if(value!=null)	//if the dialogue has a value
		{
			if(value instanceof RDFXMLLiteral)	//if the value is an XML literal
			{
				final DocumentFragment fragment=((RDFXMLLiteral)value).getDocumentFragment();	//get the value's XML fragment
				element.appendChild(document.importNode(fragment, true));	//import the fragment and append it to the element 
			}
			else	//if the value is not an XML literal
			{
				XMLUtilities.appendText(element, value.getLexicalForm());	//append the literal text to the element
			}
		}		
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

	/**The edit strategy that allows editing of choices from a list.
	 @author Garret Wilson
	 */
	protected class InteractionEditStrategy extends ListEditStrategy
	{
		/**Default constructor.*/
		public InteractionEditStrategy()
		{
			super(interactionListComponent, ActivityPanel.this);	//construct the parent class
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
			return new Question();
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
			setModelView(SEQUENCE_MODEL_VIEW);	//switch to the sequence view
			assert getListModel() instanceof List : "The interaction edit strategy relies on an implementation that uses a list model that implements the List interface.";
			final int index=((List)getListModel()).indexOf(item);	//see if the item is in our list
			if(index>=0)	//if the item we're editing is already in our list
			{
				interactionSequencePanel.go(index);	//navigate to the selected index
			}
			return item;	//G***testing
/*TODO fix list editing of interactions
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
*/
		}
	}

}
