package com.garretwilson.swing.text.rdf.maqro;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.*;

import javax.mail.internet.ContentType;
import javax.swing.*;
import javax.swing.text.*;


import static com.garretwilson.io.ContentTypeConstants.*;
import com.garretwilson.io.ContentTypes;
import com.garretwilson.rdf.*;
import static com.garretwilson.rdf.maqro.MAQROConstants.*;
import com.garretwilson.rdf.maqro.*;
import com.garretwilson.resources.icon.IconResources;

import static com.garretwilson.swing.text.rdf.RDFStyleUtilities.*;

import com.garretwilson.swing.BasicOptionPane;
import com.garretwilson.swing.XMLTextPane;
import com.garretwilson.swing.text.xml.*;
import com.garretwilson.text.xml.XMLUtilities;
import com.garretwilson.text.xml.xhtml.XHTML;
import com.garretwilson.util.Debug;
import com.globalmentor.mentoract.activity.maqro.MAQROActivityEngine;

import static com.garretwilson.swing.text.xml.XMLStyleUtilities.*;
import static com.globalmentor.java.Objects.*;

/**Creates views for MAQRO elements.
@author Garret Wilson
TODO decide if we want to check for the choices list, and create a special XMLListView; this would probably be better, as it is more consistent
*/
public class MAQROViewFactory extends XMLViewFactory
{

	/**The map of views, weakly keyed to resources.*/
	protected final Map<RDFResource, View> resourceViewMap=new WeakHashMap<RDFResource, View>();

	/**The map of views, weakly keyed to elements.*/
	protected final Map<Element, View> elementViewMap=new WeakHashMap<Element, View>();
	
	/**Default constructor*/
	public MAQROViewFactory()
	{
		super(); //construct the parent class
	}

	/**Creates a view for the given element. If the element specifies a
		namespace and a view factory has been registered for the given namespace,
		the view creation will be delegated to the designated view factory.
		As this class implements <code>ViewsFactory</code>, which allows multiple
		views to be created, this method can optionally indicate multiple views
		are needed by returning <code>null</code>.
	@param element The element this view will represent.
	@param indicateMultipleViews Whether <code>null</code> should be returned to
		indicate multiple views should represent the given element.
	@return A view to represent the given element, or <code>null</code>
		indicating the element should be represented by multiple views.
	*/
	public View create(final Element element, final boolean indicateMultipleViews)
	{
		View view=null;	//we'll store the created view here
		final Element parentElement=element.getParentElement();	//get the parent element
		final AttributeSet attributeSet=element.getAttributes();  //get the element's attribute set
		if(attributeSet!=null)  //if we have an attribute set
		{
			//G***we should probably make sure this element is in our namespace
			final String elementLocalName=XMLStyleUtilities.getXMLElementLocalName(attributeSet); //get the local name of this element
		  if(ACTIVITY_CLASS_NAME.equals(elementLocalName)) //maqro:Activity
			{
				final Activity activity=asInstance(getRDFResource(attributeSet), Activity.class);	//get the activity
				if(activity!=null)	//if the element stores the activity
				{
					final SubmitAction submitAction=new SubmitAction(activity); //create a new submission action for this activity
					view=new MAQROActivityView(element, View.Y_AXIS, submitAction);  //create a new view for the activity
				}
			}
		  else if(QUESTION_CLASS_NAME.equals(elementLocalName)) //maqro:Question
			{
				final Element activityElement=getAncestorElement(element, MAQRO_NAMESPACE_URI.toString(), ACTIVITY_CLASS_NAME);	//get the element of any enclosing activity
				final MAQROActivityView activityView=activityElement!=null ? asInstance(elementViewMap.get(activityElement), MAQROActivityView.class) : null;	//get the associated activity view, if there is one
				view=new MAQROQuestionView(element, View.Y_AXIS, activityView);	//create a question view				
			}
		  else if(DIALOGUE_CLASS_NAME.equals(elementLocalName)) //maqro:Dialogue
			{
				if(parentElement!=null)	//if there is a parent element
				{
					if(isXMLElement(parentElement.getAttributes(), MAQRO_NAMESPACE_URI.toString(), CHOICES_PROPERTY_NAME))	//if the parent element represents the list of choices maqro:choices
					{
						final Element questionElement=parentElement.getParentElement();	//the parent of the choices should be the question element itself
						if(questionElement!=null)	//if we found the question element
						{
							final MAQROQuestionView questionView=(MAQROQuestionView)elementViewMap.get(questionElement);	//get the view for the question
							final MAQRODialogueChoiceView choiceView=new MAQRODialogueChoiceView(element, questionView);	//create a dialogue choice view
/*TODO fix; transfer to the choice view or question view if we want automatic feedback, for instance
							choiceView.getToggleButton().addItemListener(new ItemListener()	//listen for the response being checked or unchecked
									{
										public void itemStateChanged(final ItemEvent itemEvent)	//if the choice button was toggled
										{
											updateOutcome(questionView.getQ)
//G***del Debug.trace("storing value in response map for ID: ", id);  //G***del
//G***del Debug.trace("storing in response map: ", getResponseMap()); //G***del
												//update the response with a boolean indication of the selection state
											getItemResponseMap(itemIdent).put(ident, new Boolean(responseLabelView.getToggleButton().isSelected()));
//G***del Debug.trace("reponse map now has size: ", getResponseMap().size());  //G***del

//G***del Debug.notify("New state of "+ident+": "+responseLabelView.getToggleButton().isSelected());  //G***testing
//G***del	System.out.println("New state of "+ident+": "+responseLabelView.getToggleButton().isSelected());  //G***testing

										}


									});
*/
							view=choiceView;	//return the view we created for the choice						
						}
					}
				}
				if(view==null)	//if we haven't created a special dialogue view
				{
					view=new XMLParagraphView(element);	//for every other dialogue instances, create a paragraph view
				}
			}
		  else if(CHOICES_PROPERTY_NAME.equals(elementLocalName)) //maqro:choices
			{
				view=new XMLBlockView(element, View.Y_AXIS);	//create block view for choices TODO maybe create a list view				
			}
		}
		if(view==null)	//if we couldn't figure out which kind of view to create
		{
			view=super.create(element, indicateMultipleViews);	//let the parent class decide what to do
		}
		if(view!=null)	//if we now have a view
		{
			elementViewMap.put(element, view);	//key the view to the element
			final RDFResource resource=getRDFResource(attributeSet);	//see if the element represents an RDF resource
			if(resource!=null)	//if the element represents a resource
			{
				resourceViewMap.put(resource, view);	//key the view to the resource
			}
		}
		return view;	//return the created view
	}

/*TODO fix
	protected void updateOutcome(final Interaction interaction)
	{
		
	
	}
*/

	/**Submits an activity.
	@param activity The activity to submit.
	*/
	protected void submit(final Activity activity)
	{
		final View activityView=resourceViewMap.get(activity);	//get the view associated with the activity
		if(activityView!=null)	//if we have a view representing the activity
		{
			final Component component=asInstance(activityView.getContainer(), Component.class);	//get the component, if there is one, associated with the activity view
			final MAQROActivityEngine activityEngine=new MAQROActivityEngine(activity);	//create a new activity engine
			activityEngine.start();	//start the engine
			try
			{
				while(activityEngine.hasNext())	//store all the responses; while there are more items
				{
					activityEngine.goNext();	//go to the next item
					final Interaction interaction=activityEngine.getItem();	//get the current item
					if(interaction!=null)	//if we have an interaction
					{
						final View interactionView=resourceViewMap.get(interaction);	//get the view associated with this interaction
						if(interactionView instanceof Outcomable)	//if we can get the outcome from this view
						{
							final Outcome outcome=((Outcomable)interactionView).getOutcome();	//get the interation outcome, if there is one
							activityEngine.setResult(interaction, outcome);	//tell the engine the result of the interaction
/*TODO del if not needed
							if(outcome.hasCorrect() && !outcome.isCorrect())	//if the outcome is marked as incorrect
							{
								incorrectInteractionIndexList.add(Integer.valueOf(activityEngine.getItemIndex()));	//add this incorrect index to our list
							}
*/
						}
					}
				}
				if(activityEngine.isConfirmSubmit())	//if we should explicitly confirm a commit
				{
						//ask the user for confimation to submit the activity; if the user doesn't really want to submit
					if(BasicOptionPane.showConfirmDialog(component,
							"Are you sure you want to submit the activity?", "Confirm submit", BasicOptionPane.OK_CANCEL_OPTION)!=BasicOptionPane.OK_OPTION)	//G***i18n
					{
						return;	//return without submitting
					}
				}
				final Outcome outcome=activityEngine.submit(); //tell the engine to submit the results, and get the results TODO add more generic
/*TODO del if not needed
				final String scoreString=MAQROActivityEngine.getScoreString(outcome);	//try to get a score string
				final StringBuffer resultStringBuffer=new StringBuffer(); //create a new string buffer in which to construct the results display
				resultStringBuffer.append("<html>");
	//G***fix		  resultStringBuffer.append("<h1>Assessment Results</h1>");
				if(scoreString!=null)	//if there is a score string
				{
					resultStringBuffer.append("<p><strong>Score:</strong> ").append(scoreString).append("</p>");	//append the score
				}
				if(incorrectInteractionIndexList.size()>0)	//if they missed any questions at all
				{
					resultStringBuffer.append("<p><strong>Questions Missed:</strong>");	//start the questions missed section TODO i18n
					for(final Integer incorrectInteractionIndex:incorrectInteractionIndexList)	//for each incorect interaction index
					{
						resultStringBuffer.append(' ').append(incorrectInteractionIndex.intValue()+1).append(',');	//show this interaction number
					}
					resultStringBuffer.setCharAt(resultStringBuffer.length()-1, '.');	//change the last comma to a period
					resultStringBuffer.append("</p>");	//end the questions missed section
				}
			  resultStringBuffer.append("</html>");
*/
				final MAQROXHTMLifier maqroXHTMLifier=new MAQROXHTMLifier();	//create an object to create XHTML from a MAQRO outcome
				final org.w3c.dom.Document document=XHTML.createXHTMLDocument("", true);	//create an XHTML document
					//create an element document from the outcome and append it to the XHTML body element
				XHTML.getBodyElement(document).appendChild(maqroXHTMLifier.createElement(document, outcome));
				final XMLTextPane outcomeTextPane=new XMLTextPane();	//TODO create a content type constructor
				final ContentType contentType=new ContentType(APPLICATION, XHTML_XML_SUBTYPE, null);	//create an application/xhtml+xml content type
				outcomeTextPane.setContentType(contentType.toString());	//set the content type to application/xhtml+xml
				outcomeTextPane.setXML(document, null, contentType);	//set the XML in the text pane
				outcomeTextPane.setEditable(false);	//don't allow the text pane to be editable
				
//TODO fix				final String outcomeXHTMLString=XMLUtilities.toString(document.getDocumentElement());	//get a string version of the entire XHTML document
/*TODO fix
				final MAQROXHTMLifier maqroXHTMLifier=new MAQROXHTMLifier();	//create an object to create XHTML from a MAQRO outcome
				final org.w3c.dom.Document document=XHTMLUtilities.createXHTMLDocument();	//create an XHTML document
					//create an element document from the outcome and append it to the XHTML body element
				XHTMLUtilities.getBodyElement(document).appendChild(maqroXHTMLifier.createElement(document, outcome));
				final String outcomeXHTMLString=XMLUtilities.toString(document.getDocumentElement());	//get a string version of the entire XHTML document
*/
				/*TODO fix
				final org.w3c.dom.Document document=XHTMLUtilities.createXHTMLDocument();	//create an XHTML document
				final org.w3c.dom.Element element=new MAQROXHTMLifier().createElement(document, outcome);	//create an XHTML element from the outcome
				final String outcomeXHTMLString="<html>"+XMLUtilities.toString(element)+"</html>";	//get a string version of the entire XHTML document, using HTML as Swing doesn't understand XHTML
System.out.println(outcomeXHTMLString);
			  BasicOptionPane.showMessageDialog(component, outcomeXHTMLString, "Assessment Results", BasicOptionPane.INFORMATION_MESSAGE);	//G***i18n; comment
*/
			  BasicOptionPane.showMessageDialog(component, new JScrollPane(outcomeTextPane), "Assessment Results", BasicOptionPane.INFORMATION_MESSAGE);	//G***i18n; comment
			}
			finally
			{
				activityEngine.stop();	//always stop the engine
			}
		}
	}

	/**Action for submitting an activity.
	@author Garret Wilson
	*/
	protected class SubmitAction extends AbstractAction
	{

		/**The activity to be submitted.*/
		protected final Activity activity;

		/**Constructs an activity submit action.
		@param activity The activity to be submitted
		*/
		public SubmitAction(final Activity activity)
		{
			super("Submit");	//create the base class G***i18n
			putValue(SHORT_DESCRIPTION, "Submit the activity.");	//set the short description G***i18n
			putValue(LONG_DESCRIPTION, "Submit the activity.");	//set the long description G***i18n
			putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_S));  //set the mnemonic key G***i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.SUBMIT_ICON_FILENAME)); //load the correct icon
			this.activity=activity;  //store the activity
		}

		/**Called when the action should be performed.
		@param actionEvent The event causing the action.
		*/
		public void actionPerformed(final ActionEvent actionEvent)
		{
			submit(activity);	//submit the activity
		}
	}

}