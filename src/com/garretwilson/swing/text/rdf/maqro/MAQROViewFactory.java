package com.garretwilson.swing.text.rdf.maqro;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.*;

import javax.swing.*;
import javax.swing.text.*;

import static com.garretwilson.lang.ObjectUtilities.*;

import com.garretwilson.rdf.RDFResource;
import static com.garretwilson.rdf.maqro.MAQROConstants.*;
import com.garretwilson.rdf.maqro.*;
import com.garretwilson.resources.icon.IconResources;

import static com.garretwilson.swing.text.rdf.RDFStyleUtilities.*;

import com.garretwilson.swing.text.ViewComponentManager;
import com.garretwilson.swing.text.xml.*;
import com.garretwilson.util.Debug;

import static com.garretwilson.swing.text.xml.XMLStyleUtilities.*;

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
					final XMLComponentBlockView activityView=new XMLComponentBlockView(element, View.Y_AXIS);  //create a new view for the activity
					final JButton submitButton=new JButton(submitAction); //create a new button for the submission
					submitButton.setFocusable(false);	//TODO fix component manager focus traversal
					activityView.getComponentManager().add(submitButton, ViewComponentManager.Border.PAGE_END); //add the button to the bottom of the activity view
					view=activityView;	//use the activity view we just created
				}
			}
		  else if(QUESTION_CLASS_NAME.equals(elementLocalName)) //maqro:Question
			{
/*TODO fix
				if(getQTIObject(element)==null) //if no QTI item has been created yet for the given item element
				{
Debug.trace("Creating item object without an assessment."); //G***del
					createItem(element);  //create an item from the element, which will automatically add it to the map of created items G***do we want this routine to automatically add the item?
				}
Debug.trace("Creating new item view");
				final QTIItemView itemView=new QTIItemView(element, QTIItemView.Y_AXIS);  //construct an item view
				putQTIView(element, itemView);  //store the view keyed indirectly to the element by the QTI object
				return itemView;  //return the item view we created
*/
				view=new MAQROQuestionView(element, View.Y_AXIS);	//create a question view				
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
							return new MAQRODialogueChoiceView(element, questionView);	//create a dialogue choice view
						}
					}
				}
				view=new XMLParagraphView(element);	//for every other dialogue instances, create a paragraph view				
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
		}
	}

}