package com.garretwilson.swing.text.xml.qti;

import java.awt.BorderLayout; //G***maybe move elsewhere
import java.awt.Color; //G***del when component color update is moved
import java.awt.event.*;
import java.text.MessageFormat;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
//G***del import com.garretwilson.net.URIConstants;
import com.garretwilson.swing.text.*;
import com.garretwilson.swing.text.xml.*;
import com.garretwilson.swing.text.xml.css.*; //G***del when component color update is moved
import com.garretwilson.resources.icon.IconResources;
import com.garretwilson.assess.qti.*;
import com.garretwilson.util.Debug;

/**Creates a view for a Question and Test Iterpoperability element.
@author Garret Wilson
*/
public class QTIViewFactory extends XMLViewFactory implements QTIConstants
{

	/**The map of QTI assessments.*/
//G***del if not needed	private final Map assessmentMap=new HashMap();

		/**Retrieves an assessment based on its ID.
		@param assessmentIdent The ID of the assessment to retrieve.
		@return The assessment with the given identification, or <code>null</code>
			if no assessment with that identification exists.
		*/
/*G***del if not needed
		public Assessment getAssessment(final String assessmentIdent)
		{
			return (Assessment)assessmentMap.get(assessmentIdent);
		}
*/

		/**Stores an assessment, keyed by its ident.
		@param assessment The assessment to store.
		@see Assessment#getIdent
		*/
/*G***del if not needed
		public void putAssessment(final Assessment assessment)
		{
		  assessmentMap.put(assessment.getIdent(), assessment);
		}
*/

	/**Clears all context information, including responses and references to
		created views.
	*/
	public void clearContext()
	{
//G***del Debug.trace("Clearing context");  //G***del
//G***del		getResponseMap().clear(); //clear the response map
		itemResponseMapMap.clear(); //clear the map of response maps
		elementQTIMap.clear();  //clear the map of QTI objects
		qtiViewMap.clear(); //clear the map of views
	}

	/**A map of maps keyed to item idents. Each map contains response objects
		keyed to response idents.
	*/
	private final Map itemResponseMapMap=new HashMap();

		/**Retrieves a response map for an item with the given ident. If no map
		  exists for the given item, one will be created, stored, and returned.
		@param itemIdent The identification of the item.
		@return A live response map for the specified item.
		*/
		public Map getItemResponseMap(final String itemIdent)
		{
			Map responseMap=(Map)itemResponseMapMap.get(itemIdent);  //get the response map for this item
			if(responseMap==null) //if there is no response map for this item
			{
				responseMap=new HashMap();  //create a new response map for this item
				itemResponseMapMap.put(itemIdent, responseMap); //store this response map, keyed to the item ident
			}
			return responseMap; //return the response map
		}


		/**Sets the response map for a given item.
		@param itemIdent The identification of the item.
		@param responseMap The live response map that will be set for the item.
		*/
		public void setItemResponseMap(final String itemIdent, final Map responseMap)
		{
			itemResponseMapMap.put(itemIdent, responseMap); //store the response map in the map map
		}

	/**A map of response objects keyed to response idents.*/
//G***del when works	private final Map responseMap=new HashMap();

		/**Retrieves a map of response objects keyed to response idents.
		  The keys are the in the form <em>itemIdent</em>#<em>responseIdent</em>.
		  The objects stored vary depending on the response type. A multiple choice
			response label, for instance, will hold a <code>Boolean</code> value
			indicating the state of the choice.
		@return A map of response objects keyed to response idents.
		*/
//G***del when works		public Map getResponseMap() {return responseMap;}

	/**The map of QTI objects, keyed by the elements they represent.*/
	private final Map elementQTIMap=new HashMap();

		/**Stores a QTI object in the map, keyed to the element.
		@param element The element represented by the QTI object.
		@param object The QTI object.
		*/
		public void putQTIObject(final Element element, final Object object)
		{
		  elementQTIMap.put(element, object); //store the QTI object in the map, keyed to the element
		}

		/**Retrieves a QTI object representing an element from the map.
		@param element The element represented by the QTI object.
		@return The QTI object that represents the element, or <code>null</code> if
			no QTI object represents the given element.
		*/
		public Object getQTIObject(final Element element)
		{
			return elementQTIMap.get(element); //lookup the QTI object using the element as a key
		}

	/**The map of views, keyed by QTI objects.*/
	private final Map qtiViewMap=new HashMap();

		/**Stores a view in the map, keyed to the QTI object.
		@param object The QTI object.
		@param view The view that represents the QTI object.
		*/
		public void putQTIView(final Object object, final View view)
		{
		  qtiViewMap.put(object, view); //store the view in the map, keyed to the QTI object
		}

		/**Retrieves a view representing a QTI object from the map.
		@param object The QTI object.
		@return The view that represents the QTI object, or <code>null</code> if
			no view represents the given QTI object.
		*/
		public View getQTIView(final Object object)
		{
			return (View)qtiViewMap.get(object); //lookup the view using the QTI object as a key
		}

	/**The map of views, keyed by elements.*/
//G***del	private final Map qtiElementViewMap=new HashMap();

		/**Stores a view keyed to an element
		@param element The element this view represents.
		@param view The view to store, keyed to the appropriate element.
		*/
		public void putQTIView(final Element element, final View view)
		{
		  qtiViewMap.put(element, view); //store the view in the map, keyed to the element
		}

		/**Retrieves a view keyed to QTI element.
		@param element The element this view represents.
		@return view The view keyed to the appropriate element, or <code>null</code>
			if no such view exists.
		*/
		public View getQTIView(final Element element)
		{
			return (View)qtiViewMap.get(element); //lookup the view using the element as a key
		}

	/**The map of views, keyed to idents.*/
//G***del	private final Map qtiViewMap=new HashMap();

		/**Stores a view in the map, keyed to the ident.
		@param ident The QTI object identification string.
		@param view The view that represents the QTI object.
		*/
/*G***del if not needed
		public void putQTIView(final String ident, final View view)
		{
		  qtiViewMap.put(ident, view); //store the view in the map, keyed to the ident
		}
*/

		/**Retrieves a view representing a QTI object from the map.
		@param ident The QTI object identification string.
		@return The view that represents the QTI with the given ident, or
			<code>null</code> if there is no view for the given ident.
		*/
/*G***del if not needed
		public View getQTIView(final String ident)
		{
			return (View)qtiViewMap.get(ident); //lookup the view using the QTI string as a key
		}
*/

		/**Stores a view keyed to a QTI object after first looking up the object
		  from the element QTI object map. The view will therefore be indirectly
			keyed to the element.
			There must have first been a QTI object stored keyed to the given element.
		@param element The element this view represents.
		@param view The view to store, keyed to the appropriate QTI object.
		@see #getQTIObject
		*/
/*G***del when works
		public void putQTIView(final Element element, final View view)
		{
			final Object qtiObject=getQTIObject(element); //get the QTI object already stored, keyed to the element
		  Debug.assert(qtiObject!=null, "QTI object not stored for element "+XMLStyleConstants.getXMLElementLocalName(element.getAttributes()));
		  putQTIView(qtiObject, view);  //store the view keyed to the QTI object
		}
*/

		/**Retrieves a view keyed to a QTI object after first looking up the object
		  from the element QTI object map. The view will therefore be indirectly
			keyed to the element.
			There must have first been a QTI object stored keyed to the given element.
		@param element The element this view represents.
		@return view The view keyed to the appropriate QTI object, keyed to the
			element.
		@see #getQTIObject
		*/
/*G***del when works
		public View getQTIView(final Element element)
		{
			final Object qtiObject=getQTIObject(element); //get the QTI object already stored, keyed to the element
//G***del			Debug.assert(qtiObject!=null, "QTI object not stored for element.");
			return qtiObject!=null ? (View)getQTIView(qtiObject) : null;  //retrieve the view keyed to the QTI object, if a QTI object as indeed found
		}
*/

	/**The last assessment view created.*/
//G***del	protected QTIAssessmentView assessmentView=null;

	/**The last logical ID response view created.*/
//G***del	protected QTIResponseLIDView responseLIDView=null;

	/**The last item view created.*/
//G***del	protected QTIItemView itemView=null;

//G***del	protected ButtonGroup buttonGroup=new ButtonGroup();  //G***fix; testing

	/**Creates a QTI view factory with a fallback view factory.*/
	public QTIViewFactory()
	{
		super(); //create the parent class
	}
/*G***fix
	public QTIViewFactory(final ViewFactory fallbackViewFactory)
	{
		super(fallbackViewFactory); //create the parent class
	}
*/

	/**Creates a view from the given QTI element.
	@param element The QTI element for which a view should be created.
	@return The view to represent the element.
	*/

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
		final AttributeSet attributeSet=element.getAttributes();  //get the element's attribute set
		if(attributeSet!=null)  //if we have an attribute set
		{
			//G***we should probably make sure this element is in our namespace
			final String elementLocalName=XMLStyleConstants.getXMLElementLocalName(attributeSet); //get the local name of this element
Debug.trace("QTI local name: ", elementLocalName);  //G***del when works
		  if(ELEMENT_ASSESSMENT.equals(elementLocalName)) //if this is an assessment
			{
				final Assessment assessment=createAssessment(element);  //create an assessment from this element hierarcy
				final SubmitAction submitAction=new SubmitAction(assessment); //create a new submission action for this assessment
//G***del if not needed				putAssessment(assessment);  //put the assessment in our map
				final XMLComponentBlockView assessmentView=new XMLComponentBlockView(element, QTIAssessmentView.Y_AXIS);  //create a new view for the assessment
//G***del if not needed				final QTIAssessmentView assessmentView=new QTIAssessmentView(element, QTIAssessmentView.Y_AXIS, submitAction);  //create a new view for the assessment
				final JButton submitButton=new JButton(submitAction); //create a new button for the submission
				assessmentView.add(submitButton, BorderLayout.AFTER_LAST_LINE); //add the button to the bottom of the assessment view
				return assessmentView;  //return the assessment view
			}
		  else if(ELEMENT_ITEM.equals(elementLocalName)) //if this is an <item>
			{
				if(getQTIObject(element)==null) //if no QTI item has been created yet for the given item element
				{
Debug.trace("Creating item object without an assessment."); //G***del
					createItem(element);  //create an item from the element, which will automatically add it to the map of created items G***do we wannt this routine to automatically add the item?
				}
Debug.trace("Creating new item view");
				final QTIItemView itemView=new QTIItemView(element, QTIItemView.Y_AXIS);  //construct an item view
/*G***del when works
				if(assessmentView!=null) //if we have an assessment view
					assessmentView.getItemViewList().add(itemView); //add this item view to the assessment view's list of item views
*/
				putQTIView(element, itemView);  //store the view keyed indirectly to the element by the QTI object
				return itemView;  //return the item view we created
			}
		  else if(ELEMENT_MATIMAGE.equals(elementLocalName)) //if this is a <matimage>
			{
/*G***fix or del
				if(getQTIObject(element)==null) //if no QTI item has been created yet for the given item element
				{
Debug.trace("Creating item object without an assessment."); //G***del
					createItem(element);  //create an item from the element, which will automatically add it to the map of created items G***do we wannt this routine to automatically add the item?
				}
Debug.trace("Creating new item view");
*/
				final QTIMaterialImageView materialImageView=new QTIMaterialImageView(element); //construct an image view
				putQTIView(element, materialImageView);  //store the view keyed indirectly to the element by the QTI object
				return materialImageView;  //return the material image view we created
			}
/*G***del when works
		  else if(ELEMENT_RESPONSE_LID.equals(elementLocalName)) //if this is a <response_lid>
			{
				if(getQTIObject(element)==null) //if no QTI item has been created yet for the given item element
				{
Debug.trace("Creating response LID object by itself."); //G***del
					createResponseLID(element);  //create a response LID from the element, which will automatically add it to the map of created items G***do we want this routine to automatically add the item?
				}
Debug.trace("building response LID view");
				final QTIResponseLIDView responseLIDView=new QTIResponseLIDView(element, QTIResponseLIDView.Y_AXIS);  //create a view for the logical ID responses
				putQTIView(element, responseLIDView);  //store the view keyed indirectly to the element by the QTI object
				return responseLIDView; //return the logical ID response view we created
			}
*/
		  else if(ELEMENT_RENDER_CHOICE.equals(elementLocalName)) //if this is a <render_choice>
			{
/*G***fix
				if(getQTIObject(element)==null) //if no QTI item has been created yet for the given item element
				{
Debug.trace("Creating response LID object by itself."); //G***del
					createResponseLID(element);  //create a response LID from the element, which will automatically add it to the map of created items G***do we want this routine to automatically add the item?
				}
*/
Debug.trace("building render choice view");
				final QTIRenderChoiceView renderChoiceView=new QTIRenderChoiceView(element, QTIResponseLIDView.Y_AXIS);  //create a view for the choice rendering
				putQTIView(element, renderChoiceView);  //store the view keyed indirectly to the element by the QTI object
				return renderChoiceView; //return the choice rendering view we created
			}
		  else if(ELEMENT_RENDER_HOTSPOT.equals(elementLocalName)) //if this is a <render_hotspot>
			{
/*G***fix
				if(getQTIObject(element)==null) //if no QTI item has been created yet for the given item element
				{
Debug.trace("Creating response LID object by itself."); //G***del
					createResponseLID(element);  //create a response LID from the element, which will automatically add it to the map of created items G***do we want this routine to automatically add the item?
				}
*/
Debug.trace("building render hotspot view");
				QTIMaterialImageView materialImageView=null;  //try to find the image with which this hotspot is paired
				final Element presentationElement=XMLStyleConstants.getAncestorElement(element, ELEMENT_PRESENTATION);  //get the presentation element in which this element resides
				if(presentationElement!=null) //if there is an enclosing presentation element
				{
					final Element materialElement=XMLStyleConstants.getChildElement(presentationElement, ELEMENT_MATERIAL); //get the child material element, if there is one
					if(materialElement!=null) //if there is a material element in the presentation
					{
						final Element materialImageElement=XMLStyleConstants.getChildElement(materialElement, ELEMENT_MATIMAGE); //get the child material image element, if there is one
						if(materialImageElement!=null) //if there is a material element in the presentation
						{
							materialImageView=(QTIMaterialImageView)getQTIView(materialImageElement);  //get the view created for the element
						}
					}
				}
				final QTIRenderHotspotView renderHotspotView=new QTIRenderHotspotView(element, materialImageView);  //create a view for the hotspot rendering, passing the view created for the image with which the hotspots are associated G***this might get changed when the hotspots can go with different images
				putQTIView(element, renderHotspotView);  //store the view keyed indirectly to the element by the QTI object
				return renderHotspotView; //return the hotspot rendering view we created
			}
		  else if(ELEMENT_RESPONSE_LABEL.equals(elementLocalName)) //if this is a <response_label>
			{
Debug.trace("building response label view");
				  //build a different type of response label view, based upon the rendering type
				final Element parentElement=element.getParentElement(); //get the parent element
				final AttributeSet parentAttributeSet=parentElement.getAttributes();  //get the parent element's attribute set
				if(attributeSet!=null)  //if we have an attribute set
				{
					//G***we should probably make sure this element is in our namespace
					final String parentElementLocalName=XMLStyleConstants.getXMLElementLocalName(parentAttributeSet); //get the local name of the parent element
				  if(ELEMENT_RENDER_CHOICE.equals(parentElementLocalName)) //if the parent is a <render_choice>
					{
						final QTIRenderChoiceView renderChoiceView=(QTIRenderChoiceView)getQTIView(parentElement);  //get the choice rendering view already created, if one is available
/*G***del when works
				final Element responseLIDElement=getAncestorElement(element, ELEMENT_RESPONSE_LID);  //get the ancestor element corresponding to the <response_lid> this label belongs to
				final QTIResponseLIDView responseLIDView=(QTIResponseLIDView)getQTIView(responseLIDElement);  //get the view created for the logical ID response
				if(responseLIDView!=null) //if we found a logical ID response view
				{
*/
						final QTIChoiceResponseLabelView responseLabelView=new QTIChoiceResponseLabelView(element, renderChoiceView);  //construct a response label, passing the enclosing choice rendering view
						final String ident=((String)XMLStyleConstants.getDefinedAttribute(attributeSet, ATTRIBUTE_IDENT));  //get the ident
						if(ident!=null && ident.length()>0) //if there is an ident
						{
Debug.trace("found ident to display QTI response: ", ident);  //G***del
/*G***del when works
							putQTIView(ident, responseLabelView); //store the response label keyed to its ident
							final String id=createID(element);  //create an ID from this element
							if(id!=null)  //if we have an ID
							{
*/
							final Element itemElement=XMLStyleConstants.getAncestorElement(element, ELEMENT_ITEM);  //get the item element in which this element resides
							if(itemElement!=null) //if there is an enclosing item element
							{
									//get the item's ident
								final String itemIdent=((String)XMLStyleConstants.getDefinedAttribute(itemElement.getAttributes(), ATTRIBUTE_IDENT));
								if(itemIdent!=null && itemIdent.length()>0) //if there is an item ident
								{
	Debug.trace("found item ident to display QTI response: ", itemIdent);  //G***del

									final Object booleanObject=getItemResponseMap(itemIdent).get(ident); //see if there is already a value for this choice
									if(booleanObject instanceof Boolean)  //if this object already has a value
									{
											//set the toggle button to reflect the stored value
										responseLabelView.getToggleButton().setSelected(((Boolean)booleanObject).booleanValue());
									}
	//G***del Debug.trace("created ID: ", id);  //G***del
										//listen for the response being checked or unchecked
									responseLabelView.getToggleButton().addItemListener(new ItemListener()
											{
												public void itemStateChanged(final ItemEvent itemEvent)
												{
	//G***del Debug.trace("storing value in response map for ID: ", id);  //G***del
	//G***del Debug.trace("storing in response map: ", getResponseMap()); //G***del
														//update the response with a boolean indication of the selection state
													getItemResponseMap(itemIdent).put(ident, new Boolean(responseLabelView.getToggleButton().isSelected()));
	//G***del Debug.trace("reponse map now has size: ", getResponseMap().size());  //G***del

		//G***del Debug.notify("New state of "+ident+": "+responseLabelView.getToggleButton().isSelected());  //G***testing
	//G***del	System.out.println("New state of "+ident+": "+responseLabelView.getToggleButton().isSelected());  //G***testing

												}


											});
								}
							}
						}
						putQTIView(element, responseLabelView);  //store the view keyed indirectly to the element by the QTI object
						return responseLabelView; //return the response label view we created
					}
				}
/*G***del when works
			  final QTIResponseLID responseLID=(QTIResponseLID)getQTIObject(responseLIDElement); //get the QTI logical ID response object already stored
				if(responseLID!=null) //if we found a logical ID response
				{
					return new QTIResponseLabelView(element, responseLID);  //construct a response label, passing the enclosing logical ID response view
				}
*/
			}
		}
		return super.create(element, indicateMultipleViews);	//if we couldn't figure out which kind of view to create, let the parent class decide what to do
//G***del		return super.create(element); //if we couldn't create the element, allow the super class to create the element
	}

	/**Creates and returns an assessment object to represent the given assessment
		element.
	@param element The element representing the assessment.
	*/
	protected Assessment createAssessment(final Element element)
	{
Debug.trace();
		final Assessment assessment=new Assessment(); //create a new assessment
		final AttributeSet attributeSet=element.getAttributes();  //get the element's attributes
		if(attributeSet!=null)  //if the element has attributes
		{
		  assessment.setIdent((String)XMLStyleConstants.getDefinedAttribute(attributeSet, ATTRIBUTE_IDENT));  //set the ident
		  assessment.setTitle((String)XMLStyleConstants.getDefinedAttribute(attributeSet, ATTRIBUTE_TITLE));  //set the title
		}
		for(int i=0; i<element.getElementCount(); ++i)  //look at each of the child elements
		{
			final Element childElement=element.getElement(i); //get a reference to this element
			final AttributeSet childAttributeSet=childElement.getAttributes();  //get the child's attributes
			if(childAttributeSet!=null) //if this child has attributes
			{
				//G***we should probably make sure this element is in our namespace
				final String childElementLocalName=XMLStyleConstants.getXMLElementLocalName(childAttributeSet); //get the local name of this child element
				if(ELEMENT_SECTION.equals(childElementLocalName)) //if this is a section
				{
					final Section section=createSection(childElement);  //create a section
					assessment.getSectionList().add(section); //add this section to our assessment G***make this better with a hash map or something
				}
			}
		}
		putQTIObject(element, assessment);  //store the QTI object in the map, keyed to the element
		return assessment;  //return the assessment we created
	}

	/**Creates and returns a section object to represent the given section element.
	@param element The element representing the section.
	*/
	protected Section createSection(final Element element)
	{
Debug.trace();
		final Section section=new Section(); //create a new section
		final AttributeSet attributeSet=element.getAttributes();  //get the element's attributes
		if(attributeSet!=null)  //if the element has attributes
		{
		  section.setIdent((String)XMLStyleConstants.getDefinedAttribute(attributeSet, ATTRIBUTE_IDENT));  //set the ident
		  section.setTitle((String)XMLStyleConstants.getDefinedAttribute(attributeSet, ATTRIBUTE_TITLE));  //set the title
		}
		for(int i=0; i<element.getElementCount(); ++i)  //look at each of the child elements
		{
			final Element childElement=element.getElement(i); //get a reference to this element
			final AttributeSet childAttributeSet=childElement.getAttributes();  //get the child's attributes
			if(childAttributeSet!=null) //if this child has attributes
			{
				//G***we should probably make sure this element is in our namespace
				final String childElementLocalName=XMLStyleConstants.getXMLElementLocalName(childAttributeSet); //get the local name of this child element
				if(ELEMENT_ITEM.equals(childElementLocalName)) //if this is an item
				{
					final Item item=createItem(childElement);  //create an item
					section.getItemList().add(item); //add this item to our section G***make this better with a hash map or something
				}
			}
		}
		putQTIObject(element, section);  //store the QTI object in the map, keyed to the element
		return section;  //return the section we created
	}

	/**Creates and returns an item object to represent the given item element.
	@param element The element representing the item.
	*/
	protected Item createItem(final Element element)
	{
Debug.trace();
		final Item item=new Item(); //create a new item
		final AttributeSet attributeSet=element.getAttributes();  //get the element's attributes
		if(attributeSet!=null)  //if the element has attributes
		{
		  item.setIdent((String)XMLStyleConstants.getDefinedAttribute(attributeSet, ATTRIBUTE_IDENT));  //set the ident
		  item.setTitle((String)XMLStyleConstants.getDefinedAttribute(attributeSet, ATTRIBUTE_TITLE));  //set the title
		}
		for(int i=0; i<element.getElementCount(); ++i)  //look at each of the child elements
		{
			final Element childElement=element.getElement(i); //get a reference to this element
			final AttributeSet childAttributeSet=childElement.getAttributes();  //get the child's attributes
			if(childAttributeSet!=null) //if this child has attributes
			{
				//G***we should probably make sure this element is in our namespace
				final String childElementLocalName=XMLStyleConstants.getXMLElementLocalName(childAttributeSet); //get the local name of this child element
				if(ELEMENT_PRESENTATION.equals(childElementLocalName)) //if this is presentation
				{
					final Presentation presentation=createPresentation(childElement);  //create presentation
				  item.setPresentation(presentation); //set the item presentation
				}
				else if(ELEMENT_RESPROCESSING.equals(childElementLocalName)) //if this is response processing
				{
					final ResponseProcessing responseProcessing=createResponseProcessing(childElement);  //create response processing
				  item.setResponseProcessing(responseProcessing); //set the item response processing
				}
			}
		}
		putQTIObject(element, item);  //store the QTI object in the map, keyed to the element
		return item;  //return the item we created
	}

	/**Creates and returns a presentation object to represent the given element.
	@param element The element representing the presentation.
	*/
	protected Presentation createPresentation(final Element element)
	{
Debug.trace();
		final Presentation presentation=new Presentation(); //create new presentation
		final AttributeSet attributeSet=element.getAttributes();  //get the element's attributes
/*G***fix for label
		if(attributeSet!=null)  //if the element has attributes
		{
		  item.setIdent((String)XMLStyleConstants.getDefinedAttribute(attributeSet, ATTRIBUTE_IDENT));  //set the ident
		  item.setTitle((String)XMLStyleConstants.getDefinedAttribute(attributeSet, ATTRIBUTE_TITLE));  //set the title
		}
*/
		for(int i=0; i<element.getElementCount(); ++i)  //look at each of the child elements
		{
			final Element childElement=element.getElement(i); //get a reference to this element
			final AttributeSet childAttributeSet=childElement.getAttributes();  //get the child's attributes
			if(childAttributeSet!=null) //if this child has attributes
			{
				//G***we should probably make sure this element is in our namespace
				final String childElementLocalName=XMLStyleConstants.getXMLElementLocalName(childAttributeSet); //get the local name of this child element
				if(ELEMENT_RESPONSE_LID.equals(childElementLocalName)) //if this is a logical ID response
				{
					final ResponseLID responseLID=createResponseLID(childElement);  //create a logical ID response
				  presentation.getResponseList().add(responseLID);  //add this response to the presentation
				}
			}
		}
		putQTIObject(element, presentation);  //store the QTI object in the map, keyed to the element
		return presentation;  //return the presentation we created
	}

	/**Creates and returns a logical ID response object to represent the given element.
	@param element The element representing the logical ID response.
	*/
	protected ResponseLID createResponseLID(final Element element)
	{
Debug.trace();
		final ResponseLID responseLID=new ResponseLID(); //create a new logical ID response
		final AttributeSet attributeSet=element.getAttributes();  //get the element's attributes
		if(attributeSet!=null)  //if the element has attributes
		{
		  responseLID.setIdent((String)XMLStyleConstants.getDefinedAttribute(attributeSet, ATTRIBUTE_IDENT));  //set the ident
//G***fix for cardinality and timing		  item.setTitle((String)XMLStyleConstants.getDefinedAttribute(attributeSet, ATTRIBUTE_TITLE));  //set the title
		}
		for(int i=0; i<element.getElementCount(); ++i)  //look at each of the child elements
		{
			final Element childElement=element.getElement(i); //get a reference to this element
			final AttributeSet childAttributeSet=childElement.getAttributes();  //get the child's attributes
			if(childAttributeSet!=null) //if this child has attributes
			{
				//G***we should probably make sure this element is in our namespace
				final String childElementLocalName=XMLStyleConstants.getXMLElementLocalName(childAttributeSet); //get the local name of this child element
				if(ELEMENT_RENDER_CHOICE.equals(childElementLocalName)) //if this is render choice
				{
					final RenderChoice renderChoice=createRenderChoice(childElement);  //create a render choice
					responseLID.getRenderList().add(renderChoice);  //add this render to the logical ID response
				}
			}
		}
		putQTIObject(element, responseLID);  //store the QTI object in the map, keyed to the element
		return responseLID;  //return the presentation we created
	}

	/**Creates and returns a choice render object to represent the given element.
	@param element The element representing the render choice.
	*/
	protected RenderChoice createRenderChoice(final Element element)
	{
Debug.trace();
		final RenderChoice renderChoice=new RenderChoice(); //create a new render choice
/*G***del if not needed
		final AttributeSet attributeSet=element.getAttributes();  //get the element's attributes
		if(attributeSet!=null)  //if the element has attributes
		{
		  responseLID.setIdent((String)XMLStyleConstants.getDefinedAttribute(attributeSet, ATTRIBUTE_IDENT));  //set the ident
//G***fix for cardinality and timing		  item.setTitle((String)XMLStyleConstants.getDefinedAttribute(attributeSet, ATTRIBUTE_TITLE));  //set the title
		}
*/
		for(int i=0; i<element.getElementCount(); ++i)  //look at each of the child elements
		{
			final Element childElement=element.getElement(i); //get a reference to this element
			final AttributeSet childAttributeSet=childElement.getAttributes();  //get the child's attributes
			if(childAttributeSet!=null) //if this child has attributes
			{
				//G***we should probably make sure this element is in our namespace
				final String childElementLocalName=XMLStyleConstants.getXMLElementLocalName(childAttributeSet); //get the local name of this child element
				if(ELEMENT_RESPONSE_LABEL.equals(childElementLocalName)) //if this is a response label
				{
					final ResponseLabel responseLabel=createResponseLabel(childElement);  //create a response label
					renderChoice.getResponseLabelList().add(responseLabel); //add this response label to the render choice
				}
			}
		}
		putQTIObject(element, renderChoice);  //store the QTI object in the map, keyed to the element
		return renderChoice;  //return the choice render we created
	}

	/**Creates and returns a response label to represent the given element.
	@param element The element representing the response label.
	*/
	protected ResponseLabel createResponseLabel(final Element element)
	{
Debug.trace();
		final ResponseLabel responseLabel=new ResponseLabel(); //create a new response label
		final AttributeSet attributeSet=element.getAttributes();  //get the element's attributes
		if(attributeSet!=null)  //if the element has attributes
		{
		  responseLabel.setIdent((String)XMLStyleConstants.getDefinedAttribute(attributeSet, ATTRIBUTE_IDENT));  //set the ident
//G***fix for rshuffle, etc.		  item.setTitle((String)XMLStyleConstants.getDefinedAttribute(attributeSet, ATTRIBUTE_TITLE));  //set the title
		}
/*G***fix for ResponseLabel sub-elements
		for(int i=0; i<element.getElementCount(); ++i)  //look at each of the child elements
		{
			final Element childElement=element.getElement(i); //get a reference to this element
			final AttributeSet childAttributeSet=childElement.getAttributes();  //get the child's attributes
			if(childAttributeSet!=null) //if this child has attributes
			{
				//G***we should probably make sure this element is in our namespace
				final String childElementLocalName=XMLStyleConstants.getXMLElementLocalName(childAttributeSet); //get the local name of this child element
				if(ELEMENT_RESPONSE_LABEL.equals(childElementLocalName)) //if this is a response label
				{
					final ResponseLabel responseLabel=createResponseLabel(childElement);  //create a response label
					renderChoice.getResponseLabelList().add(responseLabel); //add this response label to the render choice
				}
			}
		}
*/
		putQTIObject(element, responseLabel);  //store the QTI object in the map, keyed to the element
		return responseLabel;  //return the response label we created
	}

	/**Creates and returns a response processing object to represent the given element.
	@param element The element representing the response processing.
	*/
	protected ResponseProcessing createResponseProcessing(final Element element)
	{
Debug.trace("Found resprocessing");  //G***del
		final ResponseProcessing responseProcessing=new ResponseProcessing(); //create a new response processing object
		final AttributeSet attributeSet=element.getAttributes();  //get the element's attributes
		for(int i=0; i<element.getElementCount(); ++i)  //look at each of the child elements
		{
			final Element childElement=element.getElement(i); //get a reference to this element
			final AttributeSet childAttributeSet=childElement.getAttributes();  //get the child's attributes
			if(childAttributeSet!=null) //if this child has attributes
			{
				//G***we should probably make sure this element is in our namespace
				final String childElementLocalName=XMLStyleConstants.getXMLElementLocalName(childAttributeSet); //get the local name of this child element
				if(ELEMENT_OUTCOMES.equals(childElementLocalName)) //if this is outcomes
				{
					final Outcomes outcomes=createOutcomes(childElement);  //create outcomes
				  responseProcessing.setOutcomes(outcomes); //set the outcomes
				}
				else if(ELEMENT_RESPCONDITION.equals(childElementLocalName)) //if this is a response condition
				{
					final ResponseCondition responseCondition=createResponseCondition(childElement);  //create a response condition
				  responseProcessing.getResponseConditionList().add(responseCondition);  //add this response condition to the response processing
				}
			}
		}
		putQTIObject(element, responseProcessing);  //store the QTI object in the map, keyed to the element
		return responseProcessing;  //return the item we created
	}

	/**Creates and returns an outcomes object to represent the given element.
	@param element The element representing the outcomes.
	*/
	protected Outcomes createOutcomes(final Element element)
	{
Debug.trace("Found outcomes");  //G***del
		final Outcomes outcomes=new Outcomes(); //create a new outcomes object
		final AttributeSet attributeSet=element.getAttributes();  //get the element's attributes
		for(int i=0; i<element.getElementCount(); ++i)  //look at each of the child elements
		{
			final Element childElement=element.getElement(i); //get a reference to this element
			final AttributeSet childAttributeSet=childElement.getAttributes();  //get the child's attributes
			if(childAttributeSet!=null) //if this child has attributes
			{
				//G***we should probably make sure this element is in our namespace
				final String childElementLocalName=XMLStyleConstants.getXMLElementLocalName(childAttributeSet); //get the local name of this child element
				if(ELEMENT_DECVAR.equals(childElementLocalName)) //if this is a variable declaration
				{
					final DecVar decVar=new DecVar(); //G***fix; for now, create a default score variable
//G***fix					final DecVar decVar=createDecVar(childElement);  //create a variable declaration
				  outcomes.getDecVarList().add(decVar); //add this variable declaration
				}
			}
		}
		putQTIObject(element, outcomes);  //store the QTI object in the map, keyed to the element
		return outcomes;  //return the object we created
	}

	/**Creates and returns a response condition object to represent the given element.
	@param element The element representing the response condition.
	*/
	protected ResponseCondition createResponseCondition(final Element element)
	{
Debug.trace("Found respcondition");  //G***del
		final ResponseCondition responseCondition=new ResponseCondition(); //create a new response processing object
		final AttributeSet attributeSet=element.getAttributes();  //get the element's attributes
		if(attributeSet!=null)  //if the element has attributes
		{
		  responseCondition.setTitle((String)XMLStyleConstants.getDefinedAttribute(attributeSet, ATTRIBUTE_TITLE));  //set the title
		}
		for(int i=0; i<element.getElementCount(); ++i)  //look at each of the child elements
		{
			final Element childElement=element.getElement(i); //get a reference to this element
			final AttributeSet childAttributeSet=childElement.getAttributes();  //get the child's attributes
			if(childAttributeSet!=null) //if this child has attributes
			{
				//G***we should probably make sure this element is in our namespace
				final String childElementLocalName=XMLStyleConstants.getXMLElementLocalName(childAttributeSet); //get the local name of this child element
				if(ELEMENT_CONDITIONVAR.equals(childElementLocalName)) //if this is a condition variable group
				{
					final ConditionVar conditionVar=createConditionVar(childElement);  //create the condition variable group
				  responseCondition.getConditionVarList().add(conditionVar); //add the condition variable group
				}
				else if(ELEMENT_SETVAR.equals(childElementLocalName)) //if this is a variable setting
				{
					final SetVar setVar=createSetVar(childElement);  //create a variable setting
				  responseCondition.getSetVarList().add(setVar);  //add this variable setting to the list of variables to set
				}
			}
		}
		putQTIObject(element, responseCondition);  //store the QTI object in the map, keyed to the element
		return responseCondition;  //return the object we created
	}

	/**Creates and returns a condition variable group to represent the given element.
	@param element The element representing the condition variable group.
	*/
	protected ConditionVar createConditionVar(final Element element)
	{
Debug.trace("Found conditionvar");  //G***del
		final ConditionVar conditionVar=new ConditionVar(); //create a new condition variable group
		final AttributeSet attributeSet=element.getAttributes();  //get the element's attributes
		for(int i=0; i<element.getElementCount(); ++i)  //look at each of the child elements
		{
			final Element childElement=element.getElement(i); //get a reference to this element
			final AttributeSet childAttributeSet=childElement.getAttributes();  //get the child's attributes
			if(childAttributeSet!=null) //if this child has attributes
			{
				//G***we should probably make sure this element is in our namespace
				final String childElementLocalName=XMLStyleConstants.getXMLElementLocalName(childAttributeSet); //get the local name of this child element
				if(ELEMENT_VAREQUAL.equals(childElementLocalName)) //if this is variable equal condition
				{
					final VarEqual varEqual=createVarEqual(childElement);  //create the variable equal condition
				  conditionVar.getConditionList().add(varEqual);  //add this variable equal condition
				}
			}
		}
		putQTIObject(element, conditionVar);  //store the QTI object in the map, keyed to the element
		return conditionVar;  //return the object we created
	}

	/**Creates and returns a variable equal condition.
	@param element The element representing the variable equal condition.
	*/
	protected VarEqual createVarEqual(final Element element)
	{
		final VarEqual varEqual=new VarEqual(); //create a new variable equal condition
		final AttributeSet attributeSet=element.getAttributes();  //get the element's attributes
		if(attributeSet!=null)  //if the element has attributes
		{
//G***fix		  varEqual.setRespIdent((String)XMLStyleConstants.getDefinedAttribute(attributeSet, ATTRIBUTE_RESPIDENT));  //set the response ident
		}
		try
		{
//G***Del Debug.trace("Getting ready to create varequal with start offset: "+element.getStartOffset()+" end offset: "+element.getEndOffset());  //G***del
//G***Del Debug.trace("Document length: "+element.getDocument().getLength()); //G***del
Debug.trace("Getting ready to create varequal with value: ", element.getDocument().getText(element.getStartOffset(), element.getEndOffset()-element.getStartOffset()));  //G***del
			varEqual.setValue(element.getDocument().getText(element.getStartOffset(), element.getEndOffset()-element.getStartOffset()));  //get the value of the variable G***make sure there is text
		}
		catch(BadLocationException badLocationException)
		{
			Debug.warn(badLocationException); //G***fix
		}
//G***fix		varEqual.setValue(XMLUtilities.getText(element, false));  //get the value of the variable
		putQTIObject(element, varEqual);  //store the QTI object in the map, keyed to the element
		return varEqual;  //return the object we created
	}

	/**Creates and returns a variable setting.
	@param element The element representing the variable setting.
	*/
	protected SetVar createSetVar(final Element element)
	{
		final SetVar setVar=new SetVar(); //create a new variable setting
		final AttributeSet attributeSet=element.getAttributes();  //get the element's attributes
		if(attributeSet!=null)  //if the element has attributes
		{
		  setVar.setAction((String)XMLStyleConstants.getDefinedAttribute(attributeSet, ATTRIBUTE_ACTION));  //set the action
		}
		try
		{
			setVar.setValue(element.getDocument().getText(element.getStartOffset(), element.getEndOffset()-element.getStartOffset()));  //get the value of the variable G***make sure there is text
		}
		catch(BadLocationException badLocationException)
		{
			Debug.warn(badLocationException); //G***fix
		}
		putQTIObject(element, setVar);  //store the QTI object in the map, keyed to the element
		return setVar;  //return the object we created
	}

	/**Creates an identifier from the element in the form
		<em>itemIdent</em>#<em>objectIdent</em>. The element should have an
		<code>ident</code> attribute and a parent <code>&lt;item&gt;</code> that
		also has an <code>ident</code> attribute.
	@param element The QTI object for which an identifier should be created.
	@return An identifier uniquely identifying the QTI object within the item,
		or <code>null</code> if an ID could not be created because of a missing
		<code>ident</code> attribute.
	*/
/*G***del if not needed
	protected static String createID(final Element element)
	{
		final String ident=((String)XMLStyleConstants.getDefinedAttribute(element.getAttributes(), ATTRIBUTE_IDENT));  //get the ident
		if(ident!=null && ident.length()>0) //if there is an ident
		{
			final Element itemElement=XMLStyleConstants.getAncestorElement(element, ELEMENT_ITEM);  //get the item element in which this element resides
			if(itemElement!=null) //if there is an enclosing item element
			{
					//get the item's ident
				final String itemIdent=((String)XMLStyleConstants.getDefinedAttribute(itemElement.getAttributes(), ATTRIBUTE_IDENT));
				if(itemIdent!=null && itemIdent.length()>0) //if there is an item ident
				{
					return QTIUtilities.createID(itemIdent, ident);  //create an identifier from the item ident and the object ident
				}
			}
		}
		return null;  //show that we couldn't create an ID
	}
*/

	/**Action for submitting an assessment.
	@param assessment The assessment to be submitted.
	*/
	class SubmitAction extends AbstractAction
	{

		/**The assessment to be submitted.*/
		protected final Assessment assessment;

		/**Constructs an assessment submit action.
		@param submitAssessment The assessment to be submitted
		*/
		public SubmitAction(final Assessment submitAssessment)
		{
			super("Submit");	//create the base class G***i18n
			putValue(SHORT_DESCRIPTION, "Submit the assessment.");	//set the short description G***i18n
			putValue(LONG_DESCRIPTION, "Submit the assessment.");	//set the long description G***i18n
			putValue(MNEMONIC_KEY, new Integer('s'));  //set the mnemonic key G***i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.SUBMIT_ICON_FILENAME)); //load the correct icon
			assessment=submitAssessment;  //store the specified assessment
		}

		/**Called when the action should be performed.
		@param e The event causing the action.
		*/
		public void actionPerformed(ActionEvent e)
		{
//G***del			Debug.notify("Ready to grade items: "+getItemViewList().size()); //G***del
			final Map itemResponseMap=new HashMap();  //create a hash map to store lists of responses for each item, each list keyed to an item ident
			final Iterator sectionIterator=assessment.getSectionList().iterator();  //get an iterator to look through each section
		  while(sectionIterator.hasNext())  //while there are more sections
			{
Debug.trace("Found section"); //G***del
				final Section section=(Section)sectionIterator.next();  //get a reference to this section
				final Iterator itemIterator=section.getItemList().iterator(); //get an iterator to look through all the items in this section
				while(itemIterator.hasNext()) //while there are more items
				{
Debug.trace("Found item"); //G***del
				  final Item item=(Item)itemIterator.next();  //get a reference to this item
					final List responseList=new ArrayList();  //create a list to store the responses for this item
					final Presentation presentation=item.getPresentation(); //get the item's presentation
					if(presentation!=null && presentation.getResponseList().size()>0)  //if this item has presentation and there is a response section G***only one response is currently supported
					{
Debug.trace("Found presentation"); //G***del
						final ResponseLID responseLID=(ResponseLID)presentation.getResponseList().get(0);  //get the first response and assume it's a logical ID response G***fix for other response types
						if(responseLID.getRenderList().size()>0)  //if there is a render section G***add support for multiple render sections
						{
						  final RenderChoice renderChoice=(RenderChoice)responseLID.getRenderList().get(0); //get the first render type and assume it's a choice render G***add support for other types
Debug.trace("Found render choice"); //G***del
							final Iterator responseLabelIterator=renderChoice.getResponseLabelList().iterator();  //get an iterator to look through the response labels
							while(responseLabelIterator.hasNext())  //while there are more response labels
							{
								final ResponseLabel responseLabel=(ResponseLabel)responseLabelIterator.next();  //get the next response label
/*G***fix
Debug.trace("Found response label"); //G***del
								final QTIResponseLabelView responseLabelView=(QTIResponseLabelView)getQTIView(responseLabel); //get the view that corresponsds to this response label
								if(responseLabelView!=null) //if a view was created for this response label
								{
Debug.trace("Found response label view"); //G***del
									if(responseLabelView.getToggleButton().isSelected()) //if this response was selected
									{
//G***del						Debug.notify("Answered ident: "+responseLabel.getIdent()); //G***del
										responseList.add(responseLabel.getIdent());  //add this response's ident to the response list
									}
								}
*/
							}
		  				itemResponseMap.put(item.getIdent(), responseList);  //add the response list to the map, keyed to the item's ID
						}
					}
				}
			}
//match the answers to the items, creating a map of right/wrong boolean values
			final Map itemResultMap=new HashMap();  //a map of Boolean values, true representing correct, keyed to items; a hack until the IMS adds more result processing in QTI 1.2
			final Iterator itemIterator=assessment.getItemIterator(); //get an iterator to all the assessment's items
			while(itemIterator.hasNext()) //while there are more assessment items
			{
				final Item item=(Item)itemIterator.next();  //get the next item
				final String itemIdent=item.getIdent(); //get the item's ID
				boolean result=false; //start out assuming this answer is incorrect
				final List responseList=(List)itemResponseMap.get(itemIdent); //get the response list for the given item
				if(responseList!=null && responseList.size()==1)  //if there is list of responses (assume this question only has one answer)
				{
Debug.trace("Only one response for item: ", itemIdent); //G***del
					final String responseIdent=(String)responseList.get(0);  //get the first response
Debug.trace("Response ID: ", responseIdent); //G***del
					final ResponseProcessing responseProcessing=item.getResponseProcessing();  //get the item's response processing
					if(responseProcessing!=null) //if there is response processing
					{
						if(responseProcessing.getResponseConditionList().size()>0)  //if there is at least one response condition G***fix; we're only grabbing the first one now
						{
							final ResponseCondition responseCondition=(ResponseCondition)responseProcessing.getResponseConditionList().get(0);  //get the first response condition
						  if(responseCondition.getConditionVarList().size()>0)  //if there is at least one condition var G***fix; right now we're only grabbing the first one
						  {
								final ConditionVar conditionVar=(ConditionVar)responseCondition.getConditionVarList().get(0); //get the first condition var
								if(conditionVar.getConditionList().size()>0)  //if there is at least one condition var G***fix; right now we're only grabbing the first one
								{
									final Condition condition=(Condition)conditionVar.getConditionList().get(0); //grab the first condition
									Debug.assert(condition instanceof VarEqual, "Unknown condition; we only support VarEqual"); //G***fix
								  final VarEqual varEqual=(VarEqual)condition;  //cast the condition to a VarEqual
								  final String varEqualValue=varEqual.getValue(); //get the expected value
Debug.trace("expected value: ", varEqualValue); //G***del
								  if(responseIdent.equals(varEqualValue)) //if the answer matches their response
										result=true;  //show that this result is correct
								}
						  }
						}
					}
				}
				itemResultMap.put(item, new Boolean(result)); //add this result to the result map, keyed to the item
			}
//display the results
			final int questionCount=itemResultMap.size(); //find out how many questions there are
			int rawScore=0; //start out with no score
			final StringBuffer missedStringBuffer=new StringBuffer();  //create a string buffer to store the missed questions
		  int questionNumber=0; //keep track of the question number; right now, we haven't even got to a question number, yet
			final Iterator resultItemIterator=assessment.getItemIterator(); //get an iterator to all the assessment's items
			while(resultItemIterator.hasNext()) //while there are more assessment items
			{
				++questionNumber; //go to the next question nmber
				final Item item=(Item)resultItemIterator.next();  //get the next item
				final boolean itemResult=((Boolean)itemResultMap.get(item)).booleanValue(); //get the result for this item
				if(itemResult)  //if this item was correct
					rawScore++;	//add one to the number of questions they got right
				else  //if this answer is wrong
				{
					if(missedStringBuffer.length()>0) //if we're already displayed incorrect question numbers
						missedStringBuffer.append(", ");  //separate the questions missed
					missedStringBuffer.append(questionNumber);  //append this question number
				}
			}
			final String rawScoreString=String.valueOf(rawScore)+" out of "+questionCount;	//make a string with their raw score information G***fix; i18n
		  final String percScoreString=String.valueOf(Math.round(rawScore/questionCount*1000)/10)+"%"; //G***fix; comment; use MessageFormat
		  final StringBuffer resultStringBuffer=new StringBuffer(); //create a new string buffer in which to construct the results display
			resultStringBuffer.append("<html>");
//G***fix		  resultStringBuffer.append("<h1>Assessment Results</h1>");
		  resultStringBuffer.append("<p><strong>Raw Score:</strong> ").append(rawScoreString).append("</p>");
		  resultStringBuffer.append("<p><strong>Percentage Correct:</strong> ").append(percScoreString).append("</p>");
			if(rawScore!=questionCount)	//if they missed any questions at all
				resultStringBuffer.append("<p><strong>Questions Missed:</strong> ").append(missedStringBuffer).append("</p>");
		  resultStringBuffer.append("</html>");

		  JOptionPane.showMessageDialog(null, resultStringBuffer.toString(), "Assessment Results", JOptionPane.INFORMATION_MESSAGE);	//G***i18n; comment; fix null

//G***del		  Debug.notify(resultStringBuffer.toString());

//G***fix			final String resultString=resultStringBuffer.toString();  //G***testing
/*G***fix
Debug.notify("<html><p><strong>Raw Score:</strong> 0 out of 1</p></html>");
Debug.notify("<html><h1>Quiz Results</h1><p><strong>Raw Score:</strong> 0 out of 1</p></html>");
*/
/*G***fix
Debug.notify("<html><h1><center>Quiz Results</center></h1><p><strong>Raw Score:</strong> 0 out of 1</p><p><strong>Percentage Correct:</strong> 0%</P><p><strong>Questions Missed:</strong>1</p></html>");
Debug.notify("<html><h1><center>Quiz Results</center></h1><p><strong>Raw Score:</strong> 0 out of 1</p><p><strong>Percentage Correct:</strong> 0%</P><p><strong>Questions Missed:</strong>1</p></html>");
Debug.notify("<html><h1><center>Quiz Results</center></h1><p><strong>Raw Score:</strong> 0 out of 1</p><p><strong>Percentage Correct:</strong> 0%</P><p><strong>Questions Missed:</strong>1</p></html>");
Debug.notify("<html><h1><center>Quiz Results</center></h1><p><strong>Raw Score:</strong> 0 out of 1</p><p><strong>Percentage Correct:</strong> 0%</P><p><strong>Questions Missed:</strong>1</p></html>");
*/
/*G***fix
SwingUtilities.invokeLater(new Runnable()	//G***testing; comment
	{
		public void run() {Debug.notify(resultString);}	//update the status
	});
*/
//G***fix Debug.notify(resultStringBuffer.toString());
		}
	}

}