package com.garretwilson.swing.text.xml.qti;

import java.awt.Point;
import java.awt.Shape;
import java.util.*;
import javax.swing.*;
import javax.swing.text.*;
import com.garretwilson.awt.geom.GeometryUtilities;
import com.garretwilson.swing.text.ViewComponentManager;
import com.garretwilson.swing.text.xml.*;
import com.garretwilson.assess.qti.*;
import com.garretwilson.util.Debug;

/**Represents a QTI hotspot rendering.
@author Garret Wilson
*/
public class QTIRenderHotspotView extends XMLHiddenView implements QTIConstants
{

	/**The button group for the choices.*/
	private final ButtonGroup buttonGroup=new ButtonGroup();

		/**@return The button group for the choices.*/
		public ButtonGroup getButtonGroup() {return buttonGroup;}

	/**The map of response label views, keyed to response label idents.*/
//G***del	private final Map responseLabelViewMap=new HashMap();

		/**@return The map of response label views, keyed to response label idents.*/
//G***del		public Map getResponseLabelViewMap() {return responseLabelViewMap;}

	/**Constructs a choice rendering view.
	@param element The element this view is responsible for.
	@param materialImageView The view created for the image with which these
		hotspots are associated.
	*/
	public QTIRenderHotspotView(final Element element, final QTIMaterialImageView materialImageView)
	{
		super(element); //construct the parent
		if(materialImageView!=null) //if we were passed a view containing an image
		{
			final ViewComponentManager componentManager=materialImageView.getComponentManager(); //get the component manager associated with the view
				//G***maybe put some of this into common routines
			boolean allowMultipleSelections=false;  //start out assuming we won't allow multiple selections
				//get the enclosing logical ID response element
			final Element responseLIDElement=XMLStyleUtilities.getAncestorElement(element, ELEMENT_RESPONSE_LID);
			final AttributeSet responseLIDAttributeSet=responseLIDElement!=null ?
					responseLIDElement.getAttributes() : null;	//get the attributes of the logical ID response element
			if(responseLIDAttributeSet!=null)  //if the logical ID response has attributes
			{
					//get the cardinality defined for the response LID G***why not just store the responseLID in the view?
				final String cardinality=XMLStyleUtilities.getXMLAttributeValue(responseLIDAttributeSet, null, ATTRIBUTE_RCARDINALITY);
					//if we allow multiple selections
				if(MULTIPLE_CARDINALITY.equals(cardinality) || ORDERED_CARDINALITY.equals(cardinality))
				{
					allowMultipleSelections=true; //show that we allow multiple selections G***do we want to make this final at some point?
				}
			}
	//G***fix for cardinality and timing		  item.setTitle((String)XMLStyleConstants.getDefinedAttribute(attributeSet, ATTRIBUTE_TITLE));  //set the title
			final int childElementCount=element.getElementCount(); //find out how many child elements there are
			for(int i=0; i<childElementCount; ++i) //look at each child element
			{
				final Element childElement=element.getElement(i); //get a reference to this child element
				final AttributeSet childAttributeSet=childElement.getAttributes();  //get the child element's attribute set
				//G***we should probably make sure this element is in our namespace
				final String elementLocalName=XMLStyleUtilities.getXMLElementLocalName(childAttributeSet); //get the local name of this element
				if(ELEMENT_RESPONSE_LABEL.equals(elementLocalName))  //if this element is a <response_label>
				{
						//get the area type G***use namespaces here
					final String areaType=(String)XMLStyleUtilities.getDefinedAttribute(childAttributeSet, ATTRIBUTE_RAREA);
						//get the text describing the area
					try
					{
						final String areaText=childElement.getDocument().getText(childElement.getStartOffset(), childElement.getEndOffset()-childElement.getStartOffset());
						final Shape area=QTIParser.createArea(areaType, areaText);  //create a shape from the area
Debug.trace("ready to put toggle button in area: ", area);  //G***del
						final Point center=GeometryUtilities.getCenter(area); //get the center point of the area
Debug.trace("new center: ", center);  //G***del
						final JToggleButton toggleButton; //we'll create a toggle button to show in the material image view
						if(allowMultipleSelections)  //if we allow multiple selections
						{
							toggleButton=new JCheckBox(); //display a checkbox, which allows multiple selections
						}
						else  //if we don't allow multiple selections
						{
							toggleButton=new JRadioButton(); //display a radio button, which does not allow multiple selections

						}
						toggleButton.setContentAreaFilled(false); //don't paint the background---let the toggle button be transparent
						toggleButton.setHorizontalAlignment(SwingConstants.CENTER); //center the button information horizontally
						toggleButton.setVerticalAlignment(SwingConstants.CENTER); //center the button information vertically
						if(!allowMultipleSelections) //if we shouldn't allow multiple selections
						{
							getButtonGroup().add(toggleButton); //add our radio button to the button group to allow for mutual exclusion
						}
//G***del						componentManager.add(toggleButton, center.x, center.y, true); //add the toggle button to the image view's component manager, centered at the center of the hotspot area
						componentManager.add(toggleButton, area.getBounds()); //add the toggle button to the image view's component manager, specifying the relative location and size
					}
					catch(BadLocationException badLocationException)  //if we try to get text from the document from a location that doesn't exist
					{
						throw new AssertionError(badLocationException);  //this should never happen unless the document element structure is corrupted
					}
				}
			}
		}
	}

}