package com.garretwilson.swing.text.xml.qti;

import java.awt.BorderLayout;
import java.awt.Color;
import javax.swing.*;
import javax.swing.text.*;
import com.garretwilson.assess.qti.QTIConstants;
import com.garretwilson.swing.text.xml.*;
import com.garretwilson.swing.text.xml.css.XMLCSSStyleConstants;
import com.garretwilson.util.Debug;

/**Displays a QTI response label for a coice rendering, along with the
	resulting material. The toggle button's action command will be set to the
	ident of the response label.
@author Garret Wilson
*/
public class QTIChoiceResponseLabelView extends XMLComponentBlockView implements QTIConstants
{

	/**The toggle button (radio button or checkbox) this label view presents.*/
	private final JToggleButton toggleButton;

		/**@return The toggle button (radio button or checkbox) this label view
		  presents.
		*/
		public JToggleButton getToggleButton() {return toggleButton;}

	/**The identification of this response label.*/
//G***del	public final String ident;

	/**Constructs a response label view for a question and adds the view to the
		map of the logical ID response view.
	@param element The element this view is responsible for.
	@param renderChoiceView The choice rendering view with which this response
		label view is associated, or <code>null</code> if there is no associated
		choice rendering view.
//G***del if not needed	@param responseLIDView The enclosing logical ID response view.
	*/
	public QTIChoiceResponseLabelView(final Element element, final QTIRenderChoiceView renderChoiceView/*G***del if not needed, final QTIResponseLIDView responseLIDView*/)
	{
		super(element, X_AXIS); //construct the parent, tiling horizontally G***fix or del: and allowing expansion in both direction
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
		if(allowMultipleSelections)  //if we allow multiple selections
		{
			toggleButton=new JCheckBox(); //display a checkbox, which allows multiple selections
		}
		else  //if we don't allow multiple selections
		{
			toggleButton=new JRadioButton(); //display a radio button, which does not allow multiple selections
		}
		toggleButton.setContentAreaFilled(false); //don't paint the background---let the toggle button be transparent
/*G***del if not needed with the new transparency
		final AttributeSet attributeSet=getAttributes();	//get our attributes
		if(attributeSet!=null)	//if we have attributes
		{
				//get the effective background color from the attributes
			final Color backgroundColor=XMLCSSStyleConstants.getEffectiveBackgroundColor(attributeSet); //G***this sets the background color once---we need to fix this where it will change if the background changes
			getToggleButton().setBackground(backgroundColor); //set the background color of the radio button
		}
*/
		final String ident=(String)XMLStyleUtilities.getDefinedAttribute(element.getAttributes(), ATTRIBUTE_IDENT); //get the ident, if available
		toggleButton.setActionCommand(ident); //set the ident as the action command G***should we check to see if this is null? probably not---just use it as is
		if(renderChoiceView!=null && !allowMultipleSelections) //if we have an enclosing choice rendering view, and we shouldn't allow multiple selections
		{
//G***del			responseLIDView.getResponseLabelViewMap().put(ident, this); //store ourselves in the logical ID response view map, keyed by ident G***do we really want to add ourselves here? outside the constructor somewhere might be a better place
			renderChoiceView.getButtonGroup().add(getToggleButton()); //add our radio button to the enclosing choice rendering button group to allow for mutual exclusion
		}
		add(getToggleButton(), BorderLayout.BEFORE_LINE_BEGINS); //add the radio button before this line begins
	}

}