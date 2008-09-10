package com.garretwilson.swing.text.xml.qti;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.util.*;
import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.text.*;
import com.garretwilson.resources.icon.IconResources;
import com.garretwilson.swing.text.xml.*;
import com.globalmentor.mentoract.qti.Assessment;
import com.globalmentor.util.Debug;

//G***del class if not needed

/**Displays a QTI assessment, along with the appropriate submit button.
@author Garret Wilson
*/
public class QTIAssessmentView extends XMLComponentBlockView
{

	/**The button used for submitting the assessment.*/
//G***del if not needed	private final JButton submitButton;

		/**@return The button used for submitting the assessment.*/
//G***del if not needed		public JButton getSubmitButton() {return submitButton;}

	/**The list of item views. within the various sections.*/
//G***del	private final List itemViewList=new ArrayList();

		/**@return The list of item views. within the various sections.*/
//G***del		public List getItemViewList() {return itemViewList;}

	/**Constructs an assessment view.
	@param element The element this view is responsible for.
	@param axis The tiling axis, either <code>View.X_AXIS</code> or
		<code>View.Y_AXIS</code>.
	@param assessment The assessment this view represents
	*/
	public QTIAssessmentView(final Element element, final int axis, final Assessment assessment)
	{
		super(element, axis); //construct the parent G***fix or del: and allowing expansion in both direction
/*G***fix
		final String ident=(String)XMLStyleConstants.getDefinedAttribute(element.getAttributes(), ATTRIBUTE_IDENT); //get the ident, if available
		if(responseLIDView!=null) //if we have an enclosing logical response view
		{
			responseLIDView.getResponseLabelViewMap().put(ident, this); //store ourselves in the logical ID response view map, keyed by ident
			responseLIDView.getButtonGroup().add(getRadioButton()); //add our radio button to the enclosing logical ID button group
		}
		add(getRadioButton(), BorderLayout.BEFORE_LINE_BEGINS); //add the radio button before this line begins
		final JButton submitButton=new JButton(submitAction); //create a new button for the submission
		add(submitButton, BorderLayout.AFTER_LAST_LINE); //add the button to the bottom of the form
*/
	}

}