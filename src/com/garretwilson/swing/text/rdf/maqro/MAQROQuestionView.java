package com.garretwilson.swing.text.rdf.maqro;

import java.util.*;
import javax.swing.ButtonGroup;
import javax.swing.text.*;
import com.garretwilson.swing.text.xml.*;

/**View representing a MAQRO question.
@author Garret Wilson
*/
public class MAQROQuestionView extends XMLBlockView
{

	/**The button group for the choices, if any.*/
	private final ButtonGroup buttonGroup=new ButtonGroup();

		/**@return The button group for the choices.*/
		public ButtonGroup getButtonGroup() {return buttonGroup;}

	/**Constructs a choice rendering view.
	@param element The element this view is responsible for.
	@param axis The tiling axis, either <code>View.X_AXIS</code> or <code>View.Y_AXIS</code>.
	*/
	public MAQROQuestionView(final Element element, final int axis)
	{
		super(element, axis, true, true); //construct the parent, allowing expansion in both direction
	}

}