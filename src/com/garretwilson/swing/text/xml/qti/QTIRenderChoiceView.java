package com.garretwilson.swing.text.xml.qti;

import java.util.*;
import javax.swing.ButtonGroup;
import javax.swing.text.*;
import com.garretwilson.swing.text.xml.*;

/**Represents a QTI choice rendering.
@author Garret Wilson
*/
public class QTIRenderChoiceView extends XMLBlockView
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
	@param axis The tiling axis, either <code>View.X_AXIS</code> or
		<code>View.Y_AXIS</code>.
	*/
	public QTIRenderChoiceView(final Element element, final int axis)
	{
		super(element, axis, true, true); //construct the parent, allowing expansion in both direction
	}

}