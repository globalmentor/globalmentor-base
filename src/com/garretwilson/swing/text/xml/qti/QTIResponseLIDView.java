package com.garretwilson.swing.text.xml.qti;

//G***del class

import java.util.*;
import javax.swing.ButtonGroup;
import javax.swing.text.*;
import com.garretwilson.swing.text.xml.*;

/**Represents a QTI logical response group.
@author Garret Wilson
*/
public class QTIResponseLIDView extends XMLBlockView
{

	/**The button group for the logical ID responses.*/
	private final ButtonGroup buttonGroup=new ButtonGroup();

		/**The button group for the logical ID responses.*/
		public ButtonGroup getButtonGroup() {return buttonGroup;}

	/**The map of response label views, keyed to response label idents.*/
//G***del	private final Map responseLabelViewMap=new HashMap();

		/**@return The map of response label views, keyed to response label idents.*/
//G***del		public Map getResponseLabelViewMap() {return responseLabelViewMap;}

	/**Constructs a logical ID response view.
	@param element The element this view is responsible for.
	@param axis The tiling axis, either <code>View.X_AXIS</code> or
		<code>View.Y_AXIS</code>.
	*/
	public QTIResponseLIDView(final Element element, final int axis)
	{
		super(element, axis, true, true); //construct the parent, allowing expansion in both direction
	}

}