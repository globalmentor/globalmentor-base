package com.garretwilson.swing.text.xml.qti;

import javax.swing.text.*;
import com.garretwilson.assess.qti.QTIConstants;
import com.garretwilson.swing.text.xml.*;

/**Represents a QTI item in a section.
@author Garret Wilson
*/
public class QTIItemView extends XMLBlockView implements QTIConstants
{

	/**The logical ID response view; currently only one is supported.*/ //G***fix; update for multiple response types
//G***del 	QTIResponseLIDView responseLIDView=null;

	/**The identification of this response label.*/
//G***del	public final String ident;

	/**Constructs an item view.
	@param element The element this view is responsible for.
	@param axis The tiling axis, either <code>View.X_AXIS</code> or
		<code>View.Y_AXIS</code>.
	*/
	public QTIItemView(final Element element, final int axis)
	{
		super(element, axis, true, true); //construct the parent, allowing expansion in both direction
//G***del		ident=(String)XMLStyleConstants.getDefinedAttribute(element.getAttributes(), ATTRIBUTE_IDENT); //get the ident, if available
	}

}