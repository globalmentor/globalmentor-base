package com.garretwilson.model;

import com.garretwilson.lang.JavaConstants;

/**Indicates an object allows multiple views to an underlying data model.
<p>Bound properties:</p>
<dl>
	<dt><code>ModelViewable.MODEL_VIEW_PROPERTY</code> (<code>Integer</code>)</dt>
	<dd>Indicates the data view has been changed.</dd>
</dl>
@author Garret Wilson
*/
public interface ModelViewable
{

	/**The name of the bound view property.*/
	public final static String MODEL_VIEW_PROPERTY=ModelViewable.class.getName()+JavaConstants.PACKAGE_SEPARATOR+"modelView";

	/**Represents no view of the data. This should only be used for error
		conditions and when the data is not being viewed.
	*/
	public final static int NO_MODEL_VIEW=0;
	/**The view in which the data is shown in a tree structure.*/
	public final static int TREE_MODEL_VIEW=1;
	/**The view in which the data is shown in a graph structure.*/
	public final static int GRAPH_MODEL_VIEW=2;
	/**The view in which the data is shown as it would be in its final form.*/
	public final static int WYSIWYG_MODEL_VIEW=3;
	/**The view in which a sequence of items is shown, perhaps as a card deck.*/
	public final static int SEQUENCE_MODEL_VIEW=4;
	/**The view in which the data is shown as a brief overview.*/
	public final static int SUMMARY_MODEL_VIEW=5;
	/**The view in which any source data used to generate the data is shown.*/
	public final static int SOURCE_MODEL_VIEW=6;

	/**@return An array of supported model views.*/
	public int[] getSupportedModelViews();

	/**Determines whether this object supports the given data view.
	@param dataView A model view such as <code>SUMMARY_MODEL_VIEW</code>.
	@return <code>true</code> if and only if this object supports the indicated
		model view.
	*/
	public boolean isModelViewSupported(final int modelView);	

	/**@return The default view of the data, such as <code>SUMMARY_MODEL_VIEW</code>.*/
	public int getDefaultModelView();

	/**@return The view of the data, such as <code>SUMMARY_MODEL_VIEW</code>.*/
	public int getModelView();

	/**Sets the view of the data.
	@param newView The view of the data, such as <code>SUMMARY_MODEL_VIEW</code>.
	@exception IllegalArgumentException Thrown if the given view is not supported.
	*/
	public void setModelView(final int newView);
}
