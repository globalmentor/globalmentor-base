package com.garretwilson.util;

import com.garretwilson.lang.JavaConstants;

/**Indicates an object allows multiple views to an underlying data model.
<p>Bound properties:</p>
<dl>
	<dt><code>DataViewable.DATA_VIEW_PROPERTY</code> (<code>Integer</code>)</dt>
	<dd>Indicates the data view has been changed.</dd>
</dl>
@author Garret Wilson
*/
public interface DataViewable
{

	/**The name of the bound view property.*/
	public final static String DATA_VIEW_PROPERTY=DataViewable.class.getName()+JavaConstants.PACKAGE_SEPARATOR+"dataView";

	/**Represents no view of the data. This should only be used for error
		conditions and when the data is not being viewed.
	*/
	public final static int NO_DATA_VIEW=0;
	/**The view in which the data is shown in a tree structure.*/
	public final static int TREE_DATA_VIEW=1<<1;
	/**The view in which the data is shown in a graph structure.*/
	public final static int GRAPH_DATA_VIEW=1<<2;
	/**The view in which the data is shown as it would be in its final form.*/
	public final static int WYSIWYG_DATA_VIEW=1<<3;
	/**The view in which the data is shown as a brief overview.*/
	public final static int SUMMARY_DATA_VIEW=1<<4;
	/**The view in which any source data used to generate the data is shown.*/
	public final static int SOURCE_DATA_VIEW=1<<5;
	
	/**@return A value representing the supported data views ORed together.*/
	public int getSupportedDataViews();

	/**Determines whether this object supports the data views.
	@param dataViews One or more <code>XXX_DATA_VIEW</code> constants
		ORed together.
	@return <code>true</code> if and only if this object kit supports all the
		indicated data views.
	*/
	public boolean isDataViewsSupported(final int dataViews);	

	/**@return The default view of the data, such as <code>SUMMARY_DATA_VIEW</code>.*/
	public int getDefaultDataView();

	/**@return The view of the data, such as <code>SUMMARY_DATA_VIEW</code>.*/
	public int getDataView();

	/**Sets the view of the data.
	@param newView The view of the data, such as <code>SUMMARY_DATA_VIEW</code>.
	@exception IllegalArgumentException Thrown if the given view is not supported.
	*/
	public void setDataView(final int newView);
}
