/*
 * Copyright © 1996-2009 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.garretwilson.model;

import com.globalmentor.java.Java;

/**Indicates an object allows multiple views to an underlying data model.
<p>Bound properties:</p>
<dl>
	<dt>{@link #MODEL_VIEW_PROPERTY} ({@link Integer})</dt>
	<dd>Indicates the data view has been changed.</dd>
</dl>
@author Garret Wilson
*/
public interface ModelViewable
{

	/**The name of the bound view property.*/
	public final static String MODEL_VIEW_PROPERTY=ModelViewable.class.getName()+Java.PACKAGE_SEPARATOR+"modelView";

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
	/**The view in which a list of items is shown.*/
	public final static int LIST_MODEL_VIEW=5;
	/**The view in which the data is shown as a brief overview.*/
	public final static int SUMMARY_MODEL_VIEW=6;
	/**The view in which any source data used to generate the data is shown.*/
	public final static int SOURCE_MODEL_VIEW=7;
	/**The view in which configuration and settings data is shown.*/
	public final static int CONFIGURATION_MODEL_VIEW=8;

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
