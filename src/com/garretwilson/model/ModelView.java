package com.garretwilson.model;

import com.garretwilson.util.Verifiable;

/**Represents a view of a model.
@author Garret Wilson
*/
public interface ModelView<M> extends Verifiable
{

	/**Returns the view data model.
	<p>A calling program should first call <code>verify()</code> to ensure
		the data is valid and that the model reflects the currently entered data.</p>
	@return The data model for which this component provides a view.
	@see #verify()
	*/
	public M getModel();

	/**Sets the data model.
	@param newModel The data model for which this object provides a view.
	*/
	public void setModel(final M newModel);

}
