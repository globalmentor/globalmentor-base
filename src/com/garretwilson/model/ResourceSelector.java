package com.garretwilson.model;

import java.io.IOException;
import com.garretwilson.io.URIAccessible;

/**An interface to an object that allows resources to be selected for input or output.
@author Garret Wilson
*/
public interface ResourceSelector extends URIAccessible
{

	/**Selects a resource for input.
	@param oldResource The currently selected resource, if applicable, or
		<code>null</code> if there is no selected resource.
	@return The selected resource, or <code>null</code> if selection was
		canceled.
	@exception IOException Thrown if there is an error locating a resource.
	*/
	public Resource selectInputResource(final Resource oldResource) throws IOException;

	/**Selects a resource for output.
	@param oldResource The currently selected resource, if applicable, or
		<code>null</code> if there is no selected resource.
	@return The selected resource, or <code>null</code> if selection was
		canceled.
	@exception IOException Thrown if there is an error locating a resource.
	*/
	public Resource selectOutputResource(final Resource oldResource) throws IOException;

}
