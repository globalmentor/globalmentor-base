package com.garretwilson.model;

import java.io.IOException;
import java.net.URI;
import com.garretwilson.io.URIAccessible;

/**An interface to an object that allows resources to be selected for input or output.
@author Garret Wilson
*/
public interface ResourceSelector extends URIAccessible
{

	/**Retrieves a description of the resource with the given reference URI.
	@param referenceURI The reference URI of the resource in question.
	@return A description of the identified resource.
	@exception IOException Thrown if there is an error retrieving the resource
		description.
	*/
	public Resource getResource(final URI referenceURI) throws IOException;

	/**Selects a resource for input.
	@param oldResource The currently selected resource, if applicable, or
		<code>null</code> if there is no selected resource.
	@return The selected resource, or <code>null</code> if selection was
		canceled.
	@exception SecurityException Thrown if selecting an input resource is not allowed.
	@exception IOException Thrown if there is an error locating a resource.
	*/
	public Resource selectInputResource(final Resource oldResource) throws SecurityException, IOException;

	/**Selects a resource for output.
	@param oldResource The currently selected resource, if applicable, or
		<code>null</code> if there is no selected resource.
	@return The selected resource, or <code>null</code> if selection was
		canceled.
	@exception SecurityException Thrown if selecting an output resource is not allowed.
	@exception IOException Thrown if there is an error locating a resource.
	*/
	public Resource selectOutputResource(final Resource oldResource) throws SecurityException, IOException;

}
