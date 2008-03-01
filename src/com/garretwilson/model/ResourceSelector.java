package com.garretwilson.model;

import java.io.IOException;
import java.net.URI;
import com.garretwilson.io.URIAccessible;
import com.garretwilson.net.Resource;
import com.globalmentor.util.prefs.Preferencesable;

/**An interface to an object that allows resources to be selected for input or output.
@param <R> The type of resource being selected.
@author Garret Wilson
*/
public interface ResourceSelector<R extends Resource> extends URIAccessible, Preferencesable
{

	/**Retrieves a description of the resource with the given reference URI.
	@param referenceURI The reference URI of the resource in question.
	@return A description of the identified resource.
	@exception IOException Thrown if there is an error retrieving the resource
		description.
	*/
	public R getResource(final URI referenceURI) throws IOException;

	/**Selects a resource for input.
	@param oldResource The currently selected resource, if applicable, or
		<code>null</code> if there is no selected resource.
	@return The selected resource, or <code>null</code> if selection was
		canceled.
	@exception SecurityException Thrown if selecting an input resource is not allowed.
	@exception IOException Thrown if there is an error locating a resource.
	*/
	public R selectInputResource(final R oldResource) throws SecurityException, IOException;

	/**Selects a resource for output.
	@param oldResource The currently selected resource, if applicable, or
		<code>null</code> if there is no selected resource.
	@return The selected resource, or <code>null</code> if selection was
		canceled.
	@exception SecurityException Thrown if selecting an output resource is not allowed.
	@exception IOException Thrown if there is an error locating a resource.
	*/
	public R selectOutputResource(final R oldResource) throws SecurityException, IOException;

}
