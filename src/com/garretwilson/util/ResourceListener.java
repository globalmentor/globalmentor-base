package com.garretwilson.util;

import java.util.EventListener;

/**Indicates the implementing class can listen for events relating to resource
	additions, removals, and changes.
@author Garret Wilson
*/
public interface ResourceListener extends EventListener //G***mabye replace com.globalmentor.mentoract.server.event.ResourceListener with this class
{

	/**Called when a resource is added.
	The URI of the added resource will be in <code>getResoureURI()</code>.
	The resource description may be in <code>getResource()</code>, and information
		about the parent resource may be in <code>getParentResourceURI()</code> and
		<code>getParentResource()</code>, depending on the implementing class.
	@param resourceEvent The event identifying the added resource.
	@see ResourceEvent#getResourceURI
	@see ResourceEvent#getResource
	*/
	public void onResourceAdded(final ResourceEvent resourceEvent);

	/**Called when a resource is removed.
	The URI of the removed resource will be in <code>getResoureURI()</code>.
	The resource description may be in <code>getResource()</code>, and information
		about the parent resource may be in <code>getParentResourceURI()</code> and
		<code>getParentResource()</code>, depending on the implementing class.
	@param resourceEvent The event identifying the removed resource.
	@see ResourceEvent#getResourceURI
	@see ResourceEvent#getResource
	*/
	public void onResourceRemoved(final ResourceEvent resourceEvent);

	/**Called when a resource is renamed to a different referenceURI.
	The new URI of the renamed resource will be in <code>getResoureURI()</code>,
		and its resource description may be in <code>getResource()</code>, depending
		on the implementing class.
	The old URI of the renamed resource will be in <code>getOldResourceURI</code>,
		and its old resource description may be in <code>getOldResource()</code>,
		depending on the implementing class.
	Information about the new parent resource may be in
		<code>getParentResourceURI()</code> and <code>getParentResource()</code>,
		and information about the old parent resource may be in
		<code>getOldParentResourceURI()</code> and <code>getOldParentResource()</code>, 
		depending on the implementing class.
	@param resourceEvent The event identifying the removed resource.
	@see ResourceEvent#getOldParentResourceURI
	@see ResourceEvent#getOldParentResource
	@see ResourceEvent#getOldResourceURI
	@see ResourceEvent#getOldResource
	@see ResourceEvent#getParentResourceURI
	@see ResourceEvent#getParentResource
	@see ResourceEvent#getResourceURI
	@see ResourceEvent#getResource
	*/
	public void onResourceMoved(final ResourceEvent resourceEvent);

}