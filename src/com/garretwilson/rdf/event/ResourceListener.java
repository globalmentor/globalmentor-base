package com.garretwilson.rdf.event;

import java.util.EventListener;

/**Indicates the implementing class can listen for events relating to resource
	additions, removals, and changes.
@author Garret Wilson
*/
public interface ResourceListener extends EventListener //G***mabye replace com.globalmentor.mentoract.server.event.ResourceListener with this class
{

	/**Called when a resource is added.
	The URI of the added resource will be in <code>getResoureURI()</code>, and
	the resource description may be in <code>getResource()</code>, depending on
	the implementing class.
	@param resourceEvent The event identifying the resource.
	@see ResourceEvent#getResourceURI
	@see ResourceEvent#getResource
	*/
	public void onResourceAdded(final ResourceEvent resourceEvent);

}