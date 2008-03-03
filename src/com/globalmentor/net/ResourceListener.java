/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.net;

import java.util.EventListener;

/**Indicates the implementing class can listen for events relating to resource
	additions, removals, and changes.
@author Garret Wilson
*/
public interface ResourceListener extends EventListener
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