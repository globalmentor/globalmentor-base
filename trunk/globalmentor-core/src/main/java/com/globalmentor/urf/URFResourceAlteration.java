/*
 * Copyright © 2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.urf;

import java.net.URI;
import java.util.Set;

/**The encapsulation of changes to be performed on a resource, consisting of properties to be removed and/or added.
Alternations are to be performed in the following order:
<ol>
	<li>Any property URI-based removals are performed.</li>
	<li>Any property value removals, for property URIs not removed, are performed.</li>
	<li>Any property value additions are performed.</li>
</ol>
To set a property to the exclusion of all other values, a property addition should be provided and its property URI
included in the set of property URI removals.
<p>If no resource URI is provided, it means that the current URI of the resource should remain unchanged.
Because of these semantics, this interface does not provide a means for indicating a resource should become unnamed.</p>
@author Garret Wilson
*/
public interface URFResourceAlteration
{

	/**The singleton resource alteration specification specifying no alterations.*/
	public final static DefaultURFResourceAlteration NO_RESOURCE_ALTERATION=new DefaultURFResourceAlteration();

	/**@return The new URI of the resource, or <code>null</code> if the current URI of the resource, if any, should remain unchanged.*/
	public URI getResourceURI();

	/**@return The immutable set of URIs of properties to remove.*/
	public Set<URI> getPropertyURIRemovals(); 

	/**@return The immutable set of properties and values to remove.*/
	public Set<URFProperty> getPropertyRemovals(); 

	/**@return The immutable set of properties and values to add.*/
	public Set<URFProperty> getPropertyAdditions(); 

	/**Combines the given alterations with the alterations specified in this object and returns a new specification of the union of alterations.
	The resource URI specified by the given resource alteration, if provided, will override the resource URI specified by these resource alterations, if any.
	@param resourceAlteration The alteration specification to add.
	@return An alteration specification with the combined alterations specified by this specification and the given specification.
	*/
	public URFResourceAlteration add(final URFResourceAlteration resourceAlteration);

}
