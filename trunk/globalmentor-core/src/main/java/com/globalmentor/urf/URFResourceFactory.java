/*
 * Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

/**A factory to create resources.
@author Garret Wilson
@see URF
*/
public interface URFResourceFactory
{

	/**Creates a resource with the provided URI based upon the type URI, if any.
	If a type URI is provided, a corresponding type property value may be added to the resource before it is returned.
	@param resourceURI The URI of the resource to create, or <code>null</code> if the resource created should be anonymous.
	@param typeURI The URI of the resource type, or <code>null</code> if the type is not known.
	@return The resource created with this URI.
	@exception IllegalArgumentException if a resource could not be created based upon the given criteria.
	*/
	public URFResource createResource(final URI resourceURI, final URI typeURI);

}