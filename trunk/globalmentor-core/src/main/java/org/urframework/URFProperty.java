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

package org.urframework;

import java.net.URI;

/**An encapsulation of a parent scope, property URI, value, and the associated property-value scope.
<p>Properties calculate {@link #equals(Object)} by whether they have equivalent values for {@link #getPropertyURI()} and {@link #getValue()}.</p>
@author Garret Wilson
*/
public interface URFProperty extends URFValueContext
{

	/**@return The scope to which the property belongs, or <code>null</code> if this is a property definition not attached to any scope.*/
	public URFScope getSubjectScope();

	/**@return The URI of the property.*/
	public URI getPropertyURI();

}
