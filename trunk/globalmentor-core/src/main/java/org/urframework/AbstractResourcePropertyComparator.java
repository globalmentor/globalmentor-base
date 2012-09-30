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

package org.urframework;

import java.net.URI;

import static com.globalmentor.java.Objects.*;

import com.globalmentor.collections.comparators.AbstractSortOrderComparator;
import com.globalmentor.collections.comparators.SerialDelegateComparator;
import com.globalmentor.collections.comparators.SortOrder;
import com.globalmentor.util.*;

/**Abstract comparator that sorts resources based upon the value of some identified property.
@author Garret Wilson
@see SerialDelegateComparator
*/
public abstract class AbstractResourcePropertyComparator extends AbstractSortOrderComparator<URFResource>
{

	/**The property URI the value on which comparison will be made.*/
	private final URI propertyURI;

		/**@return The property URI the value on which comparison will be made.*/
		public URI getPropertyURI() {return propertyURI;}

	/**Property URI and sort order constructor.
	@param propertyURI The property URI the value on which comparison will be made.
	@param sortOrder The order in which to perform comparisons.
	@throws NullPointerException if the given property URI and/or sort order is <code>null</code>.
	*/
	protected AbstractResourcePropertyComparator(final URI propertyURI, final SortOrder sortOrder)
	{
		super(sortOrder);	//construct the parent class
		this.propertyURI=checkInstance(propertyURI, "Property URI cannot be null.");
	}

}
