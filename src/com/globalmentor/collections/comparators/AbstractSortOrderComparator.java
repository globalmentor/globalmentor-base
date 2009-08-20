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

package com.globalmentor.collections.comparators;

import static com.globalmentor.java.Objects.*;

/**A comparator that can sort in ascending or descending order.
@param <T> The type of objects that may be compared by this comparator.
@author Garret Wilson
@see SortOrder
*/
public abstract class AbstractSortOrderComparator<T> implements SortOrderComparator<T>
{

	/**The order in which to perform comparisons.*/
	private final SortOrder sortOrder;
	
		/**@return The order in which to perform comparisons.*/
		public SortOrder getSortOrder() {return sortOrder;}

	/**Sort order constructor.
	@param sortOrder The order in which to perform comparisons.
	@throws NullPointerException if the given sort order is <code>null</code>.
	*/
	public AbstractSortOrderComparator(final SortOrder sortOrder)
	{
		this.sortOrder=checkInstance(sortOrder, "Sort order cannot be null.");
	}
		
}
