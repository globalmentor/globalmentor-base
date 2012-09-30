/*
 * Copyright © 2007-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import com.globalmentor.collections.comparators.*;
import com.globalmentor.java.Longs;

/**
 * Sorts resources based upon creation order. Normally sorting based upon resource creation order alone is not useful. This class is useful to provide a
 * fallback mechanism for sorting on creation order to guarantee uniqueness distinction, either as a superclass or as a subordinate chained comparator.
 * @author Garret Wilson
 * @see URFScope#getCreationOrder()
 * @see AbstractChainedSortOrderComparator
 * @see SerialDelegateComparator
 */
public class ResourceCreationOrderComparator extends AbstractSortOrderComparator<URFResource>
{

	/** The lazily-created ascending singleton instance of the comparator. */
	private static ResourceCreationOrderComparator ascendingInstance = null;

	/** The lazily-created descending singleton instance of the comparator. */
	private static ResourceCreationOrderComparator descendingInstance = null;

	/**
	 * Retrieves a singleton instance of the comparator with ascending order.
	 * @return The lazily-created singleton instance of the comparator ascending order.
	 */
	public static ResourceCreationOrderComparator getInstance()
	{
		return getInstance(SortOrder.ASCENDING); //get the ascending singleton instance
	}

	/**
	 * Retrieves a singleton instance of the comparator with the correct sort order.
	 * @param sortOrder The order in which to perform comparisons.
	 * @return The lazily-created singleton instance of the comparator with the given sort order.
	 * @throws NullPointerException if the given sort order is <code>null</code>.
	 */
	public static ResourceCreationOrderComparator getInstance(final SortOrder sortOrder)
	{
		switch(sortOrder)
		//see which sort order is requested
		{
			case ASCENDING:
				if(ascendingInstance == null) //if there is not yet a singleton instance (the race condition here is benign)
				{
					ascendingInstance = new ResourceCreationOrderComparator(sortOrder); //create a new instance
				}
				return ascendingInstance; //return the singleton instance
			case DESCENDING:
				if(descendingInstance == null) //if there is not yet a singleton instance (the race condition here is benign)
				{
					descendingInstance = new ResourceCreationOrderComparator(sortOrder); //create a new instance
				}
				return descendingInstance; //return the singleton instance
			default:
				throw new AssertionError("Unrecognized sort order: " + sortOrder);
		}
	}

	/**
	 * Sort order constructor.
	 * @param sortOrder The order in which to perform comparisons.
	 * @throws NullPointerException if the given sort order is <code>null</code>.
	 */
	protected ResourceCreationOrderComparator(final SortOrder sortOrder)
	{
		super(sortOrder); //construct the parent class
	}

	/**
	 * {@inheritDoc} This version compares resources based upon their creation order. Usually a subclass will call this version if sufficient comparison
	 * information is unavailable.
	 * @see URFScope#getCreationOrder()
	 */
	@Override
	public int compareImpl(final URFResource resource1, final URFResource resource2)
	{
		return Longs.compare(resource1.getCreationOrder(), resource2.getCreationOrder());
	}

}
