/*
 * Copyright © 2008-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package org.urframework.content;

import org.urframework.*;

import com.globalmentor.collections.comparators.*;
import com.globalmentor.java.Comparables;

import static org.urframework.content.Content.*;

/**
 * Sorts resources based upon URF content modified time and date. This comparator does not sort unambiguously, and should be used as a delegate comparator of a
 * {@link SerialDelegateComparator}.
 * @author Garret Wilson
 * @see Content#MODIFIED_PROPERTY_URI
 * @see SerialDelegateComparator
 */
public class ResourceContentModifiedComparator extends AbstractSortOrderComparator<URFResource>
{

	/** The lazily-created ascending singleton instance of the comparator. */
	private static ResourceContentModifiedComparator ascendingInstance = null;

	/** The lazily-created descending singleton instance of the comparator. */
	private static ResourceContentModifiedComparator descendingInstance = null;

	/**
	 * Retrieves a singleton instance of the comparator with ascending order.
	 * @return The lazily-created singleton instance of the comparator ascending order.
	 */
	public static ResourceContentModifiedComparator getInstance()
	{
		return getInstance(SortOrder.ASCENDING); //get the ascending singleton instance
	}

	/**
	 * Retrieves a singleton instance of the comparator with the correct sort order.
	 * @param sortOrder The order in which to perform comparisons.
	 * @return The lazily-created singleton instance of the comparator with the given sort order.
	 * @throws NullPointerException if the given sort order is <code>null</code>.
	 */
	public static ResourceContentModifiedComparator getInstance(final SortOrder sortOrder)
	{
		switch(sortOrder)
		//see which sort order is requested
		{
			case ASCENDING:
				if(ascendingInstance == null) //if there is not yet a singleton instance (the race condition here is benign)
				{
					ascendingInstance = new ResourceContentModifiedComparator(sortOrder); //create a new instance
				}
				return ascendingInstance; //return the singleton instance
			case DESCENDING:
				if(descendingInstance == null) //if there is not yet a singleton instance (the race condition here is benign)
				{
					descendingInstance = new ResourceContentModifiedComparator(sortOrder); //create a new instance
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
	protected ResourceContentModifiedComparator(final SortOrder sortOrder)
	{
		super(sortOrder); //construct the parent class
	}

	/**
	 * {@inheritDoc} This implementation compares resources based upon URF content modified time and date. Resources without a modified property are considered
	 * less than those with a modified property.
	 * @see Content#MODIFIED_PROPERTY_URI
	 */
	@Override
	public int compareImpl(final URFResource resource1, final URFResource resource2)
	{
		return Comparables.compare(getModified(resource1), getModified(resource2)); //compare modified datetimes
	}

}
