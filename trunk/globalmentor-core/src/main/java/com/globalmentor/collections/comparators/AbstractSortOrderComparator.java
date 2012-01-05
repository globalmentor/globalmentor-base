/*
 * Copyright © 1996-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

/**
 * A comparator that can sort in ascending or descending order.
 * @param <T> The type of objects that may be compared by this comparator.
 * @author Garret Wilson
 * @see SortOrder
 */
public abstract class AbstractSortOrderComparator<T> implements SortOrderComparator<T>
{

	/** The order in which to perform comparisons. */
	private final SortOrder sortOrder;

	/** @return The order in which to perform comparisons. */
	public SortOrder getSortOrder()
	{
		return sortOrder;
	}

	/**
	 * Sort order constructor.
	 * @param sortOrder The order in which to perform comparisons.
	 * @throws NullPointerException if the given sort order is <code>null</code>.
	 */
	public AbstractSortOrderComparator(final SortOrder sortOrder)
	{
		this.sortOrder = checkInstance(sortOrder, "Sort order cannot be null.");
	}

	/**
	 * {@inheritDoc} This implementation performs an identity comparison and then delegates to {@link #compareImpl(Object, Object)}, returning a negative version
	 * of the result if {@link #getSortOrder()} is {@link SortOrder#DESCENDING}.
	 */
	public final int compare(final T object1, final T object2)
	{
		if(object1 == object2) //if the resources are identical
		{
			return 0; //identical resources are always equal
		}
		int result = compareImpl(object1, object2); //perform the comparison
		if(result != 0 && getSortOrder() == SortOrder.DESCENDING) //if the objects aren't identical and we are comparing in reverse order
		{
			result = -result; //switch the result
		}
		return result;
	}

	/**
	 * Implementation to compare two objects for order. Returns a negative integer, zero, or a positive integer as the first argument is less than, equal to, or
	 * greater than the second. Implementations do not need to perform special checks for identity, as the {@link #compare(Object, Object)} implementation will do
	 * this.
	 * @param object1 The first object to be compared.
	 * @param object2 The second object to be compared.
	 * @return A negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
	 * @throws ClassCastException if the arguments' types prevent them from being compared by this comparator.
	 */
	protected abstract int compareImpl(final T object1, final T object2);

}
