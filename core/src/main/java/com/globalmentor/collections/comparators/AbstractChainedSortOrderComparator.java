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

import java.util.Comparator;

import static java.util.Objects.*;

/**
 * A comparator that can can be chained for subordinate sorting in ascending or descending order.
 * <p>
 * Subclasses should delegate to the subordinate comparator if the compared objects are not identical yet evaluate as equal, or if sufficient information is
 * absent for sorting on both objects.
 * </p>
 * <p>
 * No checks are made to prevent circular chaining, which could create infinite loops.
 * </p>
 * @param <T> The type of objects that may be compared by this comparator.
 * @author Garret Wilson
 * @see SortOrder
 */
public abstract class AbstractChainedSortOrderComparator<T> extends AbstractSortOrderComparator<T> {

	/** The comparator to perform subordinate sorting. */
	private final Comparator<T> subordinateComparator;

	/** @return The comparator to perform subordinate sorting. */
	public Comparator<T> getSubordinateComparator() {
		return subordinateComparator;
	}

	/**
	 * Sort order and subordinate comparator constructor.
	 * @param sortOrder The order in which to perform comparisons.
	 * @param subordinateComparator The comparator to perform subordinate sorting.
	 * @throws NullPointerException if the given subordinate comparator and/or sort order is <code>null</code>.
	 */
	public AbstractChainedSortOrderComparator(final SortOrder sortOrder, final Comparator<T> subordinateComparator) {
		super(sortOrder); //construct the parent class
		this.subordinateComparator = requireNonNull(subordinateComparator, "Subordinate comparator cannot be null.");
	}

}
