/*
 * Copyright © 2019 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.collections.comparators;

import java.util.*;

import javax.annotation.*;

import static java.util.Objects.*;

/**
 * A comparator that compares objects according to some explicit order, as specified in a list.
 * @param <T> The type of objects that may be compared by this comparator.
 * @author Garret Wilson
 */
public class ExplicitOrderComparator<T> implements Comparator<T> {

	private final List<T> order;

	/**
	 * Returns the list that determines the explicit order of the objects.
	 * @return The list that determines the explicit order of the objects.
	 */
	protected List<T> getOrder() {
		return order;
	}

	private final boolean orderedFirst;

	/**
	 * Indicates whether explicitly ordered items should come before items with no known order.
	 * @return <code>true</code> if explicitly ordered items should come before items with no known order.
	 */
	protected boolean isOrderedFirst() {
		return orderedFirst;
	}

	/**
	 * Constructs a comparator to compare objects based upon the order in which they appear in a given list, placing ordered items before non-ordered items.
	 * @param <T> The type of objects that may be compared by this comparator.
	 * @param order The list that determines the order of the objects.
	 * @return A new explicit order comparator, with explicitly ordered items ordered before other items.
	 * @throws NullPointerException if the given list is <code>null</code>.
	 */
	public static <T> Comparator<T> explicitOrderFirst(@Nonnull final List<T> order) {
		return new ExplicitOrderComparator<>(order, true);
	}

	/**
	 * Constructs a comparator to compare objects based upon the order in which they appear in a given list, placing ordered items after non-ordered items.
	 * @param <T> The type of objects that may be compared by this comparator.
	 * @param order The list that determines the order of the objects.
	 * @return A new explicit order comparator, with explicitly ordered items ordered after other items.
	 * @throws NullPointerException if the given list is <code>null</code>.
	 */
	public static <T> Comparator<T> explicitOrderLast(@Nonnull final List<T> order) {
		return new ExplicitOrderComparator<>(order, false);
	}

	/**
	 * Constructs a comparator to compare objects based upon the order in which they appear in a given list.
	 * @param order The list that determines the order of the objects.
	 * @param orderedFirst <code>true</code> if explicitly ordered items should come before items with no known order.
	 * @throws NullPointerException if the given list is <code>null</code>.
	 */
	private ExplicitOrderComparator(@Nonnull final List<T> order, final boolean orderedFirst) {
		this.order = requireNonNull(order, "Order list cannot be null."); //save the default order list
		this.orderedFirst = orderedFirst;
	}

	@Override
	public int compare(final T object1, final T object2) {
		final List<T> order = getOrder();
		final int index1 = order.indexOf(object1); //get the first index
		final int index2 = order.indexOf(object2); //get the second index
		if(index1 == -1) {
			if(index2 != -1) {
				return isOrderedFirst() ? 1 : -1; //only second is explicitly ordered
			}
		} else if(index2 == -1) {
			return isOrderedFirst() ? -1 : 1; //only first is explicitly ordered
		}

		return Integer.compare(index1, index2); //compare the indexes in the explicit order list
	}

}
