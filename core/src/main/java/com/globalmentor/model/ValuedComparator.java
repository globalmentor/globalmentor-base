/*
 * Copyright © 2012 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.model;

import com.globalmentor.collections.comparators.*;
import com.globalmentor.java.Comparables;

/**
 * Comparator that sorts two {@link Valued} objects by their comparable values.
 * 
 * <p>
 * This comparator supports <code>null</code> values.
 * </p>
 * 
 * @author Garret Wilson
 * 
 * @param <V> The type of value contained in the valued objects
 */
public class ValuedComparator<V extends Comparable<V>> extends AbstractSortOrderComparator<Valued<V>> {

	/**
	 * Sort order constructor.
	 * @param sortOrder The order in which to perform comparisons.
	 * @throws NullPointerException if the given sort order is <code>null</code>.
	 */
	public ValuedComparator(final SortOrder sortOrder) {
		super(sortOrder);
	}

	/**
	 * {@inheritDoc}
	 * @see Valued#getValue()
	 */
	@Override
	protected int compareImpl(final Valued<V> object1, final Valued<V> object2) {
		return Comparables.compare(object1.getValue(), object2.getValue());
	}

}