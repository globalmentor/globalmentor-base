/*
 * Copyright © 1996-2012 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static com.globalmentor.collections.Lists.*;

import java.util.Comparator;
import java.util.List;

/**
 * A comparator that can perform comparisons based upon a sequence of other comparators. The last comparator in the sequence should unambiguously distinguish
 * non-equal objects.
 * <p>
 * If this comparator is used in descending order mode, delegate comparators that implement {@link SortOrderComparator} should be used in ascending mode, as
 * this comparator will sort in reverse order automatically; otherwise, the descending orders will cancel each other out and each sub-comparison will be
 * performed in ascending order.
 * </p>
 * <p>
 * Descending order for this comparator does <em>not</em> mean that the delegates will be traversed in reverse order.
 * </p>
 * @param <T> The type of objects that may be compared by this comparator.
 * @author Garret Wilson
 */
public class SerialDelegateComparator<T> extends AbstractSortOrderComparator<T> {

	/** The delegate comparators to use for comparison. */
	private final List<Comparator<? super T>> comparators;

	/**
	 * Comparators constructor for ascending order sorting. The last comparator in the sequence should unambiguously distinguish non-equal objects.
	 * @param comparators The delegate comparators.
	 * @throws NullPointerException if the given sort order and/or comparators is <code>null</code>.
	 * @throws IllegalArgumentException if no comparators are given.
	 */
	@SafeVarargs
	public SerialDelegateComparator(final Comparator<? super T>... comparators) {
		this(SortOrder.ASCENDING, comparators); //construct the class for ascending order
	}

	/**
	 * Sort order constructor and comparators constructor. The last comparator in the sequence should unambiguously distinguish non-equal objects.
	 * @param sortOrder The order in which to perform comparisons.
	 * @param comparators The delegate comparators.
	 * @throws NullPointerException if the given sort order and/or comparators is <code>null</code>.
	 * @throws IllegalArgumentException if no comparators are given.
	 */
	@SafeVarargs
	public SerialDelegateComparator(final SortOrder sortOrder, final Comparator<? super T>... comparators) {
		super(sortOrder); //construct the parent class
		if(comparators.length == 0) { //if no comparators are given
			throw new IllegalArgumentException("At least one delegate comparator must be given.");
		}
		@SuppressWarnings("varargs")
		final List<Comparator<? super T>> comparatorList = immutableListOf(comparators); //copy the comparators so no one can modify them externally to this class
		this.comparators = comparatorList;
	}

	/** {@inheritDoc} */
	@Override
	public int compareImpl(final T object1, final T object2) {
		int result = 0; //keep track of the result; we don't need to initialize the value, because we already ensured we'll have at least one delegate comparator, but the compiler doesn't know that, so we initialize anyway
		for(final Comparator<? super T> comparator : comparators) { //delegate to each comparator
			result = comparator.compare(object1, object2);
			if(result != 0) { //if these objects aren't equal
				break; //we're finished comparing
			}
		}
		return result; //return the ending result 
	}

}
