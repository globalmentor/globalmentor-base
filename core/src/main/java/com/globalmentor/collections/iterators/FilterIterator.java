/*
 * Copyright © 2011 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.collections.iterators;

import java.util.*;

import static java.util.Objects.*;

import com.globalmentor.model.Filter;

/**
 * An iterator that filters an existing iterator using a {@link Filter}.
 * @implSpec This version does not support {@link #remove()}.
 * @implSpec This version releases the decorated iterator when iteration is finished.
 * @implSpec This class is not thread safe.
 * @param <E> The type of element returned by the iterator.
 * @author Garret Wilson
 */
public class FilterIterator<E> extends AbstractFilteredIterator<E> {

	/** The filter for this iterator's elements. */
	private final Filter<E> filter;

	/**
	 * Decorated iterator and filter constructor.
	 * @param iterator The decorated iterator to be filtered.
	 * @param filter The filter for this iterator's elements.
	 * @throws NullPointerException if the given iterator and/or filter is <code>null</code>.
	 */
	public FilterIterator(final Iterator<E> iterator, final Filter<E> filter) {
		super(iterator);
		this.filter = requireNonNull(filter);
	}

	/**
	 * {@inheritDoc} This version delegates to {@link Filter#isPass(Object)}.
	 */
	@Override
	protected boolean isPass(final E element) {
		return filter.isPass(element);
	}

}
