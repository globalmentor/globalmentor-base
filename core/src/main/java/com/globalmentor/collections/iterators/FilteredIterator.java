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
import java.util.function.Predicate;

/**
 * An iterator that filters an existing iterator using a filter.
 * @implSpec This version does not support {@link #remove()}.
 * @implSpec This version releases the decorated iterator when iteration is finished.
 * @implSpec This class is not thread safe.
 * @param <E> The type of element returned by the iterator.
 * @author Garret Wilson
 * @deprecated in favor of {@link FilterIterator}.
 */
@Deprecated(forRemoval = true)
public class FilteredIterator<E> extends FilterIterator<E> {

	/**
	 * Decorated iterator and filter constructor.
	 * @param iterator The decorated iterator to be filtered.
	 * @param filter The filter for this iterator's elements.
	 * @throws NullPointerException if the given iterator and/or filter is <code>null</code>.
	 */
	public FilteredIterator(final Iterator<E> iterator, final Predicate<E> filter) {
		super(iterator, filter);
	}

}
