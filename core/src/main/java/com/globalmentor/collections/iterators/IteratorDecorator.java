/*
 * Copyright © 1996-2016 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import javax.annotation.*;

import static java.util.Objects.*;

/**
 * An iterator that wraps an existing iterator.
 * @apiNote This iterator also serves as an adapter, converting an iterator to an {@link Enumeration}.
 * @implNote Subclasses may override {@link #hasNext()} and/or {@link #next()}, and {@link Enumeration} compatibility will be maintained.
 * 
 * @param <E> The type of elements returned by this iterator.
 * @author Garret Wilson
 */
public class IteratorDecorator<E> extends AbstractIteratorDecorator<E> {

	/** The iterator this class decorates. */
	private final Iterator<E> iterator;

	@Override
	protected Iterator<E> getIterator() {
		return iterator;
	}

	/**
	 * Iterator constructor.
	 * @param iterator The iterator this iterator will decorate.
	 * @throws NullPointerException if the given iterator is <code>null</code>.
	 */
	public IteratorDecorator(@Nonnull final Iterator<E> iterator) {
		this.iterator = requireNonNull(iterator, "Iterator cannot be null."); //save the iterator
	}

}
