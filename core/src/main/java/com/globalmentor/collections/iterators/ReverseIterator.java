/*
 * Copyright © 1996-2022 GlobalMentor, Inc. <https://www.globalmentor.com/>
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
 * An iterator that wraps an existing {@link ListIterator} and provides its elements in reverse order.
 * @author Garret Wilson
 */
class ReverseIterator<E> implements Iterator<E> {

	/** The list iterator this class decorates. */
	protected final ListIterator<E> listIterator;

	/**
	 * List iterator constructor. The list list iterator will be placed at the end of the list.
	 * @implNote For added efficiency, the decorated list iterator should already be at the end of the list.
	 * @param listIterator The list iterator this iterator should decorate.
	 * @throws NullPointerException if the given iterator is <code>null</code>.
	 */
	public ReverseIterator(@Nonnull final ListIterator<E> listIterator) {
		this.listIterator = requireNonNull(listIterator, "Iterator cannot be null"); //save the iterator
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation delegates to the decorated iterator's opposite method, {@link ListIterator#hasPrevious()}.
	 */
	@Override
	public boolean hasNext() {
		return listIterator.hasPrevious();
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation delegates to the decorated iterator's opposite method, {@link ListIterator#previous()}.
	 */
	@Override
	public E next() {
		return listIterator.previous();
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation delegates to the decorated iterator's {@link ListIterator#remove()} method.
	 */
	@Override
	public void remove() {
		listIterator.remove();
	}

}
