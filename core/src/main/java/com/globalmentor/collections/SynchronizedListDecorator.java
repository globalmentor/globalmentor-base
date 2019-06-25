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

package com.globalmentor.collections;

import java.util.*;

/**
 * A list that wraps an existing list, providing access through the {@link List} interface. All collection access is synchronized on the provided
 * synchronization object.
 * @param <E> The type of element contained in the list.
 * @author Garret Wilson
 */
public class SynchronizedListDecorator<E> extends SynchronizedCollectionDecorator<E> implements List<E> { //TODO fix to return a synchronized list iterator

	/** The list this class decorates. */
	protected final List<E> list;

	/**
	 * List constructor.
	 * @param list The list this list should decorate.
	 * @param mutex The mutual exclusion synchronization object.
	 * @throws NullPointerException if the provided list and/or mutex is <code>null</code>.
	 */
	public SynchronizedListDecorator(final List<E> list, final Object mutex) {
		super(list, mutex); //construct the parent class
		this.list = list; //save the list
	}

	// Bulk Modification Operations

	@Override
	public boolean addAll(int index, Collection<? extends E> c) {
		synchronized(mutex) {
			return list.addAll(index, c);
		}
	}

	// Positional Access Operations

	@Override
	public E get(int index) {
		synchronized(mutex) {
			return list.get(index);
		}
	}

	@Override
	public E set(int index, E element) {
		synchronized(mutex) {
			return list.set(index, element);
		}
	}

	@Override
	public void add(int index, E element) {
		synchronized(mutex) {
			list.add(index, element);
		}
	}

	@Override
	public E remove(int index) {
		synchronized(mutex) {
			return list.remove(index);
		}
	}

	// Search Operations

	@Override
	public int indexOf(Object o) {
		synchronized(mutex) {
			return list.indexOf(o);
		}
	}

	@Override
	public int lastIndexOf(Object o) {
		synchronized(mutex) {
			return list.lastIndexOf(o);
		}
	}

	// List Iterators

	@Override
	public ListIterator<E> listIterator() {
		synchronized(mutex) {
			return list.listIterator();
		}
	}

	@Override
	public ListIterator<E> listIterator(int index) {
		synchronized(mutex) {
			return list.listIterator(index);
		}
	}

	// View

	@Override
	public List<E> subList(int fromIndex, int toIndex) {
		synchronized(mutex) {
			return list.subList(fromIndex, toIndex);
		}
	}

	@Override
	public Spliterator<E> spliterator() {
		synchronized(mutex) {
			return Spliterators.spliterator(this, Spliterator.ORDERED);
		}
	}

}
