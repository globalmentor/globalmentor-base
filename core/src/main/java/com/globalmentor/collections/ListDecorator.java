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
import java.util.function.UnaryOperator;

/**
 * A list that wraps an existing list, providing access through the {@link List} interface.
 * @param <E> The type of element contained in the list.
 * @author Garret Wilson
 */
public class ListDecorator<E> extends CollectionDecorator<E> implements List<E> {

	/** The list this class decorates. */
	protected final List<E> list;

	/**
	 * List constructor.
	 * @param list The list this list should decorate.
	 * @throws NullPointerException if the provided list is <code>null</code>.
	 */
	public ListDecorator(final List<E> list) {
		super(list); //construct the parent class
		this.list = list; //save the list
	}

	// Bulk Modification Operations

	@Override
	public boolean addAll(int index, Collection<? extends E> c) {
		return list.addAll(index, c);
	}

	// Positional Access Operations

	@Override
	public E get(int index) {
		return list.get(index);
	}

	@Override
	public E set(int index, E element) {
		return list.set(index, element);
	}

	@Override
	public void add(int index, E element) {
		list.add(index, element);
	}

	@Override
	public E remove(int index) {
		return list.remove(index);
	}

	// Search Operations

	@Override
	public int indexOf(Object o) {
		return list.indexOf(o);
	}

	@Override
	public int lastIndexOf(Object o) {
		return list.lastIndexOf(o);
	}

	// List Iterators

	@Override
	public ListIterator<E> listIterator() {
		return list.listIterator();
	}

	@Override
	public ListIterator<E> listIterator(int index) {
		return list.listIterator(index);
	}

	// View

	@Override
	public List<E> subList(int fromIndex, int toIndex) {
		return list.subList(fromIndex, toIndex);
	}

	// Default Methods

	@Override
	public void replaceAll(UnaryOperator<E> operator) {
		list.replaceAll(operator);
	}

	@Override
	public void sort(Comparator<? super E> c) {
		list.sort(c);
	}

}
