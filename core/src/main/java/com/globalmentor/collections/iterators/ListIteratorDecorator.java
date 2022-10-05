/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

/**
 * A list iterator that wraps an existing list iterator, providing access through the {@link ListIterator} interface.
 * @author Garret Wilson
 */
public class ListIteratorDecorator<E> extends IteratorDecorator<E> implements ListIterator<E> {

	/** The list iterator this class decorates. */
	protected final ListIterator<E> listIterator;

	/**
	 * List iterator constructor.
	 * @param listIterator The list iterator this list iterator should decorate.
	 * @throws NullPointerException if the given iterator is <code>null</code>.
	 */
	public ListIteratorDecorator(final ListIterator<E> listIterator) {
		super(listIterator); //construct the parent class
		this.listIterator = listIterator; //save the list iterator
	}

	// Query Operations

	/**
	 * Returns <code>true</code> if this list iterator has more elements when traversing the list in the reverse direction. (In other words, returns <code>true</code> if
	 * <code>previous</code> would return an element rather than throwing an exception.)
	 *
	 * @return <code>true</code> if the list iterator has more elements when traversing the list in the reverse direction.
	 */
	public boolean hasPrevious() {
		return listIterator.hasPrevious();
	}

	/**
	 * Returns the previous element in the list. This method may be called repeatedly to iterate through the list backwards, or intermixed with calls to
	 * <code>next</code> to go back and forth. (Note that alternating calls to <code>next</code> and <code>previous</code> will return the same element repeatedly.)
	 *
	 * @return the previous element in the list.
	 * 
	 * @throws NoSuchElementException if the iteration has no previous element.
	 */
	public E previous() {
		return listIterator.previous();
	}

	/**
	 * Returns the index of the element that would be returned by a subsequent call to <code>next</code>. (Returns list size if the list iterator is at the end of the
	 * list.)
	 *
	 * @return the index of the element that would be returned by a subsequent call to <code>next</code>, or list size if list iterator is at end of list.
	 */
	public int nextIndex() {
		return listIterator.nextIndex();
	}

	/**
	 * Returns the index of the element that would be returned by a subsequent call to <code>previous</code>. (Returns -1 if the list iterator is at the beginning of
	 * the list.)
	 *
	 * @return the index of the element that would be returned by a subsequent call to <code>previous</code>, or -1 if list iterator is at beginning of list.
	 */
	public int previousIndex() {
		return listIterator.previousIndex();
	}

	// Modification Operations

	/**
	 * Replaces the last element returned by <code>next</code> or <code>previous</code> with the specified element (optional operation). This call can be made only if
	 * neither <code>ListIterator.remove</code> nor <code>ListIterator.add</code> have been called after the last call to <code>next</code> or <code>previous</code>.
	 *
	 * @param o the element with which to replace the last element returned by <code>next</code> or <code>previous</code>.
	 * @throws UnsupportedOperationException if the <code>set</code> operation is not supported by this list iterator.
	 * @throws ClassCastException if the class of the specified element prevents it from being added to this list.
	 * @throws IllegalArgumentException if some aspect of the specified element prevents it from being added to this list.
	 * @throws IllegalStateException if neither <code>next</code> nor <code>previous</code> have been called, or <code>remove</code> or <code>add</code> have been called after
	 *           the last call to <code>next</code> or <code>previous</code>.
	 */
	public void set(E o) {
		listIterator.set(o);
	}

	/**
	 * Inserts the specified element into the list (optional operation). The element is inserted immediately before the next element that would be returned by
	 * <code>next</code>, if any, and after the next element that would be returned by <code>previous</code>, if any. (If the list contains no elements, the new element
	 * becomes the sole element on the list.) The new element is inserted before the implicit cursor: a subsequent call to <code>next</code> would be unaffected, and
	 * a subsequent call to <code>previous</code> would return the new element. (This call increases by one the value that would be returned by a call to
	 * <code>nextIndex</code> or <code>previousIndex</code>.)
	 *
	 * @param o the element to insert.
	 * @throws UnsupportedOperationException if the <code>add</code> method is not supported by this list iterator.
	 * 
	 * @throws ClassCastException if the class of the specified element prevents it from being added to this list.
	 * 
	 * @throws IllegalArgumentException if some aspect of this element prevents it from being added to this list.
	 */
	public void add(E o) {
		listIterator.add(o);
	}

}
