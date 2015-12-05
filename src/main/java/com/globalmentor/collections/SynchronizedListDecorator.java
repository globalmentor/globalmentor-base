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

	/**
	 * Inserts all of the elements in the specified collection into this list at the specified position (optional operation). Shifts the element currently at that
	 * position (if any) and any subsequent elements to the right (increases their indices). The new elements will appear in this list in the order that they are
	 * returned by the specified collection's iterator. The behavior of this operation is unspecified if the specified collection is modified while the operation
	 * is in progress. (Note that this will occur if the specified collection is this list, and it's nonempty.)
	 *
	 * @param index index at which to insert first element from the specified collection.
	 * @param c elements to be inserted into this list.
	 * @return <tt>true</tt> if this list changed as a result of the call.
	 * 
	 * @throws UnsupportedOperationException if the <tt>addAll</tt> method is not supported by this list.
	 * @throws ClassCastException if the class of one of elements of the specified collection prevents it from being added to this list.
	 * @throws NullPointerException if the specified collection contains one or more null elements and this list does not support null elements, or if the
	 *           specified collection is <tt>null</tt>.
	 * @throws IllegalArgumentException if some aspect of one of elements of the specified collection prevents it from being added to this list.
	 * @throws IndexOutOfBoundsException if the index is out of range (index &lt; 0 || index &gt; size()).
	 */
	public boolean addAll(int index, Collection<? extends E> c) {
		synchronized(mutex) {
			return list.addAll(index, c);
		}
	}

	// Positional Access Operations

	/**
	 * Returns the element at the specified position in this list.
	 *
	 * @param index index of element to return.
	 * @return the element at the specified position in this list.
	 * 
	 * @throws IndexOutOfBoundsException if the index is out of range (index &lt; 0 || index &gt;= size()).
	 */
	public E get(int index) {
		synchronized(mutex) {
			return list.get(index);
		}
	}

	/**
	 * Replaces the element at the specified position in this list with the specified element (optional operation).
	 *
	 * @param index index of element to replace.
	 * @param element element to be stored at the specified position.
	 * @return the element previously at the specified position.
	 * 
	 * @throws UnsupportedOperationException if the <tt>set</tt> method is not supported by this list.
	 * @throws ClassCastException if the class of the specified element prevents it from being added to this list.
	 * @throws NullPointerException if the specified element is null and this list does not support null elements.
	 * @throws IllegalArgumentException if some aspect of the specified element prevents it from being added to this list.
	 * @throws IndexOutOfBoundsException if the index is out of range (index &lt; 0 || index &gt;= size()).
	 */
	public E set(int index, E element) {
		synchronized(mutex) {
			return list.set(index, element);
		}
	}

	/**
	 * Inserts the specified element at the specified position in this list (optional operation). Shifts the element currently at that position (if any) and any
	 * subsequent elements to the right (adds one to their indices).
	 *
	 * @param index index at which the specified element is to be inserted.
	 * @param element element to be inserted.
	 * 
	 * @throws UnsupportedOperationException if the <tt>add</tt> method is not supported by this list.
	 * @throws ClassCastException if the class of the specified element prevents it from being added to this list.
	 * @throws NullPointerException if the specified element is null and this list does not support null elements.
	 * @throws IllegalArgumentException if some aspect of the specified element prevents it from being added to this list.
	 * @throws IndexOutOfBoundsException if the index is out of range (index &lt; 0 || index &gt; size()).
	 */
	public void add(int index, E element) {
		synchronized(mutex) {
			list.add(index, element);
		}
	}

	/**
	 * Removes the element at the specified position in this list (optional operation). Shifts any subsequent elements to the left (subtracts one from their
	 * indices). Returns the element that was removed from the list.
	 *
	 * @param index the index of the element to removed.
	 * @return the element previously at the specified position.
	 * 
	 * @throws UnsupportedOperationException if the <tt>remove</tt> method is not supported by this list.
	 * @throws IndexOutOfBoundsException if the index is out of range (index &lt; 0 || index &gt;= size()).
	 */
	public E remove(int index) {
		synchronized(mutex) {
			return list.remove(index);
		}
	}

	// Search Operations

	/**
	 * Returns the index in this list of the first occurrence of the specified element, or -1 if this list does not contain this element. More formally, returns
	 * the lowest index <tt>i</tt> such that <tt>(o==null ? get(i)==null : o.equals(get(i)))</tt>, or -1 if there is no such index.
	 *
	 * @param o element to search for.
	 * @return the index in this list of the first occurrence of the specified element, or -1 if this list does not contain this element.
	 * @throws ClassCastException if the type of the specified element is incompatible with this list (optional).
	 * @throws NullPointerException if the specified element is null and this list does not support null elements (optional).
	 */
	public int indexOf(Object o) {
		synchronized(mutex) {
			return list.indexOf(o);
		}
	}

	/**
	 * Returns the index in this list of the last occurrence of the specified element, or -1 if this list does not contain this element. More formally, returns
	 * the highest index <tt>i</tt> such that <tt>(o==null ? get(i)==null : o.equals(get(i)))</tt>, or -1 if there is no such index.
	 *
	 * @param o element to search for.
	 * @return the index in this list of the last occurrence of the specified element, or -1 if this list does not contain this element.
	 * @throws ClassCastException if the type of the specified element is incompatible with this list (optional).
	 * @throws NullPointerException if the specified element is null and this list does not support null elements (optional).
	 */
	public int lastIndexOf(Object o) {
		synchronized(mutex) {
			return list.lastIndexOf(o);
		}
	}

	// List Iterators

	/**
	 * Returns a list iterator of the elements in this list (in proper sequence).
	 *
	 * @return a list iterator of the elements in this list (in proper sequence).
	 */
	public ListIterator<E> listIterator() {
		synchronized(mutex) {
			return list.listIterator();
		}
	}

	/**
	 * Returns a list iterator of the elements in this list (in proper sequence), starting at the specified position in this list. The specified index indicates
	 * the first element that would be returned by an initial call to the <tt>next</tt> method. An initial call to the <tt>previous</tt> method would return the
	 * element with the specified index minus one.
	 *
	 * @param index index of first element to be returned from the list iterator (by a call to the <tt>next</tt> method).
	 * @return a list iterator of the elements in this list (in proper sequence), starting at the specified position in this list.
	 * @throws IndexOutOfBoundsException if the index is out of range (index &lt; 0 || index &gt; size()).
	 */
	public ListIterator<E> listIterator(int index) {
		synchronized(mutex) {
			return list.listIterator(index);
		}
	}

	// View

	/**
	 * Returns a view of the portion of this list between the specified <tt>fromIndex</tt>, inclusive, and <tt>toIndex</tt>, exclusive. (If <tt>fromIndex</tt> and
	 * <tt>toIndex</tt> are equal, the returned list is empty.) The returned list is backed by this list, so non-structural changes in the returned list are
	 * reflected in this list, and vice-versa. The returned list supports all of the optional list operations supported by this list.
	 * <p>
	 *
	 * This method eliminates the need for explicit range operations (of the sort that commonly exist for arrays). Any operation that expects a list can be used
	 * as a range operation by passing a subList view instead of a whole list. For example, the following idiom removes a range of elements from a list:
	 * 
	 * <pre>
	 * list.subList(from, to).clear();
	 * </pre>
	 * 
	 * Similar idioms may be constructed for <tt>indexOf</tt> and <tt>lastIndexOf</tt>, and all of the algorithms in the <tt>Collections</tt> class can be applied
	 * to a subList.
	 * <p>
	 *
	 * The semantics of the list returned by this method become undefined if the backing list (i.e., this list) is <i>structurally modified</i> in any way other
	 * than via the returned list. (Structural modifications are those that change the size of this list, or otherwise perturb it in such a fashion that
	 * iterations in progress may yield incorrect results.)
	 *
	 * @param fromIndex low endpoint (inclusive) of the subList.
	 * @param toIndex high endpoint (exclusive) of the subList.
	 * @return a view of the specified range within this list.
	 * 
	 * @throws IndexOutOfBoundsException for an illegal endpoint index value (fromIndex &lt; 0 || toIndex &gt; size || fromIndex &gt; toIndex).
	 */
	public List<E> subList(int fromIndex, int toIndex) {
		synchronized(mutex) {
			return list.subList(fromIndex, toIndex);
		}
	}

}
