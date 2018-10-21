/*
 * Copyright © 2011-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.io.IOException;
import java.util.*;

import static java.util.Objects.*;

import com.globalmentor.collections.iterators.*;
import com.globalmentor.java.Arrays;
import com.globalmentor.java.Objects;

import static com.globalmentor.java.Arrays.*;
import static com.globalmentor.java.Conditions.unexpected;
import static com.globalmentor.text.TextFormatter.formatList;

/**
 * A read-only list containing a single object.
 * 
 * @author Garret Wilson
 * 
 * @param <E> The type of object contained in the list.
 */
public class ObjectList<E> implements List<E>, ImmutableCollection<E> { //TODO refactor out ObjectCollection

	/** The object held in the list. */
	private E object;

	/**
	 * Object constructor.
	 * @param object The object to hold in the list.
	 * @throws NullPointerException if the given object is <code>null</code>.
	 */
	public ObjectList(final E object) {
		this.object = requireNonNull(object, "Object cannot be null.");
	}

	/** {@inheritDoc} */
	@Override
	public int size() {
		return 1;
	}

	/** {@inheritDoc} */
	@Override
	public boolean isEmpty() {
		return false;
	}

	/** {@inheritDoc} */
	@Override
	public boolean contains(Object o) {
		return object.equals(o);
	}

	/** {@inheritDoc} */
	@Override
	public Iterator<E> iterator() {
		return new ObjectIterator<E>(object);
	}

	/** {@inheritDoc} */
	@Override
	public Object[] toArray() {
		return new Object[] { object };
	}

	/** {@inheritDoc} */
	@Override
	@SuppressWarnings("unchecked")
	public <T> T[] toArray(T[] a) {
		a = Arrays.getArray(a, 1); //make sure our array is large enough
		a[0] = (T)object;
		if(a.length > 1) {
			a[1] = null;
		}
		return a;
	}

	/** {@inheritDoc} */
	@Override
	public boolean add(E e) {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	@Override
	public boolean remove(Object o) {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	@Override
	public boolean containsAll(Collection<?> c) {
		final int size = c.size();
		return size == 0 || (size == 1 && object.equals(c.iterator().next()));
	}

	/** {@inheritDoc} */
	@Override
	public boolean addAll(Collection<? extends E> c) {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	@Override
	public boolean addAll(int index, Collection<? extends E> c) {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	@Override
	public boolean retainAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	@Override
	public boolean removeAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	@Override
	public void clear() {
		throw new UnsupportedOperationException();
	}

	/**
	 * Returns the element at the specified position in this list.
	 *
	 * @param index index of the element to return
	 * @return the element at the specified position in this list
	 * @throws IndexOutOfBoundsException if the index is out of range (<code>index &lt; 0 || index &gt;= size()</code>)
	 */
	@Override
	public E get(final int index) {
		if(index != 0) {
			throw new IndexOutOfBoundsException("Index " + index + " is out of bounds for a single-object immutable list.");
		}
		return object;
	}

	/**
	 * Replaces the element at the specified position in this list with the specified element (optional operation).
	 *
	 * @param index index of the element to replace
	 * @param element element to be stored at the specified position
	 * @return the element previously at the specified position
	 * @throws UnsupportedOperationException if the <code>set</code> operation is not supported by this list
	 * @throws ClassCastException if the class of the specified element prevents it from being added to this list
	 * @throws NullPointerException if the specified element is null and this list does not permit null elements
	 * @throws IllegalArgumentException if some property of the specified element prevents it from being added to this list
	 * @throws IndexOutOfBoundsException if the index is out of range (<code>index &lt; 0 || index &gt;= size()</code>)
	 */
	@Override
	public E set(final int index, final E element) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Inserts the specified element at the specified position in this list (optional operation). Shifts the element currently at that position (if any) and any
	 * subsequent elements to the right (adds one to their indices).
	 *
	 * @param index index at which the specified element is to be inserted
	 * @param element element to be inserted
	 * @throws UnsupportedOperationException if the <code>add</code> operation is not supported by this list
	 * @throws ClassCastException if the class of the specified element prevents it from being added to this list
	 * @throws NullPointerException if the specified element is null and this list does not permit null elements
	 * @throws IllegalArgumentException if some property of the specified element prevents it from being added to this list
	 * @throws IndexOutOfBoundsException if the index is out of range (<code>index &lt; 0 || index &gt; size()</code>)
	 */
	@Override
	public void add(final int index, final E element) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Removes the element at the specified position in this list (optional operation). Shifts any subsequent elements to the left (subtracts one from their
	 * indices). Returns the element that was removed from the list.
	 *
	 * @param index the index of the element to be removed
	 * @return the element previously at the specified position
	 * @throws UnsupportedOperationException if the <code>remove</code> operation is not supported by this list
	 * @throws IndexOutOfBoundsException if the index is out of range (<code>index &lt; 0 || index &gt;= size()</code>)
	 */
	@Override
	public E remove(final int index) {
		throw new UnsupportedOperationException();
	}

	// Search Operations

	/**
	 * Returns the index of the first occurrence of the specified element in this list, or -1 if this list does not contain the element. More formally, returns
	 * the lowest index <code>i</code> such that <code>(o==null&nbsp;?&nbsp;get(i)==null&nbsp;:&nbsp;o.equals(get(i)))</code>, or -1 if there is no such index.
	 *
	 * @param o element to search for
	 * @return the index of the first occurrence of the specified element in this list, or -1 if this list does not contain the element
	 * @throws ClassCastException if the type of the specified element is incompatible with this list (optional)
	 * @throws NullPointerException if the specified element is null and this list does not permit null elements (optional)
	 */
	@Override
	public int indexOf(final Object o) {
		return Objects.equals(object, o) ? 0 : -1;
	}

	/**
	 * Returns the index of the last occurrence of the specified element in this list, or -1 if this list does not contain the element. More formally, returns the
	 * highest index <code>i</code> such that <code>(o==null&nbsp;?&nbsp;get(i)==null&nbsp;:&nbsp;o.equals(get(i)))</code>, or -1 if there is no such index.
	 *
	 * @param o element to search for
	 * @return the index of the last occurrence of the specified element in this list, or -1 if this list does not contain the element
	 * @throws ClassCastException if the type of the specified element is incompatible with this list (optional)
	 * @throws NullPointerException if the specified element is null and this list does not permit null elements (optional)
	 */
	@Override
	public int lastIndexOf(final Object o) {
		return indexOf(o);
	}

	// List Iterators

	/**
	 * Returns a list iterator over the elements in this list (in proper sequence).
	 *
	 * @return a list iterator over the elements in this list (in proper sequence)
	 */
	@Override
	public ListIterator<E> listIterator() {
		return new ObjectListIterator<E>(object);
	}

	/**
	 * Returns a list iterator of the elements in this list (in proper sequence), starting at the specified position in this list. The specified index indicates
	 * the first element that would be returned by an initial call to {@link ListIterator#next next}. An initial call to {@link ListIterator#previous previous}
	 * would return the element with the specified index minus one.
	 *
	 * @param index index of first element to be returned from the list iterator (by a call to the <code>next</code> method)
	 * @return a list iterator of the elements in this list (in proper sequence), starting at the specified position in this list
	 * @throws IndexOutOfBoundsException if the index is out of range (<code>index &lt; 0 || index &gt; size()</code>)
	 */
	@Override
	public ListIterator<E> listIterator(final int index) {
		if(index < 0 || index > 1) {
			throw new IndexOutOfBoundsException("Index " + index + " is out of bounds for a single-object immutable list.");
		}
		final ListIterator<E> listIterator = new ObjectListIterator<E>(object); //create a list iterator
		if(index == 1) {
			listIterator.next(); //go to the end so that the API contract will be met
		}
		return listIterator;
	}

	// View

	/**
	 * Returns a view of the portion of this list between the specified <code>fromIndex</code>, inclusive, and <code>toIndex</code>, exclusive. (If <code>fromIndex</code> and
	 * <code>toIndex</code> are equal, the returned list is empty.) The returned list is backed by this list, so non-structural changes in the returned list are
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
	 * Similar idioms may be constructed for <code>indexOf</code> and <code>lastIndexOf</code>, and all of the algorithms in the <code>Collections</code> class can be applied
	 * to a subList.
	 * <p>
	 *
	 * The semantics of the list returned by this method become undefined if the backing list (i.e., this list) is <i>structurally modified</i> in any way other
	 * than via the returned list. (Structural modifications are those that change the size of this list, or otherwise perturb it in such a fashion that
	 * iterations in progress may yield incorrect results.)
	 *
	 * @param fromIndex low endpoint (inclusive) of the subList
	 * @param toIndex high endpoint (exclusive) of the subList
	 * @return a view of the specified range within this list
	 * @throws IndexOutOfBoundsException for an illegal endpoint index value (<code>fromIndex &lt; 0 || toIndex &gt; size ||
	 *         fromIndex &gt; toIndex</code>)
	 */
	@Override
	public List<E> subList(final int fromIndex, final int toIndex) {
		final int length = checkIndexRange(1, fromIndex, toIndex); //check the range and get the length of the range
		return length == 1 ? this : java.util.Collections.<E> emptyList(); //since the range is OK, there will only be one or no elements in the range
	}

	/** {@inheritDoc} */
	public boolean equals(Object o) {
		if(!(o instanceof List)) {
			return false;
		}
		final List<?> list = (List<?>)o;
		return list.size() == 1 && object.equals(list.get(0));
	}

	/** {@inheritDoc} */
	public int hashCode() {
		return object.hashCode();
	}

	@Override
	public String toString() {
		try {
			final StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.append('[');
			formatList(stringBuilder, this);
			stringBuilder.append(']');
			return stringBuilder.toString();
		} catch(final IOException ioException) {
			throw unexpected(ioException);
		}
	}

}
