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

import static java.util.Objects.*;

import static com.globalmentor.java.Conditions.*;

/**
 * A default list iterator that can iterate over a given list. The iterator provides a way to return only a subset of list items by overriding the
 * {@link #isIncluded(int)} method. The results of this iterator are undefined if the underlying list is modified. Every concrete subclass must call
 * {@link #updateIncludedIndexes()} after superclass initialization has taken place.
 * @param <I> The type of item returned by the iterator, which may or may not be the type of element contained in the list.
 * @param <E> The type of element contained in the list.
 * @author Garret Wilson
 */
public abstract class AbstractListIterator<I, E> implements ListIterator<I> {

	/** The list over which to iterate. */
	private final List<E> list;

	/** @return The list over which to iterate. */
	protected final List<E> getList() {
		return list;
	}

	/** The next index to iterate, or {@link List#size()} if there is no next item. */
	private int nextIndex;

	/** The previous index to iterate, or -1 if there is no previous item. */
	private int previousIndex;

	/**
	 * The last index iterated in time, or -1 if no index has been retrieved. This is not the same as the previous index in relation to the current index.
	 */
	private int lastIndex = -1;

	/**
	 * List constructor starting at the first index.
	 * @param list The list over which to iterate.
	 * @throws NullPointerException if the given list is <code>null</code>.
	 */
	public AbstractListIterator(final List<E> list) {
		this(list, 0); //construct the class with a next index of the first available index
	}

	/**
	 * List and index constructor.
	 * @param list The list over which to iterate.
	 * @param index The index of first value to be returned from the list iterator (by a call to the {@link #next()} method).
	 * @throws NullPointerException if the given list is <code>null</code>.
	 * @throws IndexOutOfBoundsException if the index is out of range (<var>index</var> &lt; 0 || <var>index</var> &gt; <code>size()</code>).
	 */
	public AbstractListIterator(final List<E> list, final int index) {
		this.list = requireNonNull(list, "List cannot be null.");
		final int size = list.size(); //get the size of the list
		nextIndex = checkIndexBounds(index, size + 1); //make sure the next index is within the list or at the size of the list 
	}

	/**
	 * Ensures that the current next and previous indexes are included. This method must be called whenever the indexes need to be recalculated, such as after the
	 * iterator is initialized.
	 */
	protected void updateIncludedIndexes() {
		if(nextIndex < list.size() && !isIncluded(nextIndex)) { //if the next index is on a non-included index
			nextIndex = getNextIncludedIndex(nextIndex); //advance to the next included index
		}
		previousIndex = getPreviousIncludedIndex(nextIndex); //calculate the previous index		
	}

	/**
	 * Determines the next index to be included after the given index.
	 * @param index The index, which may be -1, before the next index to be included.
	 * @return The index of the next item to be included after the given index, or {@link List#size()} if there is no next index.
	 * @see #isIncluded(int)
	 */
	protected int getNextIncludedIndex(int index) {
		final int size = list.size(); //get the size of the list
		do {
			++index; //advance to the next index
		} while(index < size && !isIncluded(index)); //keep advancing until we find an included index or we run out of indexes
		return index; //return the new next index
	}

	/**
	 * Determines the previous index to be included before the given index.
	 * @param index The index, which may be {@link List#size()}, after the previous index to be included.
	 * @return The index of the previous item to be included before the given index, or -1 if there is no previous index.
	 * @see #isIncluded(int)
	 */
	protected int getPreviousIncludedIndex(int index) {
		do {
			--index; //go to the previous index
		} while(index >= 0 && !isIncluded(index)); //keep retreating until we find an included index or we run out of indexes
		return index; //return the new previous index
	}

	/**
	 * Determines whether the item at the given index should be included.
	 * @param index The index of the item to check.
	 * @return <code>true</code> if the item at the given index should be included in the iteration, else <code>false</code> if it should be ignored.
	 */
	protected abstract boolean isIncluded(final int index);

	/** @return <code>true</code> if the list iterator has more elements when traversing the list in the forward direction. */
	public boolean hasNext() {
		return nextIndex < list.size(); //return whether we have a valid next index
	}

	/**
	 * Returns the next element in the list.
	 * @return The next element in the list.
	 * @throws NoSuchElementException if the iteration has no next element.
	 */
	public I next() {
		if(hasNext()) { //if we have a next item
			final I item = getItem(nextIndex); //get the item at the next index
			lastIndex = nextIndex; //show the last index used
			previousIndex = lastIndex; //the last index will also become our previous index
			nextIndex = getNextIncludedIndex(nextIndex); //find the next index
			return item; //return the next item
		} else { //if we have no next item
			throw new NoSuchElementException("No next element available.");
		}
	}

	/** @return <code>true</code> if the list iterator has more elements when traversing the list in the reverse direction. */
	public boolean hasPrevious() {
		return previousIndex >= 0; //return whether the previous index is valid
	}

	/**
	 * Returns the previous element in the list.
	 * @return The previous element in the list.
	 * @throws NoSuchElementException if the iteration has no previous element.
	 */
	public I previous() {
		if(hasPrevious()) { //if we have a previous item
			final I item = getItem(previousIndex); //get the item at the previous index
			lastIndex = previousIndex; //show the last index used
			nextIndex = lastIndex; //the last index will also become our nextindex
			previousIndex = getPreviousIncludedIndex(nextIndex); //find the previous index
			return item; //return the previous item
		} else { //if we have no previous item
			throw new NoSuchElementException("No previous element available.");
		}
	}

	/** @return The index of the element that would be returned by a subsequent call to {@link #next()}, or list size if list iterator is at end of list. */
	public int nextIndex() {
		return nextIndex; //return the next index
	}

	/** @return The index of the element that would be returned by a subsequent call to {@link #previous()}, or -1 if list iterator is at beginning of list. */
	public int previousIndex() {
		return previousIndex; //return the previous index
	}

	/**
	 * Removes from the list the last element that was returned by {@link #next()} or {@link #previous()}.
	 * @throws IllegalStateException neither {@link #next()} nor {@link #previous()} have been called, or {@link #remove()} or {@link #add(Object)} have been
	 *           called after the last call to {@link #next()} or {@link #previous()}.
	 */
	public void remove() {
		if(lastIndex >= 0) { //if there is a last index
			list.remove(lastIndex); //remove the item at the last index
			if(previousIndex == lastIndex) { //if the previous index was removed
				--nextIndex; //back up the next index by one
				previousIndex = getPreviousIncludedIndex(nextIndex); //recalculate the previous index
			} else if(nextIndex == lastIndex) { //if the next index was removed
				nextIndex = getNextIncludedIndex(previousIndex); //recalculate the next index; the previous index, which is before the next index, should not be affected
			}
			lastIndex = -1; //there is no longer a last index
		} else { //if there is no last index
			throw new IllegalStateException("No element to remove.");
		}
	}

	/**
	 * Replaces the last element returned by {@link #next()} or {@link #previous()} with the specified element.
	 * @param object The element with which to replace the last element returned by {@link #next()} or {@link #previous()}.
	 * @throws UnsupportedOperationException if the {@link #set(Object)} operation is not supported by this list iterator.
	 * @throws ClassCastException if the class of the specified element prevents it from being added to this list.
	 * @throws IllegalArgumentException if some aspect of the specified element prevents it from being added to this list.
	 * @throws IllegalStateException neither {@link #next()} nor {@link #previous()} have been called, or {@link #remove()} or {@link #add(Object)} have been
	 *           called after the last call to {@link #next()} or {@link #previous()}.
	 */
	public void set(final I object) {
		if(lastIndex >= 0) { //if there is a last index
			setItem(lastIndex, object); //set the item at the last index
			if(previousIndex == lastIndex) { //if the previous index was changed
				previousIndex = getPreviousIncludedIndex(nextIndex); //recalculate the previous index to ensure that the item is included
			} else if(nextIndex == lastIndex) { //if the next index was removed
				nextIndex = getNextIncludedIndex(previousIndex); //recalculate the next index to ensure that the item is included
			}
		} else { //if there is no last index
			throw new IllegalStateException("No element to set.");
		}
	}

	/**
	 * Inserts the specified element into the list. The element is inserted immediately before the next element that would be returned by {@link #next()}, if any,
	 * and after the next element that would be returned by {@link #previous()}, if any. (If the list contains no elements, the new element becomes the sole
	 * element on the list.) The new element is inserted before the implicit cursor: a subsequent call to {@link #next()} would be unaffected, and a subsequent
	 * call to {@link #previous()} would return the new element. (This call increases by one the value that would be returned by a call to {@link #nextIndex()} or
	 * {@link #previousIndex()}.)
	 * @param object The element to insert.
	 * @throws UnsupportedOperationException if the {@link #add(Object)} operation is not supported by this list iterator.
	 * @throws ClassCastException if the class of the specified element prevents it from being added to this list.
	 * @throws IllegalArgumentException if some aspect of this element prevents it from being added to this list.
	 */
	public void add(final I object) {
		addItem(nextIndex, object); //add the object at the next position in the list
		++nextIndex; //adjust the next index
		previousIndex = getPreviousIncludedIndex(nextIndex); //recalculate the previous index to ensure that the item is included
		lastIndex = -1; //there is no longer a last index, so remove() and set() will not be allowed to work
	}

	/**
	 * Retrieves an item representing the element at the given position in the list.
	 * @param index The list index
	 * @return An item representing the element at the given index in the list
	 * @throws IndexOutOfBoundsException if the index is out of range (<var>index</var> &lt; 0 || <var>index</var> &gt;= <code>size()</code>).
	 */
	protected abstract I getItem(final int index);

	/**
	 * Sets the element at the given position in the list.
	 * @param index The list index
	 * @param item The item representing the element to be stored at the specified position.
	 * @return An item representing the element previously at the specified position.
	 * @throws UnsupportedOperationException if the {@link #set(Object)} operation is not supported by this list iterator.
	 * @throws IndexOutOfBoundsException if the index is out of range (<var>index</var> &lt; 0 || <var>index</var> &gt;= <code>size()</code>).
	 */
	protected abstract I setItem(final int index, final I item);

	/**
	 * Inserts an element at the given position in the list.
	 * @param index The list index
	 * @param item The item representing the element to be inserted at the specified position.
	 * @throws UnsupportedOperationException if the {@link #add(Object)} operation is not supported by this list iterator.
	 * @throws IndexOutOfBoundsException if the index is out of range (<var>index</var> &lt; 0 || <var>index</var> &gt; <code>size()</code>).
	 */
	protected abstract void addItem(final int index, final I item);
}
