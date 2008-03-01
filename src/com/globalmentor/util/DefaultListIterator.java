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

package com.globalmentor.util;

import java.util.List;

/**A default list iterator that can iterate over a given list.
@param <E> The type of element contained in the list.
@author Garret Wilson
*/
public class DefaultListIterator<E> extends AbstractListIterator<E, E>
{

	/**List constructor starting at the first index.
	@param list The list over which to iterate.
	@exception NullPointerException if the given list is <code>null</code>.
	*/
	public DefaultListIterator(final List<E> list)
	{
		this(list, 0);	//construct the class with a next index of the first available index
	}

	/**List and index constructor.
	@param list The list over which to iterate.
	@param index The index of first value to be returned from the list iterator (by a call to the {@link #next()} method).
	@exception NullPointerException if the given list is <code>null</code>.
	@exception IndexOutOfBoundsException if the index is out of range (<var>index</var> &lt; 0 || <var>index</var> &gt; <code>size()</code>).
	*/
	public DefaultListIterator(final List<E> list, final int index)
	{
		super(list, index);	//construct the parent class
		updateIncludedIndexes();	//initialize the iterator
	}

	/**Determines whether the item at the given index should be included.
	This version includes all indexes.
	@param index The index of the item to check.
	@return <code>true</code> if the item at the given index should be included in the iteration, else <code>false</code> if it should be ignored.
	*/
	protected boolean isIncluded(final int index)
	{
		return true;	//default to allowing all indexes
	}

	/**Retrieves an item representing the element at the given position in the list.
	@param index The list index
	@return An item representing the element at the given index in the list
	@exception IndexOutOfBoundsException if the index is out of range (<var>index</var> &lt; 0 || <var>index</var> &gt;= <code>size()</code>).
	*/
	protected E getItem(final int index)
	{
		return getList().get(index);	//return the item from the list
	}

	/**Sets the element at the given position in the list.
	@param index The list index
	@param item The item representing the element to be stored at the specified position.
	@return An item representing the element previously at the specified position.
	@exception UnsupportedOperationException if the {@link #set(Object)} operation is not supported by this list iterator.
	@exception IndexOutOfBoundsException if the index is out of range (<var>index</var> &lt; 0 || <var>index</var> &gt;= <code>size()</code>).
	*/
	protected E setItem(final int index, final E item)
	{
		return getList().set(index, item);	//set the item in the list
	}

	/**Inserts an element at the given position in the list.
	@param index The list index
	@param item The item representing the element to be inserted at the specified position.
	@exception UnsupportedOperationException if the {@link #add(Object)} operation is not supported by this list iterator.
	@exception IndexOutOfBoundsException if the index is out of range (<var>index</var> &lt; 0 || <var>index</var> &gt; <code>size()</code>).
	*/
	protected void addItem(final int index, final E item)
	{
		getList().add(index, item);	//set the item in the list
	}

}
