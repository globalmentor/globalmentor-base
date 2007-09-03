package com.garretwilson.util;

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
