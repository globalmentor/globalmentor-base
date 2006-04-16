package com.garretwilson.util;

import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;

import static com.garretwilson.lang.IntegerUtilities.*;
import static com.garretwilson.lang.ObjectUtilities.*;

/**A default list iterator that can iterate over given list.
@param <E> The type of element contained in the list.
@author Garret Wilson
*/
public class DefaultListIterator<E> implements ListIterator<E>
{

	/**The list over which to iterate.*/
	private final List<E> list;

	/**The next index to iterate.*/
	private int nextIndex;

	/**The last index iterated in time, or -1 if no index has been retrieved.
	This is not the same as the previous index in relation to the current index.
	*/
	private int lastIndex=-1;

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
		this.list=checkInstance(list, "List cannot be null.");
		this.nextIndex=checkIndexBounds(index, 0, list.size()+1);	//allow for the next index being the actual size of the list
	}

	/**@return <code>true</code> if the list iterator has more elements when traversing the list in the forward direction.*/
	public boolean hasNext()
	{
		return nextIndex<list.size();	//return whether we have a valid next index
	}

	/**Returns the next element in the list.
	@return The next element in the list.
	@exception NoSuchElementException if the iteration has no next element.
	*/
	public E next()
	{
		if(hasNext())	//if we have a next item
		{
			final E item=list.get(nextIndex);	//get the item at the next index
			lastIndex=nextIndex;	//show the last index used
			++nextIndex;	//the next index will be one more
			return item;	//return the next item
		}
		else	//if we have no next item
		{
			throw new NoSuchElementException("No element at index "+nextIndex);
		}
	}

	/**@return <code>true</code> if the list iterator has more elements when traversing the list in the reverse direction.*/
	public boolean hasPrevious()
	{
		return nextIndex>0;	//return whether the next index is above the first index
	}

	/**Returns the previous element in the list.
	@return The previous element in the list.
	@exception NoSuchElementException if the iteration has no previous element.
	*/
	public E previous()
	{
		if(hasPrevious())	//if we have a previous item
		{
			--nextIndex;	//move the next index back one
			final E item=list.get(nextIndex);	//get the item at the new next index, which is the previous item to where we were
			lastIndex=nextIndex;	//show the last index used
			return item;	//return the previous item
		}
		else	//if we have no previous item
		{
			throw new NoSuchElementException("No element at index "+(nextIndex-1));
		}			
	}

	/**@return The index of the element that would be returned by a subsequent call to {@link #next()}, or list size if list iterator is at end of list.*/
	public int nextIndex()
	{
		return nextIndex;	//return the next index
	}

	/**@return The index of the element that would be returned by a subsequent call to {@link #previous()}, or -1 if list iterator is at beginning of list.*/ 
	public int previousIndex()
	{
		return nextIndex-1;	//return the previous index
	}

	/**Removes from the list the last element that was returned by {@link #next()} or {@link #previous()}.
	@exception IllegalStateException neither {@link #next()} nor {@link #previous()} have been called,
		or {@link #remove()} or {@link #add(Object)} have been called after the last call to {@link #next()} or {@link #previous()}.
	*/
	public void remove()
	{
		if(lastIndex>0)	//if there is a last index
		{
			list.remove(lastIndex);	//remove the item at the last index
			lastIndex=-1;	//there is no longer a last index
			if(nextIndex>lastIndex)	//if this affects the next index
			{
				--nextIndex;	//back up the next index by one
			}
		}
		else	//if there is no last index
		{
			throw new IllegalStateException("No element to remove.");
		}
	}

	/**Replaces the last element returned by {@link #next()} or {@link #previous()} with the specified element.
	@param object The element with which to replace the last element returned by {@link #next()} or {@link #previous()}.
	@exception ClassCastException if the class of the specified element prevents it from being added to this list.
	@exception IllegalArgumentException if some aspect of the specified element prevents it from being added to this list.
	@exception IllegalStateException neither {@link #next()} nor {@link #previous()} have been called,
		or {@link #remove()} or {@link #add(Object)} have been called after the last call to {@link #next()} or {@link #previous()}.
	 */
	public void set(final E object)
	{
		if(lastIndex>0)	//if there is a last index
		{
			list.set(lastIndex, object);	//set the item at the last index
		}
		else	//if there is no last index
		{
			throw new IllegalStateException("No element to set.");
		}			
	}

	/**Inserts the specified element into the list.
	 *   The
     * element is inserted immediately before the next element that would be
     * returned by <tt>next</tt>, if any, and after the next element that
     * would be returned by <tt>previous</tt>, if any.  (If the list contains
     * no elements, the new element becomes the sole element on the list.)
     * The new element is inserted before the implicit cursor: a subsequent
     * call to <tt>next</tt> would be unaffected, and a subsequent call to
     * <tt>previous</tt> would return the new element.  (This call increases
     * by one the value that would be returned by a call to <tt>nextIndex</tt>
     * or <tt>previousIndex</tt>.)
     *
	@param object The element to insert.
	@exception ClassCastException if the class of the specified element prevents it from being added to this list.
	@exception IllegalArgumentException if some aspect of this element prevents it from being added to this list.
	*/
	public void add(E object)
	{
		list.add(nextIndex, object);	//add the object at the next position in the list
		++nextIndex;	//adjust the next index
		lastIndex=-1;	//there is no longer a last index, so remove() and set() will not be allowed to work
	}

}
