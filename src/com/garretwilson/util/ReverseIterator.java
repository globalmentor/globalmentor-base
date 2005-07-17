package com.garretwilson.util;

import static com.garretwilson.lang.ObjectUtilities.checkNull;

import java.util.*;

/**An iterator that wraps an existing list iterator and provides its elements in reverse order.
In most use cases, the decorated list iterator should already be at the end of the list.
@author Garret Wilson
*/
public class ReverseIterator<E> implements Iterator<E>
{

	/**The list iterator this class decorates.*/
	protected final ListIterator<E> listIterator;

	/**List iterator constructor.
	In most use cases, the decorated list iterator should already be at the end of the list.
	@param listIterator The list iterator this iterator should decorate.
	@exception NullPointerException if the given iterator is <code>null</code>.
	*/
	public ReverseIterator(final ListIterator<E> listIterator)
	{
		this.listIterator=checkNull(listIterator, "Iterator cannot be null");	//save the iterator
	}

	/**Returns <code>true</code> if the iteration has more elements.
	This implementation delegates to the decorated iterator's opposite method, {@link ListIterator#hasPrevious()}.
	@return <code>true</code> if the iterator has more elements, which for this implementation is in the reverse direction.
	 */
	public boolean hasNext() {return listIterator.hasPrevious();}

	/**Returns the next element in the iteration.
	This implementation delegates to the decorated iterator's opposite method, {@link ListIterator#previous()}.
	@return The next element in the iteration, which for this implementation is in the reverse direction.
	@exception NoSuchElementException iteration has no more elements.
	*/
	public E next() {return listIterator.previous();}

	/**
	 * 
	 * Removes from the underlying collection the last element returned by the
	 * iterator (optional operation).  This method can be called only once per
	 * call to <tt>next</tt>.  The behavior of an iterator is unspecified if
	 * the underlying collection is modified while the iteration is in
	 * progress in any way other than by calling this method.
	 *
	 * @exception UnsupportedOperationException if the <tt>remove</tt>
	 *		  operation is not supported by this Iterator.
     
	 * @exception IllegalStateException if the <tt>next</tt> method has not
	 *		  yet been called, or the <tt>remove</tt> method has already
	 *		  been called after the last call to the <tt>next</tt>
	 *		  method.
	 */
	public void remove() {listIterator.remove();}

}
