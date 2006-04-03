package com.garretwilson.util;

import java.util.*;
import static com.garretwilson.lang.ObjectUtilities.*;

/**An iterator that wraps an existing iterator, providing access through the <code>Iterator</code> interface.
@author Garret Wilson
*/
public class IteratorDecorator<E> implements Iterator<E>
{

	/**The iterator this class decorates.*/
	protected final Iterator<E> iterator;

	/**Iterator constructor.
	@param iterator The iterator this iterator should decorate.
	@exception NullPointerException if the given iterator is <code>null</code>.
	*/
	public IteratorDecorator(final Iterator<E> iterator)
	{
		this.iterator=checkInstance(iterator, "Iterator cannot be null");	//save the iterator
	}

	/**
	 * Returns <tt>true</tt> if the iteration has more elements. (In other
	 * words, returns <tt>true</tt> if <tt>next</tt> would return an element
	 * rather than throwing an exception.)
	 *
	 * @return <tt>true</tt> if the iterator has more elements.
	 */
	public boolean hasNext() {return iterator.hasNext();}

	/**
	 * Returns the next element in the iteration.
	 *
	 * @return the next element in the iteration.
	 * @exception NoSuchElementException iteration has no more elements.
	 */
	public E next() {return iterator.next();}

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
	public void remove() {iterator.remove();}

}
