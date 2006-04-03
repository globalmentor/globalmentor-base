package com.garretwilson.util;

import java.util.*;

import static com.garretwilson.lang.ObjectUtilities.*;

/**An iterator backed by an enumeration. An enumeration adapter.
This class also serves as an enumeration decorator by implementing the {@link Enumeration} interface.
Element removal is not supported.
@param <E> The type of element being iterated.
@author Garret Wilson
*/
public class EnumerationIterator<E> implements Iterator<E>, Enumeration<E>
{

	/**The adapted enumeration.*/
	private final Enumeration<E> enumeration;

		/**@return The adapted enumeration.*/
		protected Enumeration<E> getEnumeration() {return enumeration;}

	/**Enumeration constructor.
	@param enumeration The adapted enumeration.
	@exception NullPointerException if the given enumeration is <code>null</code>.
	*/
	public EnumerationIterator(final Enumeration<E> enumeration)
	{
		this.enumeration=checkInstance(enumeration, "Enumeration cannot be null.");		
	}

	/**@return <code>true</code> if the iteration has more elements.*/
	public boolean hasNext()
	{
		return getEnumeration().hasMoreElements();	//return whether the enumeration has more elements
	}

	/**@return The next element in the iteration.
	@exception NoSuchElementException if the iteration has no more elements.
	*/
	public E next()
	{
		return getEnumeration().nextElement();	//return the next element in the enumeration
	}

	/**Removes from the underlying collection the last element returned by the iterator.
	This implementation throws an exception, as removal is not supported.
	@exception UnsupportedOperationException if the <code>remove</code> operation is not supported by this iterator.
	*/
	public void remove()
	{
		throw new UnsupportedOperationException("An enumeration iterator does not support removal.");
	}

	/**@return <code>true</code> if and only if this enumeration object contains at least one more element to provide; <code>false</code> otherwise.
	This implementation delegates to {@link #hasNext()}.
	*/
  public boolean hasMoreElements()
  {
  	return hasNext();	//delegate to the iterator version
  }

  /**@return The next element of this enumeration.
	This implementation delegates to {@link #next()}.
	@exception  NoSuchElementException if no more elements exist.
	*/
	public E nextElement()
	{
		return next();	//delegate to the iterator version
	}

}
