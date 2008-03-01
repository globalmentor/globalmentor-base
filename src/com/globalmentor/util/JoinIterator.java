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

import java.util.*;

/**An iterator that joins the contents of two iterators.
This class also implements the {@link Enumeration} interface.
Element removal is not supported.
@param <E> The type of element being iterated.
@author Garret Wilson
*/
public class JoinIterator<E> implements Iterator<E>, Enumeration<E>	//TODO refactor to accept a vararg of iterators
{
	/**The first iterator, or <code>null</code> if the first iterator has been exhausted.*/
	private Iterator<E> iterator1;

	/**The second iterator, or <code>null</code> if there is no second iterator.*/
	private Iterator<E> iterator2;

	/**Iterator constructor.
	@param iterator1 The first iterator, or <code>null</code> if there is no first iterator.
	@param iterator2 The second iterator, containing the elements to use when the first iterator is exhausted, or <code>null</code> if there is no second iterator.
	@exception NullPointerException if one of the given iterators is null.
	*/
	public JoinIterator(final Iterator<E> iterator1, final Iterator<E> iterator2)
	{
		this.iterator1=iterator1;
		this.iterator2=iterator2;
	}

	/**@return <code>true</code> if the iteration has more elements.*/
	public boolean hasNext()
	{
  	if(iterator1!=null)	//if we haven't exhausted the first iterator
  	{
  		if(iterator1.hasNext())	//if the iterator has another element
  		{
  			return true;	//show that we have another element
  		}
  		else	//if the first iterator has run out
  		{
  			iterator1=null;	//don't use the first iterator anymore
  		}
  	}
 		return iterator2!=null ? iterator2.hasNext() : null;	//see if the second iterator has more elements, if there is a second iterator
	}

	/**@return The next element in the iteration.
	@exception NoSuchElementException if the iteration has no more elements.
	*/
	public E next()
	{
		if(hasNext())	//if we have elements (this will determine which iterator to use)
		{
			if(iterator1!=null)	//if there is a first iterator
			{
				return iterator1.next();	//get the iterator from the first iterator
			}
			else if(iterator2!=null)	//if there is a second iterator
			{
				return iterator2.next();	//get the iterator from the second iterator
			}
		}
		throw new NoSuchElementException();
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
