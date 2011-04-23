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

package com.globalmentor.collections.iterators;

import java.util.*;

import static com.globalmentor.java.Objects.*;

/**An iterator that wraps an existing iterator, providing access through the {@link Iterator} interface.
@author Garret Wilson
*/
public class IteratorDecorator<E> implements Iterator<E>
{

	/**The iterator this class decorates.*/
	private final Iterator<E> iterator;

	/**@return The iterator this class decorates.*/
	protected Iterator<E> getIterator() {return iterator;}
	
	/**Iterator constructor.
	@param iterator The iterator this iterator should decorate.
	@exception NullPointerException if the given iterator is <code>null</code>.
	*/
	public IteratorDecorator(final Iterator<E> iterator)
	{
		this.iterator=checkInstance(iterator, "Iterator cannot be null");	//save the iterator
	}

	/**{@inheritDoc}*/
	public boolean hasNext() {return getIterator().hasNext();}

	/**{@inheritDoc}*/
	public E next() {return getIterator().next();}

	/**{@inheritDoc}*/
	public void remove() {getIterator().remove();}

}
