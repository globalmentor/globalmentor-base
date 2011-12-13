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

import com.globalmentor.model.Filter;

/**An iterator that filters the results of an existing iterator.
<p>The iterator allows a filter to be applied to the objects it returns, only
	returning those that pass through the given filter, if any.</p>
<p>The iterator can also restrict the number of returned elements to a
	given maximum.</p>
@author Garret Wilson
*/
public class CountedFilterIterator<E> extends IteratorDecorator<E>	//TODO refactor by extending FilterIterator and adding count and max count
{

	/**The number of elements to return, or -1 if all of the elements in the
		proxied iterator should be returned.
	*/
	protected final int maxCount;

	/**The total number of integers returned.*/
	protected int count;

	/**The filter used to exclude items from the iterator.*/
	private Filter<E> filter;

		/**@return The filter used to exclude items from the iterator.*/
		public Filter<E> getFilter() {return filter;}

		/**Sets the filter used to exclude items from the iterator.
		@param filter The new filter to use, or <code>null</code> if there should
			be no filtering.
		*/
		public void setFilter(final Filter<E> filter) {this.filter=filter;}

	/**The constant object representing no next object available.*/
	protected final static Object NO_NEXT_OBJECT=new Object();

	/**The value that has been retrieved and has passed the filter and is waiting
		to be returned, or <code>NO_NEXT_OBJECT</code> if there is no primed next
		object.
	*/
	protected Object primedNext;

	/**Iterator constructor.
	@param iterator The iterator this iterator should proxy.
	*/
	public CountedFilterIterator(final Iterator<E> iterator)
	{
		this(iterator, -1);	//construct a filter iterator that uses all the elements in the proxied iterator
	}

	/**Iterator and count constructor.
	@param iterator The iterator this iterator should proxy.
	@param maxCount The number of elements to return, or -1 if all of the elements
		in the proxied iterator should be returned.
	*/
	public CountedFilterIterator(final Iterator<E> iterator, final int maxCount)
	{
		super(iterator);	//construct the parent class
		primedNext=NO_NEXT_OBJECT;	//show that we have no primed next value
		this.maxCount=maxCount;	//set the maximum number of elements to return
	}

	/**@return <code>true</code> if the iteration has more elements.*/
	public boolean hasNext()
	{
			//determine if we haven't reached the maximum count or there is no maximum count
			//then prime another value if we can
		return (maxCount<0 || count<maxCount) && primeNext();	//if there are more element to retrieve, and one is primed and ready
	}

	/**@return The next element in the iteration.
	@exception NoSuchElementException Thrown if the iteration has no more elements.
	*/
	@SuppressWarnings("unchecked")
	public E next()
	{
		if(hasNext())	//if there is a next object waiting for us
		{
			final Object next=primedNext;	//get the next object primed and waiting
			primedNext=NO_NEXT_OBJECT;	//show that we've used the primed next object
			return (E)next;	//return the next object we found
		}
		else	//if we've ran out of objects
		{
			throw new NoSuchElementException();
		}
	}

	/**Retrieves the next object and stores it locally to return from the next
		call to <code>next()</code>. If an object is already primed, no action
		occurs.
	@return <code>true</code> if there is another integer left to retrieve.
	*/
	protected boolean primeNext()
	{
		if(primedNext==NO_NEXT_OBJECT)	//if there is no primed next object
		{
			primedNext=getNext();	//get the next object
		}
		return primedNext!=NO_NEXT_OBJECT;	//return whether or not there now is a primed next object		
	}

	/**@return The next random integer in the iteration, or
		{@link #NO_NEXT_OBJECT} if the iteration has no more elements.
	*/
	protected Object getNext()
	{
		final Filter<E> filter=getFilter();	//get our filter, if there is one
		while(getIterator().hasNext())	//while there are available items in the proxied iterator
		{
			final E next=getIterator().next();	//get the next element from the iterator
			final boolean isPass=filter==null || filter.isPass(next);	//see if our next element passes
			if(isPass)	//if this item isn't filtered out
			{
				return next;	//return the object we found
			}
		}
		return NO_NEXT_OBJECT;	//if we've ran out of objects in our underlying iterator, show that there is no next object
	}

}
