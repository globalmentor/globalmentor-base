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

/**An iterator that iterates a sequence of random iterators.
	The iterator has the ability to restrict its output to a particular range.
	A subset of the range can optionally be returned.
	This iterator can keep track of the numbers used and only return unused integers.
@author Garret Wilson
@see Integer
*/
public class RandomIntegerIterator implements Iterator<Integer>	//TODO maybe later make this generic for Number subclasses 
{

	/**The random number generator.*/
	protected final Random random; 

	/**The lower bound, inclusive, of the range to include.*/
	protected final int rangeMin;

	/**The upper bound, inclusive, of the range to include.*/
	protected final int rangeMax;

	/**The absolute positive length of the range of possible values.*/
	protected final int range;

	/**The number of integers to return, or -1 if an
		unlimited number of integers should be returned.
	*/
	protected final int maxCount;

	/**<code>true</code> if values should be repeated, else <code>false</code>.*/
	protected final boolean repeat;

	/**Create a new set to hold the integers excluded from the iterator.*/
	protected final SortedSet<Integer> excludedIntegerSortedSet;

	/**The total number of integers returned.*/
	protected int count;

	/**The filter used to exclude items from the iterator.*/
	private Filter<Integer> filter;

		/**@return The filter used to exclude items from the iterator.*/
		public Filter<Integer> getFilter() {return filter;}

		/*Sets the filter used to exclude items from the iterator.
		@param filter The new filter to use, or <code>null</code> if there should
			be no filtering.
		*/
		public void setFilter(final Filter<Integer> filter) {this.filter=filter;}

	/**The value that has been retrieved and has passed the filter and is waiting
		to be returned, or <code>null</code> if there is no primed next object.
	*/
	protected Integer primedNext;

	/**Shuffle constructor.
	Constructs an iterator that returns numbers between zero (inclusive) and
		the given maximum value (exclusive). Values returned will be
		0&le;<var>value</var>&lt;<code>maxValue</code>, and will not be repeated.
		All numbers will be returned.
	@param maxValue The upper bound, exclusive, of the values to return.
	*/
	public RandomIntegerIterator(final int maxValue)
	{
		this(new Random(), maxValue);
	}

	/**Shuffle constructor with random number generator.
	Constructs an iterator that returns numbers between zero (inclusive) and
		the given maximum value (exclusive). Values returned will be
		0&le;<var>value</var>&lt;<code>maxValue</code>, and will not be repeated.
		All numbers will be returned.
	@param random The random number generator.
	@param maxValue The upper bound, exclusive, of the values to return.
	*/
	public RandomIntegerIterator(final Random random, final int maxValue)
	{
		this(random, maxValue, maxValue);
	}

	/**Constructs an iterator that returns numbers between zero (inclusive) and
		the given maximum value (exclusive). Values returned will be
		0&le;<var>value</var>&lt;<code>maxValue</code>, and will not be repeated.
	@param maxValue The upper bound, exclusive, of the values to return.
	@param maxCount The number of integers to return, or -1 if an unlimited
		number of integers should be returned.
	@exception IllegalArgumentException Thrown if the given maximum count is
		greater than the allowed range.
	*/
	public RandomIntegerIterator(final int maxValue, final int maxCount)
	{
		this(new Random(), maxValue, maxCount);
	}

	/**Constructs an iterator that returns numbers between zero (inclusive) and
		the given maximum value (exclusive). Values returned will be
		0&le;<var>value</var>&lt;<code>maxValue</code>, and will not be repeated.
	@param random The random number generator.
	@param maxValue The upper bound, exclusive, of the values to return.
	@param maxCount The number of integers to return, or -1 if an unlimited
		number of integers should be returned.
	@exception IllegalArgumentException Thrown if the given maximum count is
		greater than the allowed range.
	*/
	public RandomIntegerIterator(final Random random, final int maxValue, final int maxCount)
	{
		this(random, maxValue, maxCount, false);
	}

	/**Constructs an iterator that returns numbers between zero (inclusive) and
		the given maximum value (exclusive). Values returned will be
		0&le;<var>value</var>&lt;<code>maxValue</code>.
	@param maxValue The upper bound, exclusive, of the values to return.
	@param maxCount The number of integers to return, or -1 if an unlimited
		number of integers should be returned.
	@param repeat <code>true</code> if values should be repeated, else <code>false</code>.
	@exception IllegalArgumentException Thrown if the given maximum count is
		greater than the allowed range.
	*/
	public RandomIntegerIterator(final int maxValue, final int maxCount, final boolean repeat)
	{
		this(new Random(), maxValue, maxCount, repeat);
	}

	/**Constructs an iterator that returns numbers between zero (inclusive) and
		the given maximum value (exclusive). Values returned will be
		0&le;<var>value</var>&lt;<code>maxValue</code>.
	@param random The random number generator.
	@param maxValue The upper bound, exclusive, of the values to return.
	@param maxCount The number of integers to return, or -1 if an unlimited
		number of integers should be returned.
	@param repeat <code>true</code> if values should be repeated, else <code>false</code>.
	@exception IllegalArgumentException Thrown if the given maximum count is
		greater than the allowed range.
	*/
	public RandomIntegerIterator(final Random random, final int maxValue, final int maxCount, final boolean repeat)
	{
		this(random, 0, maxValue-1, maxCount, repeat);
	}

	/**Full constructor with default random number generator.
	@param rangeMin The lower bound, inclusive, of the range to include.
	@param rangeMax The upper bound, inclusive, of the range to include.
	@param maxCount The number of integers to return, or -1 if an unlimited
		number of integers should be returned.
	@param repeat <code>true</code> if values should be repeated, else <code>false</code>.
	@exception IllegalArgumentException Thrown if the given maximum count is
		greater than the allowed range.
	*/
	public RandomIntegerIterator(final int rangeMin, final int rangeMax, final int maxCount, final boolean repeat)
	{
		this(new Random(), rangeMin, rangeMax, maxCount, repeat);		
	}

	/**Full constructor with random number generator.
	@param random The random number generator.
	@param rangeMin The lower bound, inclusive, of the range to include.
	@param rangeMax The upper bound, inclusive, of the range to include.
	@param maxCount The number of integers to return, or -1 if an unlimited
		number of integers should be returned.
	@param repeat <code>true</code> if values should be repeated, else <code>false</code>.
	@exception IllegalArgumentException Thrown if the given maximum count is
		greater than the allowed range.
	*/
	public RandomIntegerIterator(final Random random, final int rangeMin, final int rangeMax, final int maxCount, final boolean repeat)
	{
		range=rangeMax-rangeMin+1;	//see how many integers are in the range
		if(maxCount>range)	//if they want more than they can have in the range
			throw new IllegalArgumentException("Cannot return "+maxCount+" from a range of only "+range);
		excludedIntegerSortedSet=new TreeSet<Integer>();	//create a sorted set in which to hold the excluded integers 
		this.random=random;	//save the values
		this.rangeMin=rangeMin;
		this.rangeMax=rangeMax;
		this.maxCount=maxCount;
		this.repeat=repeat;
		count=0;	//show that we have not yet retrieved any integers
		primedNext=null;	//show that we have no primed next value
		filter=null;	//default to no filter
	}

	/**Determines whether the given integer value should be excluded from the iteration.
	<p>This method may safely be called in between calls to <code>next()</code>.</p>
	@param integer The integer to exclude or not.
	@param exclude <code>true</code> if the value should be excluded from future
		calls to <code>next()</code>, else <code>false</code> if it should be
		possible (but not guaranteed) that the value will be returned from
		future calls to <code>next()</code>.
	*/
	public void setExcluded(final int integer, final boolean exclude)
	{
		setExcluded(Integer.valueOf(integer), exclude);	//exclude or not the Integer version of the value
	}

	/**Determines whether the given integer value should be excluded from the iteration.
	<p>This method may safely be called in between calls to <code>next()</code>.</p>
	@param integer The integer to exclude or not.
	@param exclude <code>true</code> if the value should be excluded from future
		calls to <code>next()</code>, else <code>false</code> if it should be
		possible (but not guaranteed) that the value will be returned from
		future calls to <code>next()</code>.
	*/
	public void setExcluded(final Integer integer, final boolean exclude)
	{
		if(exclude)	//if we should include this value
			excludedIntegerSortedSet.add(integer);	//add the value to our opt-out list
		else	//if we shouldn't exclude this value
			excludedIntegerSortedSet.remove(integer);	//remove it from our exclusion set
	}

	/**@return <code>true</code> if there are more integers left to retrieve.*/
	public boolean hasNext()
	{
			//determine if we haven't reached the maximum count or there is no maximum count
			//then prime another value if we can
		return (maxCount<0 || count<maxCount) && primeNext();	//if there are more integers to retrieve, and one is primed and ready
	}

	/**@return The next random integer in the iteration.
	@exception NoSuchElementException Thrown if the iteration has no more elements.
	*/
	public Integer next()
	{
		if(hasNext())	//if there is a next object waiting for us, and we haven't reached our maximum value
		{
			final Integer next=primedNext;	//get the next object primed and waiting
			primedNext=null;	//show that we've used the primed next object
			++count;	//show that we've retrieved another value
			return next;	//return the next integer we found
		}
		else	//if we've reached our max count
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
		if(primedNext==null)	//if there is no primed next object
		{
			primedNext=getNext();	//get the next object
		}
		return primedNext!=null;	//return whether or not there now is a primed next object		
	}

	/**@return The next random integer in the iteration, or <code>null</code> if
		the iteration has no more elements.
	*/
	protected Integer getNext()
	{
		final Filter<Integer> filter=getFilter();	//get our filter, if there is one
		int includedRange=range-excludedIntegerSortedSet.size();	//see how many are left in the range if we exclude the excluded integers
		while(includedRange>0)	//while there are available items to choose from
		{
			final int index=random.nextInt(includedRange);	//select an index into the included range that's left
			int value=rangeMin+index;	//shift the index to the start of the range
			if(excludedIntegerSortedSet.size()>0)	//if we have any exluded integers, adjust our value accordingly; otherwise, we already have the correct value
			{
				final Iterator<Integer> excludedIntegerIterator=excludedIntegerSortedSet.iterator();	//look at the excluded integers in order
				while(excludedIntegerIterator.hasNext())	//look at each excluded integer in order
				{
					final Integer excludedInteger=excludedIntegerIterator.next();	//get the next excluded integer in order
					if(excludedInteger.intValue()<=value)	//if our indexed value includes this excluded value, so to speak
					{
						++value;	//increase our value to compensate for the excluded value
					}
					else	//if we've reached excluded values that are above our indexed value
					{
						break;	//all other exclusions are above our value, so don't check them
					}
				}
			}
			final Integer nextInteger=Integer.valueOf(value);	//we've found the next integer value
			final boolean isPass=filter==null || filter.isPass(nextInteger);	//see if our next integer passes
			if(!repeat || !isPass)	//if we shouldn't repeat values, or if this item is filtered out
			{
				excludedIntegerSortedSet.add(nextInteger);	//add this integer to our excluded integer set so that we won't use it next time
				--includedRange;	//show that we have a smaller included range to choose from, now that we've excluded something
			}
			if(isPass)	//if this item isn't filtered out
			{
				return nextInteger;	//return the integer we found
			}
		}
		return null;	//if we've reached our max count, return null
	}

	/**This implementation does not support element removal, and always throws
		an exception.
	@exception UnsupportedOperationException Thrown because the
		<code>remove</code> operation is not supported by this iterator.
	 */
	public void remove()
	{
		throw new UnsupportedOperationException();
	}

}
