package com.garretwilson.util;

import java.util.*;

/**An iterator that iterates a sequence of random iterators.
	The iterator has the ability to restrict its output to a particular range.
	A subset of the range can optionally be returned.
	This iterator can keep track of the numbers used and only return unused integers.
<p>This iterator returns object of type <code>Integer</code>.</p>
@author Garret Wilson
@see Integer
*/
public class RandomIntegerIterator implements Iterator 
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
		unlimited number of integers should be returned
	*/
	protected final int maxCount;

	/**<code>true</code> if values should be repeated, else <code>false</code>.*/
	protected final boolean repeat;

	/**Create a new set to hold the integers excluded from the iterator.*/
	protected final SortedSet excludedIntegerSortedSet;

	/**The total number of integers returned.*/
	protected int count;

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
		excludedIntegerSortedSet=new TreeSet();	//create a sorted set in which to hold the excluded integers 
		this.random=random;	//save the values
		this.rangeMin=rangeMin;
		this.rangeMax=rangeMax;
		this.maxCount=maxCount;
		this.repeat=repeat;
		count=0;	//show that we have not yet retrieved any integers
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
		setExcluded(new Integer(integer), exclude);	//exclude or not the Integer version of the value
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
		return maxCount<0 || count<maxCount;	//determine if we haven't reached the maximum count or there is no maximum count
	}

	/**@return The next random integer in the iteration.
	@exception NoSuchElementException Thrown if the iteration has no more elements.
	*/
	public Object next()
	{
		if(hasNext())	//if we have more elements
		{
			final int includedRange=range-excludedIntegerSortedSet.size();	//see how many are left in the range if we exclude the excluded integers
			final int index=random.nextInt(includedRange);	//select an index into the included range that's left
			int value=rangeMin+index;	//shift the index to the start of the range
			if(excludedIntegerSortedSet.size()>0)	//if we have any exluded integers, adjust our value accordingly; otherwise, we already have the correct value
			{
				final Iterator excludedIntegerIterator=excludedIntegerSortedSet.iterator();	//look at the excluded integers in order
				while(excludedIntegerIterator.hasNext())	//look at each excluded integer in order
				{
					final Integer excludedInteger=(Integer)excludedIntegerIterator.next();	//get the next excluded integer in order
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
			final Integer nextInteger=new Integer(value);	//we've found the next integer value
			if(!repeat)	//if we shouldn't repeat values
			{
				excludedIntegerSortedSet.add(nextInteger);	//add this integer to our excluded integer set so that we won't use it next time
			}
			++count;	//show that we've retrieved another value
			return nextInteger;	//return the integer we found
		}
		else	//if we've reached our max count
		{
			throw new NoSuchElementException();
		}
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
