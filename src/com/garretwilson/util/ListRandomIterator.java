package com.garretwilson.util;

import java.util.*;

/**An iterator that randomly iterates a list.
<p>A subset of the list can optionally be returned.</p>
<p>By default the iterator returns a random selection in random order. To
	iterate a random subset in non-random list order, call
	<code>setRandomOrder</code> with <code>false</code>. TODO fix setRandomOrder()</p>
<p>This iterator allows filtering of the returned objects.</p>
@author Garret Wilson
*/
public class ListRandomIterator implements Iterator 
{

	/**The list to randomly iterate.*/
	private final List list;

	/**The iterator that determines the next index of the list to return.*/
	protected final RandomIntegerIterator indexIterator;

	/**The filter used to exclude items from the iterator.*/
	private Filter filter;

		/**@return The filter used to exclude items from the iterator.*/
		public Filter getFilter() {return filter;}

		/*Sets the filter used to exclude items from the iterator.
		@param filter The new filter to use.
		*/
		public void setFilter(final Filter filter) {this.filter=filter;}

	/**Constructs an iterator that returns all of the elements of the list.
	<p>A default random number generator is used.</p>
	@param list The list to randomly iterate.
	*/
	public ListRandomIterator(final List list)
	{
		this(list, new Random());
	}

	/**Constructs an iterator that returns all of the elements of the list.
	@param list The list to randomly iterate.
	@param random The random number generator.
	*/
	public ListRandomIterator(final List list, final Random random)
	{
		this(list, random, list.size());
	}

	/**Constructs an iterator that returns at most <code>maxCount</code> elements
		of the list.
	<p>A default random number generator is used.</p>
	@param list The list to randomly iterate.
	@param maxCount The number of elements to return.
	@exception IllegalArgumentException Thrown if the given maximum count is
		greater than the allowed range or less than zero.
	*/
	public ListRandomIterator(final List list, final int maxCount)
	{
		this(list, new Random(), maxCount);
	}

	/**Constructs an iterator that returns at most <code>maxCount</code> elements
		of the list.
	@param list The list to randomly iterate.
	@param random The random number generator.
	@param maxCount The number of elements to return.
	@exception IllegalArgumentException Thrown if the given maximum count is
		greater than the allowed range or less than zero.
	*/
	public ListRandomIterator(final List list, final Random random, final int maxCount)
	{
		if(maxCount<0)	//if they want a negative number of elements from the list (the random integer iterator will make sure the maximum count is not too high)
			throw new IllegalArgumentException("Cannot return less than zero elements from a list.");
		this.list=list;	//save the list
		indexIterator=new RandomIntegerIterator(random, list.size(), maxCount, false);	//randomly iterate through maxCount indexes without repeating
		indexIterator.setFilter(new IndexFilter());	//create a filter that will filter out indexes based upon the object they represent
	}

	/**Determines whether the given index should be excluded from the iteration.
	<p>This method may safely be called in between calls to <code>next()</code>.</p>
	@param index The index to exclude or not.
	@param exclude <code>true</code> if the element at the given index
		should be excluded from future calls to <code>next()</code>, else
		<code>false</code> if it should be possible (but not guaranteed) that the
		element at the given index will be returned from future calls to
		<code>next()</code>.
	*/
	public void setExcluded(final int index, final boolean exclude)
	{
		indexIterator.setExcluded(index, exclude);	//include or exclude the given index
	}

	/**Determines whether the given element should be excluded from the iteration.
	If the given element is not in the list, no action occurs.
	<p>This method may safely be called in between calls to <code>next()</code>.</p>
	@param object The element to exclude or not.
	@param exclude <code>true</code> if the element should be excluded from future
		calls to <code>next()</code>, else <code>false</code> if it should be
		possible (but not guaranteed) that the element will be returned from
		future calls to <code>next()</code>.
	*/
	public void setExcluded(final Object object, final boolean exclude)
	{
		final int index=list.indexOf(object);	//get the index in the list of the object
		if(index>=0)	//if the object is in the list
			setExcluded(index, exclude);	//exclude or not the index
	}

	/**@return <code>true</code> if there are more elements left to retrieve.*/
	public boolean hasNext()
	{
		return indexIterator.hasNext();	//see if there are more random indexes left
	}

	/**@return The next random element in the iteration.
	@exception NoSuchElementException Thrown if the iteration has no more elements.
	*/
	public Object next()
	{
		return list.get(((Integer)indexIterator.next()).intValue());	//get the next random index and return the corresponding object from the list
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

	/**A class that filters random integers based upon whether the object at
		the given index in our list passes the criteria of our local filter.
	@author Garret Wilson
	*/
	protected class IndexFilter implements Filter
	{
		/**Determines whether a given object should pass through the filter or be
			filtered out.
		An integer object will be filtered out if the object at that index in our
			list doesn't pass our main list filter.
		@param object The object to filter.
		@return <code>true</code> if the object should pass through the filter, else
			<code>false</code> if the object should be filtered out.
		*/
		public boolean isPass(final Object object)
		{
			if(getFilter()!=null)	//if we have a local filter
			{
				if(object instanceof Integer)	//if the object is an integer (it always should be an integer
				{
					final Integer integer=(Integer)object;	//cast the object to an integer
					final Object item=list.get(integer.intValue());	//get this item in our list
					return getFilter().isPass(item);	//filter the item in our list using our local filter
				}
				return false;	//filter out anything we don't understand
			}
			else	//if we don't have a filter
			{
				return true;	//let everything pass
			}
		}
		
	}

}
