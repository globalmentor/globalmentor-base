package com.garretwilson.util;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**An iterator to a single object.
This implementation allows <code>null</code> values.
This implementation does not allow removal, as removing has no meaning in this context.
@param <E> The type of elements in this iterator
@author Garret Wilson
*/
public class ObjectIterator<E> implements Iterator<E>
{
	/**The single object being iterated.*/
	private E object;

	/**Whether we've not retrieved the object.*/
	private boolean hasNext=true;

	/**Object constructor.
	@param object The single object over which iteration should occur.
	*/
	public ObjectIterator(final E object)
	{
		this.object=object;	//save the object
	}

	/**@return <code>true</code> if the single object has not yet been retrieved.*/
	public boolean hasNext() {return hasNext;}

	/**@return The next element in the iteration.
	@exception NoSuchElementException if the single object has already been returned.
	*/
	public E next()
	{
		if(hasNext)	//if we haven't returned the object, yet
		{
			final E next=object;	//get the object
			hasNext=false;	//indicate we've retrieved the object
			object=null;	//release the object
			return next;	//return our copy of the object
		}
		else	//if we've already returned the object
		{
			throw new NoSuchElementException("Already returned object.");
		}
	}

	/**Removes from the underlying collection the last element returned by the iterator
	This implementation does not support removal.
	@exception UnsupportedOperationException because this implementation does not support removal.
	*/
	public void remove()
	{
		throw new UnsupportedOperationException("This iterator does not support removing the object.");
	}
}
