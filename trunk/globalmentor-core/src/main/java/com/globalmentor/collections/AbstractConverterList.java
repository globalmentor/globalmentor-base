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

package com.globalmentor.collections;

import java.util.*;

import com.globalmentor.collections.iterators.IteratorDecorator;
import com.globalmentor.collections.iterators.ListIteratorDecorator;

/**A list that provides access to another list, automatically converting elements to objects possibly of a different type.
The conversion is done on the fly as elements are requested, and not before.
@param <S> The type of element contained in the source list.
@param <D> The type of element contained in the destination list.
@author Garret Wilson
*/
public abstract class AbstractConverterList<S, D> extends ListDecorator<D>	//TODO add conversion access for all necessary methods
{

	/**The map of converted objects, or <code>null</code> if we should not remember converted objects.*/ 
	private final Map<S, D> convertedObjectMap;
	
	/**List constructor that converts objects from scratch on each request.
	@param list The list the elements of which this list should convert.
	*/
	public AbstractConverterList(final List<S> list)
	{
		this(list, false);	//construct the class without remembering converted objects
	}

	/**List and reuse constructor.
	@param list The list the elements of which this list should convert.
	@param reuse <code>true</code> if the converter should remember and reuse converted objects,
		or <code>false</code> if the conversion should take place from scratch upon each request.
	*/
	@SuppressWarnings("unchecked")
	public AbstractConverterList(final List<S> list, final boolean reuse)
	{
		super((List<D>)list);	//construct the parent class with the source list, even though we know the list may be of a different type; we'll do the correct conversions for the access methods
		convertedObjectMap=reuse ? new HashMap<S, D>() : null;	//if we should reuse converted objects, create a map in which to store them
	}

	/**Converts an object in the list to another object.
	@param source The object to convert.
	@return The converted object.
	*/
	protected abstract D convert(final S source);

	/**Converts an object in the list to another object or retrieves it from the
	converted object map if it has already been converted and saved.
	@param source The object to convert.
	@return The converted object that was saved, or an object created from scratch, depending on the class settings.
	*/
	private D getConvertedObject(final S source)
	{
		D convertedObject=null;	//we'll see if we can convert the object
		if(convertedObjectMap!=null)	//if we are remembering converted objects
		{
			convertedObject=convertedObjectMap.get(source);	//see if we already have converted the object
		}
		if(convertedObject==null)	//if we don't already have an object, we'll have to convert it from scratch
		{
			convertedObject=convert(source);	//convert the object
			if(convertedObjectMap!=null)	//if we are remembering converted objects
			{
				convertedObjectMap.put(source, convertedObject);	//store the converted object for next time
			}
		}
		return convertedObject;	//return the converted object		
	}

	/**Converts and returns the element at the specified position in this list.
	@param index The index of element to convert and return.
	@return The converted element at the specified position in this list.
	@exception IndexOutOfBoundsException Thrown if the index is out of range (index &lt; 0 || index &gt; size()).
	*/
	@SuppressWarnings("unchecked")
	public D get(final int index)
	{
		return getConvertedObject((S)super.get(index));	//convert and return the element at this index
	}

	/**@return A custom proxied iterator that will convert returned elements on the fly.*/
	@SuppressWarnings("unchecked")
	public Iterator<D> iterator()
	{
		return new ConverterIterator((Iterator<S>)super.iterator());	//create an iterator that will convert the elements
	}

	/**@return A custom proxied list iterator that will convert returned elements on the fly.*/
	@SuppressWarnings("unchecked")
	public ListIterator<D> listIterator()
	{
		return new ConverterListIterator((ListIterator<S>)super.listIterator());	//create an iterator that will convert the elements
	}

	/**Returns a custom proxied list iterator that will convert returned elements on the fly.
	@param index The index of first element to be returned from the
		list iterator (by a call to the <code>next</code> method).
	@return A custom proxied list iterator that will convert returned elements.
	@exception IndexOutOfBoundsException Thrown if the index is out of range
		(index &lt; 0 || index &gt; size()).
	*/
	@SuppressWarnings("unchecked")
	public ListIterator<D> listIterator(final int index)
	{
		return new ConverterListIterator((ListIterator<S>)super.listIterator(index));	//create an iterator that will convert the entries to questions
	}

	/**A custom proxied iterator that converts returned elements on the fly.
	@author Garret Wilson
	*/
	protected class ConverterIterator extends IteratorDecorator<D>
	{
	
		/**Iterator constructor.
		@param iterator The iterator of source objects.
	*/
		@SuppressWarnings("unchecked")
		public ConverterIterator(final Iterator<S> iterator)
		{
			super((Iterator<D>)iterator);	//construct the parent class source iterator, even though we know the list may be of a different type; we'll do the correct conversions for the access methods
		}
	
		/**Converts and returns the next element in the iteration.
		@return An object representing the converted next element in the iteration.
		@exception NoSuchElementException Thrown if the iteration has no more elements.
		*/
		@SuppressWarnings("unchecked")
		public D next()
		{
			return getConvertedObject((S)super.next());	//get the next entry, convert it, and return it 			
		}
	
	//TODO fix conversion versions of the other iterator methods
	
	}

	/**A custom proxied list iterator that converts returned elements on the fly.
	@author Garret Wilson
	*/
	protected class ConverterListIterator extends ListIteratorDecorator<D>
	{
	
		/**List iterator constructor.
		@param iterator The iterator of source objects.
		*/
		@SuppressWarnings("unchecked")
		public ConverterListIterator(final ListIterator<S> listIterator)
		{
			super((ListIterator<D>)listIterator);	//construct the parent class source iterator, even though we know the list may be of a different type; we'll do the correct conversions for the access methods
		}
	
		/**Converts and returns the next element in the iteration.
		@return An object representing the converted next element in the iteration.
		@exception NoSuchElementException Thrown if the iteration has no more elements.
		*/
		@SuppressWarnings("unchecked")
		public D next()
		{
			return getConvertedObject((S)super.next());	//get the next entry, convert it, and return it 			
		}

		/**Converts and returns the previous element in the iteration.
		@return An object representing the converted previous element in the iteration.
		@exception NoSuchElementException Thrown if the iteration has no previous element.
		*/
		@SuppressWarnings("unchecked")
		public D previous()
		{
			return getConvertedObject((S)super.previous());	//get the previous entry, convert it, and return it 			
		}
	
	//TODO fix conversion versions of the other iterator methods
	
	}

}
