package com.garretwilson.urf;

import java.util.*;
import java.lang.reflect.Array;
import java.net.URI;
import static java.lang.System.*;

import com.garretwilson.lang.IntegerUtilities;
import static com.garretwilson.lang.ObjectUtilities.*;
import com.garretwilson.util.DefaultListIterator;
import static com.garretwilson.urf.URF.*;

/**An URF list resource that allows convenient indexed access to its elements.
The {@link List}-related methods consider the list elements to be the first of each sequential ordinal property starting with zero.
For example, if a list had properties with indexes º0, º1, º1, º2, º4, the second object with index º1 would be ignored,
as would index º4. {@link List} modification methods will remove duplicate ordinal properties.
The {@link #clear()} method will remove all ordinal properties, even non-sequential ones.
This implementation does not supoprt <code>null</code> values.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@param <E> The type of element stored in the list.
@author Garret Wilson
*/
public class URFListResource<E extends URFResource> extends DefaultURFResource implements URFCollectionResource<E>, List<E>
{

	/**Default constructor with no URI.*/
	public URFListResource()
	{
		this((URI)null);	//create a resource without a URI
	}

	/**URI and type URIs constructor.
	If no types are specified, the type {@value URF#LIST_CLASS_URI} will be added.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
	@param typeURIs The URIs of the types, if any, to add to the resource.
	*/
	public URFListResource(final URI uri, final URI... typeURIs)
	{
		super(uri, typeURIs.length>0 ? typeURIs : new URI[]{LIST_CLASS_URI});	//construct the parent class, specifying the list class if there are no types given
	}

	/**Collection constructor with no URI.
	The elements of the specified collection will be added to this list in the order they are returned by the collection's iterator.
	@param collection The collection whose elements are to be placed into this list.
	@exception NullPointerException if the specified collection is <code>null</code>.
	*/
	public URFListResource(final Collection<? extends E> collection)
	{
		this(null, collection);	//construct the class with no URI
	}

	/**URI and collection constructor.
	The elements of the specified collection will be added to this list in the order they are returned by the collection's iterator.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
	@param collection The collection whose elements are to be placed into this list.
	@exception NullPointerException if the specified collection is <code>null</code>.
	*/
	public URFListResource(final URI uri, final Collection<? extends E> collection)
	{
		this(uri);	//construct the class with the URI
		addAll(collection);	//add all the collection elements to the list
	}

	/**Retrieves the length of the list.
	The length is defined as the first zero-based index for which there is no element.
	This implementation traverses of all ordinal properties.
	@return The length of the list.
	*/
	public long getLength()
	{
		long length=0;	//keep track of how many sequential zero-based indexes we find
		readLock().lock();	//get a read lock
		try
		{
			for(final URFProperty ordinalProperty:getNamespaceProperties(ORDINAL_NAMESPACE_URI))	//look at all the ordinal properties; if there are duplicate ordinal properties, we'll ignore them
			{
				final long index=Long.parseLong(getLocalName(ordinalProperty.getPropertyURI()));	//get the index
				if(index==length)	//if we found this index
				{
					++length;	//show that we found another element
				}
				else if(index>length)	//if the index is more than the length, we've skipped an index
				{
					break;	//stop enumerating the indexes
				}
			}
			return length;	//return the count of all subsequent indexes starting at zero
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Returns the number of elements in this list.
	If this list contains more than {@link Integer#MAX_VALUE} elements, returns {@link Integer#MAX_VALUE}.
	This implementation delegates to {@link #getLength()}.
	@return the number of elements in this list
	*/
	public int size()
	{
		final long length=getLength();	//get the length of the list
		return length<Integer.MAX_VALUE ? (int)length : Integer.MAX_VALUE;	//return the value, with a ceiling of Integer.MAX_VALUE
	}

	/**Returns <code>true</code> if this list contains no elements.
	@return <tt>true</tt> if this list contains no elements
	*/
	public final boolean isEmpty()
	{
		return !hasProperty(ORDINAL_0_URI);	//if the list has no zero index, by definition it is empty (its length is zero)
	}

	/**Returns if this list contains the specified element.
	@param object The element whose presence in this list is to be tested.
	@return <code>true</code> if this list contains the specified element
	@throws ClassCastException if the type of the specified element is incompatible with this list.
	@throws NullPointerException if the specified element is <code>null</code> and this list does not permit <code>null</code> elements.
	*/
	public boolean contains(final Object object)
	{
		checkInstance((URFResource)object, "URF lists do not support null elements.");
		readLock().lock();	//get a read lock
		try
		{
			for(final URFResource element:this)	//look at each element
			{
				if(element.equals(object))	//if the object is equal to this element
				{
					return true;	//we found the element
				}
			}
			return false;	//indicate that we didn't find the element
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Returns an iterator over the elements in this list in proper sequence.
	@return An iterator over the elements in this list in proper sequence
	*/
	public Iterator<E> iterator()
	{
		return listIterator();	//return the list iterator
	}

	/**Returns an array containing all of the elements in this list in proper sequence (from first to last element).
	@return An array containing all of the elements in this list in proper sequence.
	@see Arrays#asList(Object[])
	*/
	public final Object[] toArray()
	{
		readLock().lock();	//get a read lock
		try
		{
			final int size=size();	//get the current size
			final Object[] array=new Object[size];	//create a new array of the correct size
			for(int i=0; i<size; ++i)	//for each index
			{
				array[i]=getPropertyValue(createOrdinalURI(i));	//store this element in the array
			}
			return array;	//return the array
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Returns an array containing all of the elements in this list in proper sequence (from first to last element);
	the runtime type of the returned array is that of the specified array.
	If the list fits in the specified array, it is returned therein.
	Otherwise, a new array is allocated with the runtime type of the specified array and the size of this list.
	If the list fits in the specified array with room to spare (i.e., the array has more elements than the list),
	the element in the array immediately following the end of the list is set to <code>null</code>.
	@param array The array into which the elements of this list are to be stored, if it is big enough;
	otherwise, a new array of the same runtime type is allocated for this purpose.
	@return An array containing the elements of this list.
	@throws ArrayStoreException if the runtime type of the specified array is not a supertype of the runtime type of every element in this list.
	@throws NullPointerException if the specified array is <code>null</code>.
	*/
	public <T> T[] toArray(T[] array)
	{
		int length=array.length;	//get the length of the array
		long index=0;	//keep track of our index
		readLock().lock();	//get a read lock
		try
		{
			do	//start out assuming that the array is large enough; don't find our size ahead of time, because that's time consuming and we'll have to do it eventually anyway
			{
				final URFResource propertyValue=getPropertyValue(createOrdinalURI(index));	//get the value at the current index
				if(propertyValue==null)	//if we've reached the end of the list
				{
					break;	//copying
				}
				if(index>=length)	//if we run out of array indexes
				{
					final int size=size();	//we'll have to actually calculate our size now
					final T[] newArray=(T[])Array.newInstance(array.getClass().getComponentType(), size);	//create a new array with the correct size
					arraycopy(array, 0, newArray, 0, length);	//copy over everything we've copied so far
					array=newArray;	//replace the array
					length=size;	//replace the length with the new length
				}
				++index;	//go to the next index
			}
			while(index<=Integer.MAX_VALUE);	//keep copying until we run out of integers
			if(index<length-1)	//if we haven't copied something into the last element of the array
			{
				array[(int)index]=null;	//show that we didn't use all the available array indexes by setting the next index to null
			}
			return array;	//return the array
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

  // Modification Operations

  /**Appends the specified element to the end of this list.
	@param element Element to be appended to this list.
	@return <code>true</code> (as specified by {@link Collection#add}).
	@throws ClassCastException if the class of the specified element prevents it from being added to this list.
	@throws NullPointerException if the specified element is <code>null</code> and this list does not permit <code>null</code> elements.
	@throws IllegalArgumentException if some property of this element prevents it from being added to this list.
	*/
	public boolean add(final E element)
	{
		checkInstance((URFResource)element, "URF lists do not support null elements.");
		writeLock().lock();	//get a write lock
		try
		{
			add(size(), element);	//add the element at the end
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
		return true;	//this method always modifies the list
	}

	/**Removes the first occurrence of the specified element from this list, if it is present.
	If this list does not contain the element, it is unchanged.
	@param object Element to be removed from this list, if present.
	@return <code>true</code> if this list contained the specified element.
	@throws ClassCastException if the type of the specified element is incompatible with this list.
	@throws NullPointerException if the specified element is <code>null</code> and this list does not permit <code>null</code> elements.
	*/
	public boolean remove(final Object object)
	{
		checkInstance((URFResource)object, "URF lists do not support null elements.");
		writeLock().lock();	//get a write lock
		try
		{
			final int index=indexOf(object);	//get the index of the object
			if(index>=0)	//if the object is in the list
			{
				remove(index);	//remove the object from the index
				return true;	//indicate that the list was modified
			}
			else	//if we couldn't find the object
			{
				return false;	//we left the list untouched
			}
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}


  // Bulk Modification Operations

	/**Returns <code>true</code> if this list contains all of the elements of the specified collection.
	@param collection The collection to be checked for containment in this list.
	@return <code>true</code> if this list contains all of the elements of the specified collection.
	@throws ClassCastException if the types of one or more elements in the specified collection are incompatible with this list.
	@throws NullPointerException if the specified collection contains one or more <code>null</code> elements and this list does not permit <code>null</code> elements,
	or if the specified collection is <code>null</code>.
	@see #contains(Object)
	*/
	public boolean containsAll(final Collection<?> collection)
	{
		readLock().lock();	//get a read lock
		try
		{
			for(final Object object:collection)	//look at each item in the collection
			{
				if(!contains((URFResource)object))	//if we don't contain this object
				{
					return false;	//indicate that we don't contain all the items
				}
			}
			return false;	//there were no non-matches, so everything matched
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Appends all of the elements in the specified collection to the end of this list,
	in the order that they are returned by the specified collection's iterator (optional operation).
	The behavior of this operation is undefined if the specified collection is modified while the operation is in progress.
	@param collectiono The collection containing elements to be added to this list.
	@return <code>true</code> if this list changed as a result of the call.
	@throws ClassCastException if the class of an element of the specified collection prevents it from being added to this list.
	@throws NullPointerException if the specified collection contains one or more <code>null</code> elements and this list does not permit <code>null</code> elements,
		or if the specified collection is <code>null</code>
	@see #add(Object)
	*/
	public boolean addAll(final Collection<? extends E> collection)
	{
		writeLock().lock();	//get a write lock
		try
		{
			final int size=size();	//get the size of this list
			long index=size;	//we'll start adding elements at the end
			for(final E item:collection)	//for each item in the collection
			{
				checkInstance((URFResource)item, "URF lists do not support null elements.");
				setPropertyValue(createOrdinalURI(index), item);	//add this item at the current index
				++index;	//go to the next index for the next item
			}
			return index>size;	//if the index is greater than the size, we added at least one element and therefore modified the list
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

	/**Inserts all of the elements in the specified collection into this list at the specified position.
	Shifts the element currently at that position (if any) and any subsequent elements to the right (increases their indices).
	The new elements will appear in this list in the order that they are returned by the specified collection's iterator.
	The behavior of this operation is undefined if the specified collection is modified while the operation is in progress.
	@param index The index at which to insert the first element from the specified collection.
	@param c The collection containing elements to be added to this list.
	@return <code>true</code> if this list changed as a result of the call.
	@throws ClassCastException if the class of an element of the specified collection prevents it from being added to this list.
	@throws NullPointerException if the specified collection contains one or more <code>null</code> elements and this list does not permit <code>null</code> elements,
		or if the specified collection is <code>null</code>.
	@throws IndexOutOfBoundsException if the index is out of range (<code>index &lt; 0 || index &gt; size()</code>).
	*/
	public boolean addAll(final int index, final Collection<? extends E> collection)
	{
		final int collectionSize=collection.size();	//get the size of the colletion
		writeLock().lock();	//get a write lock
		try
		{
			final int size=size();	//get the size of this list
			IntegerUtilities.checkIndexBounds(index, 0, size);	//check the bounds of the index
			long newIndex=index;	//start at the given index
			for(final E element:collection)	//for each element in the collection
			{
				checkInstance((URFResource)element, "URF lists do not support null elements.");
				final E oldElement=(E)setPropertyValue(createOrdinalURI(newIndex), element);	//replace the element at this index with the element from the collection
				setPropertyValue(createOrdinalURI(newIndex+collectionSize), element);	//shift the old element up in the list past the added elements from the collection
			}
			return collectionSize>0;	//if the collection wasn't empty, we modified the list
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

	/**Removes from this list all of its elements that are contained in the specified collection.
 	@param collection The collection containing elements to be removed from this list.
	@return <code>true</code> if this list changed as a result of the call.
	@throws ClassCastException if the class of an element of this list is incompatible with the specified collection.
	@throws NullPointerException if this list contains a <code>null</code> element and the specified collection does not permit <code>null</code> elements,
		or if the specified collection is <code>null</code>.
	@see #remove(Object)
	@see #contains(Object)
	*/
	public boolean removeAll(final Collection<?> collection)
	{
		boolean changed=false;	//nothing in this list has changed so far
		writeLock().lock();	//get a write lock
		try
		{
			for(final Object object:collection)	//look at each object in the collection; this is an expensive brute force method, but it's about the only simple way to do things
			{
				if(remove(object))	//remove the object from the list; if the object was in the list previously
				{
					changed=true;	//the list changed
				}
			}
			return changed;	//return whether this list changed
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

	/**Retains only the elements in this list that are contained in the specified collection.
	In other words, removes from this list all the elements that are not contained in the specified collection.
	@param collection The collection containing elements to be retained in this list.
	@return <code>true</code> if this list changed as a result of the call.
	@throws ClassCastException if the class of an element of this list is incompatible with the specified collection.
	@throws NullPointerException if this list contains a <code>null</code> element and the specified collection does not permit <code>null</code> elements,
		or if the specified collection is <code>null</code>.
	@see #remove(Object)
	@see #contains(Object)
	*/
	public boolean retainAll(final Collection<?> collection)
	{
		boolean changed=false;	//nothing in this list has changed so far
		writeLock().lock();	//get a write lock
		try
		{
			final int size=size();	//get the size of this list; this is an expensive operation to do up front, but at least it doesn't involve any list modification, which would shift elements and be even more expensive
			for(int i=size()-1; i>=0; --i)	//get the size of this list; this is an expensive operation to do up front, but at least it doesn't involve any list modification, which would shift elements and be even more expensive; working backwards will cause shifting possibly to be less wasteful, and will not require index compensations
			{
				final URFResource propertyValue=getPropertyValue(createOrdinalURI(i));	//get the value at the current index
				if(!collection.contains(propertyValue))	//if this value isn't contained in the collection
				{
					remove(i);	//remove the item at this index; shifting won't disturb our iteration, as we're working backwards
					changed=true;	//we just changed the list
				}
			}
			return changed;	//indicate whether we changed the list
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

	/**Removes all of the elements from this list.
	The list will be empty after this call returns.
	*/
	public void clear()
	{
		removeNamespaceProperties(ORDINAL_NAMESPACE_URI);	//remove all ordinal properties
	}

  // Comparison and hashing

	/**Compares the specified object with this list for equality.
	Returns <code>true</code> if and only if the specified object is also a list, both lists have the same size,
	and all corresponding pairs of elements in the two lists are <dfn>equal</dfn>.
	@param object The object to be compared for equality with this list.
	@return <code>true</code> if the specified object is equal to this list.
	*/
  public boolean equals(final Object object)
  {
  	if(object instanceof List)	//if the object is a list
  	{
  		final List<?> list=(List<?>)object;	//get the object as a list
  		int index=0;	//start with the first index
  		readLock().lock();	//get a read lock
  		try
  		{
	  		for(final Object item2:list)	//for each object in the other list
	  		{
	  			final URFResource item1=getPropertyValue(createOrdinalURI(index));	//get the value at the given index in this list
	  			if(item1==null || !item1.equals(item2))	//if these elements aren't the same (TODO use a better routine to compare nulls once URF is given nulls)
	  			{
	  				return false;	//the lists aren't equal
	  			}
	  			++index;	//go to the next index in this list
	  		}
	  		if(getPropertyValue(createOrdinalURI(index))!=null)	//if the lists are the same size, the next index in this list should be null
	  		{
  				return false;	//the lists aren't equal; this list is longer than the other
	  		}
	  		return true;	//both lists were the same size and all objects matched
  		}
  		finally
  		{
  			readLock().unlock();	//always release the read lock
  		}
  	}
  	else	//if the other object is not a list
  	{
  		return false;	//the objects aren't equal
  	}
  }

	/**Returns the hash code value for this list. The hash code of a list is defined to be the result of the following calculation:
	<pre>
		int hashCode = 1;
		Iterator&lt;E&gt; i = list.iterator();
		while(i.hasNext())
		{
			E obj = i.next();
			hashCode = 31*hashCode + (obj==null ? 0 : obj.hashCode());
		}
	</pre>
	@return The hash code value for this list.
	@see Object#equals(Object)
	@see #equals(Object)
	*/
	public int hashCode()
	{
		int hashCode=1;	//start with a hash code of 1, according to the list contract
		long index=0;	//start at the first index
		readLock().lock();	//get a read lock
		try
		{
			do
			{
				final URFResource propertyValue=getPropertyValue(createOrdinalURI(index));	//get the value at the current index
				if(propertyValue==null)	//if we've reached the end of the list
				{
					break;	//stop hashing
				}
				hashCode=31*hashCode+propertyValue.hashCode();	//update the hash code, as per the list contract
				++index;	//go to the next index
			}
			while(index<=Integer.MAX_VALUE);	//keep looking until we run out of integers
			return hashCode;	//return the calculated hash code
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}


  // Positional Access Operations

	/**Returns the element at the specified position in this list.
	@param index The index of the element to return.
	@return The element at the specified position in this list.
	@throws IndexOutOfBoundsException if the index is out of range (<code>index &lt; 0 || index &gt;= size()</code>)
	*/
	public E get(final int index)
	{
		readLock().lock();	//get a read lock
		try
		{
			return (E)getPropertyValue(createOrdinalURI(checkIndexBounds(index, false)));	//return the element at the given index after checking the index
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Replaces the element at the specified position in this list with the specified element.
	@param index The index of the element to replace.
	@param element The element to be stored at the specified position
	@return The element previously at the specified position.
	@throws ClassCastException if the class of the specified element prevents it from being added to this list.
	@throws NullPointerException if the specified element is <code>null</code> and this list does not permit <code>null</code> elements.
	@throws IndexOutOfBoundsException if the index is out of range (<code>index &lt; 0 || index &gt;= size()</code>).
	*/
	public E set(final int index, E element)
	{
		return (E)setPropertyValue(createOrdinalURI(checkIndexBounds(index, false)), element);	//check the index, set the value, and return the old value
	}

	/**Inserts the specified element at the specified position in this list.
	Shifts the element currently at that position (if any) and any subsequent elements to the right (adds one to their indices).
	@param index The index at which the specified element is to be inserted.
	@param element The element to be inserted.
	@throws ClassCastException if the class of the specified element prevents it from being added to this list.
	@throws NullPointerException if the specified element is <code>null</code> and this list does not permit <code>null</code> elements.
	@throws IndexOutOfBoundsException if the index is out of range (<code>index &lt; 0 || index &gt; size()</code>)
	*/
	public void add(int index, E element)
	{
		writeLock().lock();	//get a write lock
		try
		{
			checkIndexBounds(index, true);	//check the index, allowing the size() index
			do
			{
				element=(E)setPropertyValue(createOrdinalURI(index), element);	//replace the element at this index and see what used to be at that index
				++index;	//the old element, if any, will be shifted up one index
			}
			while(element!=null);	//keep shifting upwards until we replace a null element (which is one more than the end of the list)
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

	/**Removes the element at the specified position in this list.
	Shifts any subsequent elements to the left (subtracts one from their indices).
	Returns the element that was removed from the list.
	@param index The index of the element to be removed.
	@return The element previously at the specified position.
	@throws IndexOutOfBoundsException if the index is out of range (<code>index &lt; 0 || index &gt;= size()</code>).
	*/
	public E remove(final int index)
	{
		writeLock().lock();	//get a write lock
		try
		{
			final int size=size();	//find the size of the list
			IntegerUtilities.checkIndexBounds(index, 0, size);	//make sure the index is within the correct bounds (we don't need to use our local routine because we've already calculated the size
			E element=null;	//start off with a null, which will be the new value of the last index
			for(int i=size-1; i>=index; --i)	//start with the last index and work our way down to the requested index
			{
				element=(E)setPropertyValue(createOrdinalURI(i), element);	//replace the current index with the last element we replaced, and update our record of the last element
			}
			return element;	//the last element replaced will be the old value of the requested index
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}


  // Search Operations

	/**Returns the index of the first occurrence of the specified element in this list, or -1 if this list does not contain the element.
	@param object The element to search for.
	@return The index of the first occurrence of the specified element in this list, or -1 if this list does not contain the element.
	@throws ClassCastException if the type of the specified element is incompatible with this list.
	@throws NullPointerException if the specified element is <code>null</code> and this list does not permit <code>null</code> elements.
	*/
	public int indexOf(final Object object)
	{
		checkInstance((URFResource)object, "URF lists do not support null elements.");
		long index=0;	//keep track of our index
		readLock().lock();	//get a read lock
		try
		{
			do
			{
				final URFResource propertyValue=getPropertyValue(createOrdinalURI(index));	//get the value at the current index
				if(propertyValue==null)	//if we've reached the end of the list
				{
					break;	//stop looking
				}
				if(propertyValue.equals(object))	//if we find the object
				{
					return (int)index;	//return the index
				}
				++index;	//go to the next index
			}
			while(index<=Integer.MAX_VALUE);	//keep looking until we run out of integers
			return -1;	//indicate that we didn't find the value
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Returns the index of the last occurrence of the specified element in this list, or -1 if this list does not contain the element.
	@param object The element to search for.
	@return The index of the last occurrence of the specified element in this list, or -1 if this list does not contain the element.
	@throws ClassCastException if the type of the specified element is incompatible with this list.
	@throws NullPointerException if the specified element is <code>null</code> and this list does not permit <code>null</code> elements.
	*/
	public int lastIndexOf(final Object object)
	{
		checkInstance((URFResource)object, "URF lists do not support null elements.");
		long index=0;	//keep track of our index
		int lastIndex=-1;	//we'll update the last index with the last known index of a matching value
		readLock().lock();	//get a read lock
		try
		{
			do
			{
				final URFResource propertyValue=getPropertyValue(createOrdinalURI(index));	//get the value at the current index
				if(propertyValue==null)	//if we've reached the end of the list
				{
					break;	//stop looking
				}
				if(propertyValue.equals(object))	//if we find the object
				{
					lastIndex=(int)index;	//note this index and keep looking
				}
				++index;	//go to the next index
			}
			while(index<=Integer.MAX_VALUE);	//keep looking until we run out of integers
			return lastIndex;	//return the last known index, if any
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}


  // List Iterators

	/**Returns a list iterator over the elements in this list (in proper sequence).
	@return A list iterator over the elements in this list (in proper sequence).
	*/
	public ListIterator<E> listIterator()
	{
		return listIterator(0);	//return a list iterator starting at the first index
	}

	/**Returns a list iterator of the elements in this list (in proper sequence), starting at the specified position in this list.
	@param index The index of first element to be returned from the list iterator (by a call to the {@link Iterator#next()} method).
	@return A list iterator of the elements in this list (in proper sequence), starting at the specified position in this list.
	@throws IndexOutOfBoundsException if the index is out of range (<code>index &lt; 0 || index &gt; size()</code>)
	*/
	public ListIterator<E> listIterator(int index)
	{
		readLock().lock();	//get a read lock
		try
		{
			checkIndexBounds(index, true);	//check the index, allowing the size() index
			return new DefaultListIterator<E>(this, index);	//create and return a default list iterator
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

  // View

	/**Returns a view of the portion of this list between the specified <var>fromIndex</var>, inclusive, and <var>toIndex</var>, exclusive.
	(If <var>fromIndex</var> and <var>toIndex</var> are equal, the returned list is empty.)
	The returned list is backed by this list, so non-structural changes in the returned list are reflected in this list, and vice-versa.
	The returned list supports all of the optional list operations supported by this list.<p>
	The semantics of the list returned by this method become undefined if the backing list (i.e., this list) is <dfn>structurally modified</dfn> in
  any way other than via the returned list.
	@param fromIndex The low endpoint (inclusive) of the sub-list
	@param toIndex The high endpoint (exclusive) of the sub-list.
	@return A view of the specified range within this list.
	@throws IndexOutOfBoundsException for an illegal endpoint index value (<var>fromIndex &lt; 0 || toIndex &gt; size || fromIndex &gt; toIndex</var>).
	*/
	public List<E> subList(int fromIndex, int toIndex)
	{
		throw new UnsupportedOperationException("URF lists do not yet support sublists.");	//TODO implement
	}

	/**Checks the given index to ensure that it is not below zero or at/above the size of the list.
	@param index The index to check.
	@param allowSize <code>false</code> if the given index itself should be checked, or <code>true</code> if only the previous indexes should be checked.
	@return The valid index.
	@exception IndexOutOfBoundsException if the index is less than zero or greater than {@link #size()} (if <code>allowSize</code> is <code>true</code>);
		or is greater than or equal to {@link #size()} (if <code>allowSize</code> is <code>false</code>).
	*/
	protected int checkIndexBounds(final int index, final boolean allowSize)
	{
		if(index<0)	//if this index is below zero, it's prima facie invalid
		{
			throw new IndexOutOfBoundsException("Index out of bounds: "+index);
		}
		readLock().lock();	//get a read lock
		try
		{
			for(int i=allowSize ? index-1 : index; i>=0; --i)	//make sure there are no missing indexes up to and including this value (or one less than the value, if we should allow the size index); if so, this index is out of bounds
			{
				if(!hasProperty(createOrdinalURI(index)))	//if there is no such index
				{
					throw new IndexOutOfBoundsException("Index out of bounds: "+index);
				}
			}
			return index;	//return the index, which we've verified to be valid
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Converts the given collection to an URF list resource.
	If the collection is already a list resource, the collection is returned;
	otherwise, a new new list resource with the contents of the collection is returned. 
	@param <T> The type of elements contained in the collection. 
	@param collection The collection to convert to a list resource.
	@return A list resource representing the contents of the given collection.
	*/
	public static <T extends URFResource> URFListResource<T> toListResource(final Collection<T> collection)
	{
		return collection instanceof URFListResource ? (URFListResource<T>)collection : new URFListResource<T>(collection);	//if the collection is already a list resource, return the collection as a list resource; otherwise create a new list resource with the contents of the collection
	}

}
