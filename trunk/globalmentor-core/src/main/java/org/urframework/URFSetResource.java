/*
 * Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package org.urframework;

import java.util.*;
import java.net.URI;

import com.globalmentor.java.Arrays;

import static com.globalmentor.java.Objects.*;
import static org.urframework.URF.*;

/**An URF set resource that allows convenient access to its elements.
@param <E> The type of element stored in the set.
@author Garret Wilson
@see URF#ELEMENT_PROPERTY_URI
*/
public class URFSetResource<E extends URFResource> extends DefaultURFResource implements URFCollectionResource<E>, Set<E>
{

	/**Default constructor with no URI.*/
	public URFSetResource()
	{
		this((URI)null);	//create a resource without a URI
	}

	/**URI and type URIs constructor.
	If no types are specified, the type {@value URF#SET_CLASS_URI} will be added.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
	@param typeURIs The URIs of the types, if any, to add to the resource.
	*/
	public URFSetResource(final URI uri, final URI... typeURIs)
	{
		super(uri, typeURIs.length>0 ? typeURIs : new URI[]{SET_CLASS_URI});	//construct the parent class, specifying the set class if there are no types given
	}

	/**Collection constructor with no URI.
	The elements of the specified collection will be added to this set in the order they are returned by the collection's iterator.
	@param collection The collection whose elements are to be placed into this set.
	@exception NullPointerException if the specified collection is <code>null</code>.
	*/
	public URFSetResource(final Collection<? extends E> collection)
	{
		this(null, collection);	//construct the class with no URI
	}

	/**URI and collection constructor.
	The elements of the specified collection will be added to this set in the order they are returned by the collection's iterator.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
	@param collection The collection whose elements are to be placed into this set.
	@exception NullPointerException if the specified collection is <code>null</code>.
	*/
	public URFSetResource(final URI uri, final Collection<? extends E> collection)
	{
		this(uri);	//construct the class with the URI
		addAll(collection);	//add all the collection elements to the set
	}

	/**Returns the number of elements in this set (its cardinality).
	If this set contains more than {@link Integer#MAX_VALUE} elements, returns {@link Integer#MAX_VALUE}.
	@return The number of elements in this set (its cardinality).
	*/
	public int size()
	{
		final long count=getPropertyValueCount(ELEMENT_PROPERTY_URI);	//find out how many elements there are
		return count<Integer.MAX_VALUE ? (int)count : Integer.MAX_VALUE;	//return the value, with a ceiling of Integer.MAX_VALUE
	}

	/**Returns <code>true</code> if this set contains no elements./
	@return <code>true</code> if this set contains no elements
	*/
	public boolean isEmpty()
	{
		return !hasProperty(ELEMENT_PROPERTY_URI);	//the set is empty if there are no elements
	}

	/**Returns <code>true</code> if this set contains the specified element.
	@param object The element whose presence in this set is to be tested.
	@return <code>true</code> if this set contains the specified element.
	@throws ClassCastException if the type of the specified element is incompatible with this set.
	@throws NullPointerException if the specified element is <code>null</code> and this set does not permit <code>null</code> elements.
	*/
	public boolean contains(final Object object)
	{
		return hasPropertyValue(ELEMENT_PROPERTY_URI, checkInstance((URFResource)object, "URF sets do not support null elements."));
	}

	/**Returns an iterator over the elements in this set.
	The elements are returned in no particular order unless the element properties are ordered..
	@return An iterator over the elements in this set.
	*/
	public Iterator<E> iterator()
	{
		return (Iterator<E>)getPropertyValues(ELEMENT_PROPERTY_URI).iterator();	//return an iterator to property values
	}

	/**Returns an array containing all of the elements in this set in the same order they would be iterated.
	@return An array containing all the elements in this set.
	*/
	public Object[] toArray()
	{
		readLock().lock();	//get a read lock
		try
		{
			final int count=(int)getPropertyValueCount(ELEMENT_PROPERTY_URI);	//get the number of elements
			final Object[] array=new Object[count];	//create a new array of the correct size
			int index=0;	//start at the first index
			for(final URFResource element:getPropertyValues(ELEMENT_PROPERTY_URI))	//for each element
			{
				assert index<count : "The size of the set changed during iteration, even with a read lock.";
				array[index++]=element;	//store the element in the array and advance the index
			}
			return array;	//return the array
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Returns an array containing all of the elements in this set; the runtime type of the returned array is that of the specified array.
	If the set fits in the specified array, it is returned therein.
	Otherwise, a new array is allocated with the runtime type of the specified array and the size of this set.
	<p>If this set fits in the specified array with room to spare (i.e., the array has more elements than this set),
	the element in the array immediately following the end of the set is set to <code>null</code>.</p>
	@param array The array into which the elements of this set are to be stored, if it is big enough;
		otherwise, a new array of the same runtime type is allocated for this purpose.
	@return An array containing all the elements in this set.
	@throws ArrayStoreException if the runtime type of the specified array is not a supertype of the runtime type of every element in this set.
	@throws NullPointerException if the specified array is <code>null</code>
	*/
	public <T> T[] toArray(T[] array)
	{
		readLock().lock();	//get a read lock
		try
		{
			final int count=(int)getPropertyValueCount(ELEMENT_PROPERTY_URI);	//get the number of elements
			array=Arrays.getArray(array, count);	//make sure our array is large enough
			int index=0;	//start at the first index
			for(final URFResource element:getPropertyValues(ELEMENT_PROPERTY_URI))	//for each element
			{
				assert index<count : "The size of the set changed during iteration, even with a read lock.";
				array[index++]=(T)element;	//store the element in the array and advance the index
			}
			if(index<array.length)	//if we haven't reached the end of the array
			{
				array[index]=null;	//add a null to indicate the end of the array, as per the toArray(T[]) API
			}
			return array;	//return the array
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

  // Modification Operations

	/**Adds the specified element to this set if it is not already present.
	@param element The element to be added to this set.
	@return <code>true</code> if this set did not already contain the specified element.
	@throws ClassCastException if the class of the specified element prevents it from being added to this set.
	@throws NullPointerException if the specified element is <code>null</code> and this set does not permit <code>null</code> elements.
	@throws IllegalArgumentException if some property of the specified element prevents it from being added to this set.
	*/
	public boolean add(final E element)
	{
		return addPropertyValue(ELEMENT_PROPERTY_URI, element);	//add the element, returning whether this set was modified
	}

	/**Removes the specified element from this set if it is present.
	@param object The object to be removed from this set, if present.
	@return <code>true</code> if this set contained the specified element.
	@throws ClassCastException if the type of the specified element is incompatible with this set.
	@throws NullPointerException if the specified element is <code>null</code> and this set does not permit <code>null</code> elements.
	*/
	public boolean remove(final Object object)
	{
		return removePropertyValue(ELEMENT_PROPERTY_URI, (URFResource)object);	//remove the value, returning whether that modified this set
	}

  // Bulk Operations

	/**Returns <code>true</code> if this set contains all of the elements of the specified collection.
	If the specified collection is also a set, this method returns <code>true</code> if it is a <dfn>subset</dfn> of this set.
	@param collection The collection to be checked for containment in this set.
	@return <code>true</code> if this set contains all of the elements of the specified collection.
	@throws ClassCastException if the types of one or more elements in the specified collection are incompatible with this set
	@throws NullPointerException if the specified collection contains one or more <code>null</code> elements and this set does not permit <code>null</code> elements (optional), or if the specified collection is <code>null</code>.
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

  /**Adds all of the elements in the specified collection to this set if they're not already present.
	If the specified collection is also a set, the {@link #addAll(Collection)} operation effectively
	modifies this set so that its value is the <dfn>union</dfn> of the two.
	The behavior of this operation is undefined if the specified collection is modified while the operation is in progress.
	@param collection The collection containing elements to be added to this set.
	@return <code>true</code> if this set changed as a result of the call.
	@throws ClassCastException if the class of an element of the specified collection prevents it from being added to this set.
	@throws NullPointerException if the specified collection contains one or more <code>null</code> elements and this set does not permit <code>null</code> elements, or if the specified collection is null.
	@throws IllegalArgumentException if some property of an element of the specified collection prevents it from being added to this set.
	@see #add(Object)
	*/
  public boolean addAll(final Collection<? extends E> collection)
	{
		boolean modified=false;	//we haven't been modified, yet
		writeLock().lock();	//get a write lock
		try
		{
			for(final E element:collection)	//for each element in the other collection
			{
				if(add(element))	//add this element; if this resulted in a modification
				{
					modified=true;	//we modified the set
				}
			}
			return modified;	//return whether this set was modified
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

  /**Retains only the elements in this set that are contained in the specified collection.
	In other words, removes from this set all of its elements that are not contained in the specified collection.
	If the specified collection is also a set, this peration effectively modifies this set so that its value is the <dfn>intersection</dfn> of the two sets.
	@param collection The collection containing elements to be retained in this set.
	@return <code>true</code> if this set changed as a result of the call.
	@throws ClassCastException if the class of an element of this set is incompatible with the specified collection.
	@throws NullPointerException if this set contains a <code>null</code> element and the specified collection does not permit <code>null</code> elements.
	@see #remove(Object)
	*/
	public boolean retainAll(final Collection<?> collection)
	{
		boolean modified=false;	//we haven't been modified, yet
		writeLock().lock();	//get a write lock
		try
		{
			final Iterator<URFResource> elementIterator=getPropertyValues(ELEMENT_PROPERTY_URI).iterator();	//get an iterator to the elements; iterate this set only once because that operation is probably the least efficient
			while(elementIterator.hasNext())	//while there are more values
			{
				final URFResource element=elementIterator.next();	//get the next element
				if(!collection.contains(element))	//if the collection does not contain this element
				{
					elementIterator.remove();	//remove this element
					modified=true;	//we modified the set
				}
			}
			return modified;	//return whether this set was modified
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

  /**Removes from this set all of its elements that are contained in the specified collection.
	If the specified collection is also a set, this operation effectively modifies this set so that
	its value is the <dfn>asymmetric set difference</dfn> of the two sets.
	@param collection The collection containing elements to be removed from this set.
	@return <code>true</code> if this set changed as a result of the call.
	@throws ClassCastException if the class of an element of this set is incompatible with the specified collection.
	@throws NullPointerException if this set contains a <code>null</code> element and the specified collection does not permit <code>null</code> elements or if the specified collection is <code>null</code>.
	@see #remove(Object)
	@see #contains(Object)
	*/
	public boolean removeAll(final Collection<?> collection)
	{
		boolean modified=false;	//we haven't been modified, yet
		writeLock().lock();	//get a write lock
		try
		{
			final Iterator<URFResource> elementIterator=getPropertyValues(ELEMENT_PROPERTY_URI).iterator();	//get an iterator to the elements; iterate this set only once because that operation is probably the least efficient
			while(elementIterator.hasNext())	//while there are more values
			{
				final URFResource element=elementIterator.next();	//get the next element
				if(collection.contains(element))	//if the collection contain this element
				{
					elementIterator.remove();	//remove this element
					modified=true;	//we modified the set
				}
			}
			return modified;	//return whether this set was modified
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

	/**Removes all of the elements from this set.
	The set will be empty after this call returns.
	*/
	public void clear()
	{
		removePropertyValues(ELEMENT_PROPERTY_URI);	//remove all the elements
	}

  // Comparison and hashing

  /**Compares the specified object with this set for equality.
	Returns <code>true</code> if the specified object is also a set, the two sets have the same size,
	and every member of the specified set is contained in this set (or equivalently, every member of this set is contained in the specified set).
	@param object The object to be compared for equality with this set.
	@return <code>true</code> if the specified object is equal to this set.
	*/
  public boolean equals(final Object object)
  {
  	if(object==this)	//if we're being compared to ourselves
  	{
  		return true;	//this object always equals itself
  	}
  	if(object instanceof Set)	//if the object is a set
  	{
  		final Set<?> set=(Set<?>)object;	//cast the object to a set
  		readLock().lock();	//get a read lock
  		try
  		{
  			if(size()==set.size())	//if the sets are the same size
  			{
	  			for(final URFResource element:getPropertyValues(ELEMENT_PROPERTY_URI))	//for each element; iterate this set only once because that operation is probably the least efficient
	  			{
	  				if(!set.contains(element))	//if the given set does not contain this element
	  				{
	  					return false;	//the sets are not equal
	  				}
	  			}
	  			return true;	//the sets are the same size and contain the same elements
  			}
  		}
  		finally
  		{
  			readLock().unlock();	//always release the read lock
  		}
  	}
  	return false;	//either the other object wasn't a set, or it wasn't the same size as this set
  }

  /**Returns the hash code value for this set.
	The hash code of a set is defined to be the sum of the hash codes of the elements in the set,
	where the hash code of a <code>null</code> element is defined to be zero.
	@return The hash code value for this set.
	@see Object#equals(Object)
	@see Set#equals(Object)
	*/
	public int hashCode()
	{
		int hashCode=0;	//start with a hash code of 0, according to the set contract
		readLock().lock();	//get a read lock
		try
		{
			for(final URFResource element:getPropertyValues(ELEMENT_PROPERTY_URI))	//for each element
			{
				if(element!=null)	//if this element is not null
				{
					hashCode+=element.hashCode();	//add this hash code to our total
				}
			}
			return hashCode;	//return the calculated hash code
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Converts the given collection to an URF set resource.
	If the collection is already a set resource, the collection is returned;
	otherwise, a new set resource with the contents of the collection is returned. 
	@param <T> The type of elements contained in the collection. 
	@param collection The collection to convert to a set resource.
	@return A set resource representing the contents of the given collection.
	*/
	public static <T extends URFResource> URFSetResource<T> toSetResource(final Collection<T> collection)
	{
		return collection instanceof URFSetResource ? (URFSetResource<T>)collection : new URFSetResource<T>(collection);	//if the collection is already a set resource, return the collection as a set resource; otherwise create a new set resource with the contents of the collection
	}

}
