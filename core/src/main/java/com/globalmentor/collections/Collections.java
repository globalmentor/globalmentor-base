/*
 * Copyright © 1996-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.text.TextFormatter.*;

/**
 * Various utilities to be used with collections.
 * @author Garret Wilson
 * @see java.util.Collection
 */
public class Collections {

	/**
	 * Adds the string representation of the elements specified by the collection to the specified string collection.
	 * @param stringCollection The collection to which objects will be added.
	 * @param collection The source of the added strings.
	 * @return <code>true</code> if the collection changed as a result of the call.
	 */
	public static boolean addAll(final Collection<? super String> stringCollection, final Collection<?> collection) {
		boolean changed = false; //assume we won't modify the collection
		for(final Object object : collection) { //for each object in the collection
			if(stringCollection.add(object.toString())) { //get the string value of the object add it to the collection; if the collection changed
				changed = true; //show that we modified the collection				
			}
		}
		return changed; //return whether or not we changed the collection
	}

	/**
	 * Adds all of the elements specified by the given iterable to the specified collection.
	 * @param <T> The type of the elements contained on the collection.
	 * @param collection The collection to which objects will be added.
	 * @param iterable The source of the added objects.
	 * @return <code>true</code> if the collection changed as a result of the call.
	 */
	public static <T> boolean addAll(final Collection<T> collection, final Iterable<? extends T> iterable) {
		return addAll(collection, iterable.iterator()); //get an iterator and add all the elements
	}

	/**
	 * Adds all of the elements specified by the given iterator to the specified collection.
	 * @param <T> The type of the elements contained on the collection.
	 * @param collection The collection to which objects will be added.
	 * @param iterator The source of the added objects.
	 * @return <code>true</code> if the collection changed as a result of the call.
	 */
	public static <T> boolean addAll(final Collection<T> collection, final Iterator<? extends T> iterator) {
		boolean changed = false; //assume we won't modify the collection
		while(iterator.hasNext()) { //while there are more items in the iterator
			if(collection.add(iterator.next())) { //get the next item from the iterator and add it to the collection; if the collection changed
				changed = true; //show that we modified the collection				
			}
		}
		return changed; //return whether or not we changed the collection
	}

	/**
	 * Removes all of the elements specified by the given iterable from the specified collection.
	 * @param <T> The type of the elements contained on the collection.
	 * @param collection The collection from which objects will be removed.
	 * @param iterable The source of the removed objects.
	 * @return <code>true</code> if the collection changed as a result of the call.
	 */
	public static <T> boolean removeAll(final Collection<T> collection, final Iterable<? extends T> iterable) {
		return removeAll(collection, iterable.iterator()); //get an iterator and remove all the elements
	}

	/**
	 * Removes all of the elements specified by the given iterator from the specified collection.
	 * @param <T> The type of the elements contained on the collection.
	 * @param collection The collection from which objects will be removed.
	 * @param iterator The source of the removed objects.
	 * @return <code>true</code> if the collection changed as a result of the call.
	 */
	public static <T> boolean removeAll(final Collection<T> collection, final Iterator<? extends T> iterator) {
		boolean changed = false; //assume we won't modify the collection
		while(iterator.hasNext()) { //while there are more items in the iterator
			if(collection.remove(iterator.next())) { //get the next item from the iterator and remove it from the collection; if the collection changed
				changed = true; //show that we modified the collection				
			}
		}
		return changed; //return whether or not we changed the collection
	}

	/**
	 * Determines if the given collection contains an object that is an instance of the given class.
	 * @param <T> The type of the elements contained on the collection.
	 * @param collection The collection to search.
	 * @param objectClass The class for which to find an implementing object.
	 * @return <code>true</code> if the collection contains an object that implements the given class, else <code>false</code>.
	 */
	public static <T> boolean containsInstance(final Collection<T> collection, final Class<? extends T> objectClass) {
		for(final T item : collection) { //for each item in the collection
			if(objectClass.isInstance(item)) { //if the next item is an instance of the class
				return true; //show that the collection contains an instance of the the given class
			}
		}
		return false; //show that we didn't find any instances of the given class
	}

	/**
	 * Creates a hash set of the given generic type and initializes it with the given elements.
	 * @param <E> The type of elements the hash set will contain.
	 * @param elements The elements with which to initialize the hash set.
	 * @return A new hash set containing the given elements.
	 */
	public static <E> HashSet<E> createHashSet(final E... elements) {
		final HashSet<E> hashSet = new HashSet<E>(elements.length); //create a new hash set large enough to store the given elements
		java.util.Collections.addAll(hashSet, elements); //add all of the given elements to the hash set
		return hashSet; //return the hash set we created and initialized
	}

	/**
	 * Retrieves the first iterated object, if any, from the collection.
	 * @param <T> The type of object stored in the collection.
	 * @param collection The collection from which the object should be retrieved.
	 * @return The first iterated object from the collection, or <code>null</code> if the collection is empty.
	 * @throws NullPointerException if the given collection is <code>null</code>.
	 * @see Collection#isEmpty()
	 * @see Collection#iterator()
	 */
	public static <T> T get(final Collection<T> collection) {
		return collection.isEmpty() ? null : collection.iterator().next();
	}

	/**
	 * Retrieves the first iterated object, if any, from the iterable.
	 * @param <T> The type of object stored in the iterable.
	 * @param iterable The iterable from which the object should be retrieved.
	 * @return The first iterated object from the iterable, or <code>null</code> if the iterable is empty.
	 * @throws NullPointerException if the given iterable is <code>null</code>.
	 * @see Collection#isEmpty()
	 * @see Iterable#iterator()
	 */
	public static <T> T get(final Iterable<T> iterable) {
		if(iterable instanceof Collection && ((Collection<T>)iterable).isEmpty()) { //if the iterable is an empty collection
			return null; //short circuit the tests; there is nothing to return
		}
		final Iterator<T> iterator = iterable.iterator();
		return iterator.hasNext() ? iterator.next() : null;
	}

	/**
	 * Sets the contents of the collection to the contents of the other given collection. This is a convenience method for {@link Collection#clear()} followed by
	 * {@link Collection#addAll(Collection)}.
	 * @param <T> The type of objects in the collection.
	 * @param collection The collection to set.
	 * @param newCollection The elements to set in the collection.
	 * @return <code>true</code> if the collection changed as a result of the call.
	 */
	public static <T> boolean set(final Collection<T> collection, final Collection<? extends T> newCollection) {
		boolean changed = false;
		if(!collection.isEmpty()) {
			collection.clear(); //clear the contents of the collection
			changed = true;
		}
		if(collection.addAll(newCollection)) { //add all the contents of the new collection
			changed = true;
		}
		return changed;
	}

	/**
	 * Sets the contents of the collection to the contents of the other given collection. This is a convenience method for {@link Collection#clear()} followed by
	 * {@link java.util.Collections#addAll(Collection, Object...)}.
	 * @param <T> The type of objects in the collection.
	 * @param collection The collection to set.
	 * @param newElements The elements to set in the collection.
	 * @return <code>true</code> if the collection changed as a result of the call.
	 */
	public static <T> boolean set(final Collection<T> collection, final T... newElements) {
		boolean changed = false;
		if(!collection.isEmpty()) {
			collection.clear(); //clear the contents of the collection
			changed = true;
		}
		if(java.util.Collections.addAll(collection, newElements)) { //add all the contents of the new collection
			changed = true;
		}
		return changed;
	}

	/**
	 * Determines whether the iterable is empty.
	 * @param <T> The type of object stored in the iterable.
	 * @param iterable The iterable from which the object would be retrieved.
	 * @return <code>true</code> if an iterator returned from the iterable would return zero elements.
	 * @throws NullPointerException if the given iterable is <code>null</code>.
	 * @see Collection#isEmpty()
	 * @see Iterable#iterator()
	 */
	public static <T> boolean isEmpty(final Iterable<T> iterable) {
		if(iterable instanceof Collection) { //if the iterable is a collection
			return ((Collection<?>)iterable).isEmpty(); //delegate to the collection
		}
		final Iterator<T> iterator = iterable.iterator();
		return iterator.hasNext() ? false : true; //return true if there is no next element
	}

	/**
	 * Sorts the specified collection into ascending order, according to the <dfn>natural ordering</dfn> of its elements, and returns an array with the results.
	 * @param collection The collection to be sorted.
	 * @return An array containing the sorted contents of the collection.
	 * @throws ClassCastException if the list contains elements that are not <dfn>mutually comparable</dfn> (for example, strings and integers).
	 * @see Comparable
	 */
	/*TODO fix when toArray() is fixed
		public static <T extends Comparable<? super T>> T[] getSorted(final Collection<T> collection)
		{
			final T[] array=toArray(collection);	//convert the collection to an array
			Arrays.sort(array);	//sort the array
			return array;	//return the array
		}
	*/

	/**
	 * Sorts the specified collection according to the order induced by the specified comparator.
	 * @param collection The collection to be sorted.
	 * @param comparator The comparator to determine the order of the list, or <code>null</code> if the elements' <dfn>natural ordering</dfn> should be used.
	 * @return An array containing the sorted contents of the collection.
	 * @throws ClassCastException if the list contains elements that are not <dfn>mutually comparable</dfn> using the specified comparator.
	 * @see Comparator
	 */
	/*TODO fix when toArray() is fixed
		public static <T> T[] getSorted(final Collection<T> collection, final Comparator<? super T> comparator)
		{
			final T[] array=toArray(collection);	//convert the collection to an array
			Arrays.sort(array, comparator);	//sort the array
			return array;	//return the array
	  }
	*/

	/**
	 * Converts the collection to a generics array.
	 * @param collection The collection to convert to an array.
	 * @return An array containing all the elements in the collection.
	 */
	/*TODO fix
	  public static <T> T[] toArray(final Collection<T> collection)
	  {
	//TODO fix  	final T[] emptyArray=createArray();	//create an empty generics-aware array
	//TODO fix  	return collection.toArray(emptyArray);	//pass the array to the collection, which will create a new array as necessary
	  	final T[] array;	//we'll create the array and store it here
	  	if(collection.isEmpty()) {	//if the collection is empty
	  		array=createArray();	//create an empty generics-aware array TODO check to make sure this is actually of the correct type
	  	}
	  	else {	//if the collection is not empty
	  		final T element=collection.iterator().next();	//get the first element in the collection
	  		final Class<T> elementType	this will not work, as we don't know if this class is representational of all elements in the collection
	  	}
	  	return collection.toArray(array);	//convert the collection to an array
	  }
	*/

	/**
	 * Converts an iterable to a collection. If the iterable is already a collection, it is returned as such. If the the iterable is not already a collection, a
	 * new collection is returned with the contents of the iterable.
	 * @param <T> The type of object in the iterable.
	 * @param iterable The iterable to convert.
	 * @return A collection with the current contents of the iterable.
	 */
	public static <T> Collection<T> toCollection(final Iterable<T> iterable) {
		if(iterable instanceof Collection) { //if the iterable is already a collection
			return (Collection<T>)iterable; //return it
		}
		final List<T> list = new ArrayList<T>();
		addAll(list, iterable);
		return list;
	}

	/**
	 * Converts a collection to a string by concatenating the string values of each member of the collection, separated by a comma.
	 * @param <T> The type of the elements contained on the collection.
	 * @param collection The collection to convert to a string.
	 * @return A string representation of the collection of elements.
	 */
	public static <T> String toString(final Collection<T> collection) {
		return toString(collection, ','); //construct a string using a comma
	}

	/**
	 * Converts a collection to a string by concatenating the string values of each member of the collection, separated by the given delimiter character.
	 * @param <T> The type of the elements contained on the collection.
	 * @param collection The collection to convert to a string.
	 * @param delimiter The character to place between elements.
	 * @return A string representation of the collection of elements.
	 */
	public static <T> String toString(final Collection<T> collection, final char delimiter) {
		return formatList(delimiter, collection).toString(); //format the list into a string buffer and return the resulting string
	}

	/**
	 * Converts a collection to a string by concatenating the string values of each member of the collection, separated by the given delimiter.
	 * @param <T> The type of the elements contained on the collection.
	 * @param collection The collection to convert to a string.
	 * @param delimiter The character sequence to place between elements.
	 * @return A string representation of the collection of elements.
	 */
	public static <T> String toString(final Collection<T> collection, final String delimiter) {
		return formatList(delimiter, collection).toString(); //format the list into a string buffer and return the resulting string
	}

}
