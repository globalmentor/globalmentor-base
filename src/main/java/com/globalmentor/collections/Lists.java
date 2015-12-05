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

import static com.globalmentor.java.Arrays.*;
import static java.util.Arrays.*;
import static java.util.Collections.*;

import java.util.*;

/**
 * Utilities to be used with lists.
 * @author Garret Wilson
 */
public class Lists {

	/**
	 * Replaces the first occurrence of the given object with a new object. If the object does not appear in the list, no action is taken.
	 * @param <E> The type of element contained in the list.
	 * @param list The list in which the object is included.
	 * @param oldObject The object to replace.
	 * @param newObject The object to take the place of the old object.
	 */
	public static <E> void replace(final List<E> list, final E oldObject, final E newObject) {
		final int index = list.indexOf(oldObject); //get the index of the old object
		if(index >= 0) { //if the old item exists
			list.set(index, newObject); //replace the item at that index
		}
	}

	/**
	 * Creates a read-only list containing the given elements.
	 * @param <E> The type of element contained in the list.
	 * @param elements The elements to be contained in the list.
	 * @throws NullPointerException if the given array of elements is <code>null</code>.
	 */
	public static <E> List<E> immutableListOf(final E... elements) { //TODO improve to return an ImmutableList<E>
		return immutableListOf(elements, 0, elements.length);
	}

	/**
	 * Creates a read-only list containing the given elements in the given range.
	 * @param <E> The type of element contained in the list.
	 * @param elements The elements to be contained in the list.
	 * @param start the initial index of the range to be included, inclusive
	 * @param end the final index of the range to be included, exclusive.
	 * @throws NullPointerException if the given array of elements is <code>null</code>.
	 * @throws IllegalArgumentException if the start index is greater than the end index.
	 * @throws ArrayIndexOutOfBoundsException if the start index is less than zero or the end index is greater than the length.
	 */
	public static <E> List<E> immutableListOf(final E[] elements, final int start, final int end) { //TODO improve to return an ImmutableList<E>
		return immutableListOf(java.util.Collections.<E> emptyList(), elements, start, end);
	}

	/**
	 * Creates a read-only list containing the elements of the provided collection along with the given elements.
	 * @param <E> The type of element contained in the list.
	 * @param collection The existing collection to augment.
	 * @param elements The elements to be contained in the list.
	 * @param start the initial index of the range to be included, inclusive
	 * @param end the final index of the range to be included, exclusive.
	 * @throws NullPointerException if the given collection and/or array of elements is <code>null</code>.
	 */
	public static <E> List<E> immutableListOf(final Collection<? extends E> collection, final E... elements) { //TODO improve to return an ImmutableList<E>
		return immutableListOf(collection, elements, 0, elements.length);
	}

	/**
	 * Creates a read-only list containing the elements of the provided collection along with the given elements.
	 * @param <E> The type of element contained in the list.
	 * @param collection The existing collection to augment.
	 * @param elements The elements to be contained in the list.
	 * @param start the initial index of the range to be included, inclusive
	 * @param end the final index of the range to be included, exclusive.
	 * @throws NullPointerException if the given collection and/or array of elements is <code>null</code>.
	 * @throws IllegalArgumentException if the start index is greater than the end index.
	 * @throws ArrayIndexOutOfBoundsException if the start index is less than zero or the end index is greater than the length.
	 */
	public static <E> List<E> immutableListOf(final Collection<? extends E> collection, final E[] elements, final int start, final int end) { //TODO improve to return an ImmutableList<E>
		final int arrayLength = checkIndexRange(elements, start, end);
		if(collection.isEmpty()) { //if the collection is empty, take some shortcuts
			if(arrayLength == 0) { //if the list will be empty
				return emptyList(); //return the shared empty list
			} else if(arrayLength == 1) { //if there will only be one element
				return new ObjectList<E>(elements[start]); //return an immutable list containing only one object
			}
			return unmodifiableList(asList(copyOfRange(elements, start, end))); //clone the array defensively so that it won't be modified by the caller
		}
		if(arrayLength == 0) { //if no extra elements are given, take some shortcuts
			if(collection instanceof List && collection instanceof ImmutableCollection) { //if the collection is already an immutable list TODO fix for Java's immutable lists
				@SuppressWarnings("unchecked")
				final List<E> list = (List<E>)collection; //this is already an immutable list, so return it; it doesn't matter if it contains subclasses, we can use it as a List<E> because it is immutable
				return list;
			}
			final int size = collection.size(); //see how big the collection is
			if(size == 0) { //if the collection is empty
				return emptyList(); //return the shared empty list
			}
			if(size == 1) { //if the collection only contains one element
				return new ObjectList<E>(collection.iterator().next()); //return an immutable list containing only one object
			}
		}
		final List<E> newList = new ArrayList<E>(collection); //copy the collection
		addAll(newList, elements); //add all the extra elements
		return unmodifiableList(newList); //return an immutable version of the list
	}

	/**
	 * Creates and returns a mutable list containing the contents of the given iterator with the additional elements, if any.
	 * @param iterator The iterator a list of the contents of which should be returned.
	 * @param elements The additional elements, if any, to add to the list.
	 * @return A mutable list containing the indicated elements.
	 */
	public static <E> List<E> listOf(final Iterator<E> iterator, final E... elements) {
		final List<E> list = new ArrayList<E>();
		Collections.addAll(list, iterator);
		java.util.Collections.addAll(list, elements);
		return list;
	}

	/**
	 * Creates and returns a mutable list containing the contents of the given iterable with the additional elements, if any.
	 * @param iterable The iterable a list of the contents of which should be returned.
	 * @param elements The additional elements, if any, to add to the list.
	 * @return A mutable list containing the indicated elements.
	 */
	public static <E> List<E> listOf(final Iterable<E> iterable, final E... elements) {
		final List<E> list = new ArrayList<E>();
		Collections.addAll(list, iterable);
		java.util.Collections.addAll(list, elements);
		return list;
	}

	/**
	 * Returns a list to represent the given iterable. If the given iterable is a {@link List}, it will be returned. If the given iterable is not a {@link List},
	 * a temporary list will be created and filled with the contents of the given iterable.
	 * <p>
	 * In most cases the returned list should not be modified, as there are no guarantees of whether the list will be backed by existing data or whether the list
	 * will even be mutable.
	 * </p>
	 * @param <T> The type of elements contained in the iterable.
	 * @param iterable The iterable of elements.
	 * @return A list containing the elements of the given iterable.
	 */
	public static <T> List<T> toList(final Iterable<T> iterable) {
		if(iterable instanceof List) { //if the iterable is a list
			return (List<T>)iterable; //return the iterable itself
		} else { //we'll have to create a list
			final List<T> list; //we'll create a list, prepopulated if possible
			if(iterable instanceof Collection) { //if the iterable is a collection
				list = new ArrayList<T>((Collection<T>)iterable); //construct a list from the contents of the iterable
			} else { //if the iterable isn't a collection
				list = new ArrayList<T>(); //create a new list
				Collections.addAll(list, iterable); //fill the list with the contents of the iterable
			}
			return list; //return the list we created
		}
	}

}