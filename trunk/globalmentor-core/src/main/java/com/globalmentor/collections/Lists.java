/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

/**
 * Utilities to be used with lists.
 * @author Garret Wilson
 */
public class Lists
{

	/**
	 * Replaces the first occurrence of the given object with a new object. If the object does not appear in the list, no action is taken.
	 * @param <E> The type of element contained in the list.
	 * @param list The list in which the object is included.
	 * @param oldObject The object to replace.
	 * @param newObject The object to take the place of the old object.
	 */
	public static <E> void replace(final List<E> list, final E oldObject, final E newObject)
	{
		final int index = list.indexOf(oldObject); //get the index of the old object
		if(index >= 0) //if the old item exists
		{
			list.set(index, newObject); //replace the item at that index
		}
	}

	/**
	 * Appends the contents of a collection to a list. This method functions identical to {@link List#addAll(Collection)}, except that this method returns the
	 * given list. This is useful for chaining methods.
	 * @param <E> The type of element contained in the list.
	 * @param collection The collection to add to the list.
	 * @return The given list.
	 */
	public static <E> List<E> addAll(final List<E> list, final Collection<? extends E> collection)
	{
		list.addAll(collection);
		return list;
	}

	/**
	 * Creates a read-only list with the given elements.
	 * @param <E> The type of element the list will contain.
	 * @param elements The elements to add to the new list.
	 * @return A read-only list containing the given elements.
	 */
	public static <E> List<E> createReadOnlyList(final E... elements)
	{
		return java.util.Collections.unmodifiableList(java.util.Arrays.asList(elements.clone())); //clone the array defensively so that it won't be modified by the caller
	}

	/**
	 * Creates a read-only list with the contents of the given collection.
	 * @param <E> The type of element the list will contain.
	 * @param collection The collection to add to the new list.
	 * @return A read-only list with the contents of the given collection.
	 */
	public static <E> List<E> createReadOnlyList(final Collection<? extends E> collection)
	{
		return java.util.Collections.unmodifiableList(addAll(new ArrayList<E>(), collection));
	}

	/**
	 * Returns a list to represent the given iterable. If the given iterable is a {@link List}, it will be returned. If the given iterable is not a {@link List},
	 * a temporary list will be created and filled with the contents of the given iterable.
	 * @param <T> The type of elements contained in the iterable.
	 * @param iterable The iterable of elements.
	 * @return A list containing the elements of the given iterable.
	 */
	public static <T> List<T> toList(final Iterable<T> iterable)
	{
		if(iterable instanceof List) //if the iterable is a list
		{
			return (List<T>)iterable; //return the iterable itself
		}
		else
		//we'll have to create a list
		{
			final List<T> list; //we'll create a list, prepopulated if possible
			if(iterable instanceof Collection) //if the iterable is a collection
			{
				list = new ArrayList<T>((Collection<T>)iterable); //construct a list from the contents of the iterable
			}
			else
			//if the iterable isn't a collection
			{
				list = new ArrayList<T>(); //create a new list
				Collections.addAll(list, iterable); //fill the list with the contents of the iterable
			}
			return list; //return the list we created
		}
	}

}