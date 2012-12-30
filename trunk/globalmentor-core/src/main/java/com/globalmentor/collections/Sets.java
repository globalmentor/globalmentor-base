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

import static java.util.Collections.*;

import java.util.*;

/**
 * Utilities to be used with sets.
 * @author Garret Wilson
 */
public class Sets
{

	/**
	 * Creates a read-only set containing the given elements.
	 * @param <E> The type of element contained in the set.
	 * @param elements The elements to be contained in the set.
	 * @throws NullPointerException if the given array of elements is <code>null</code>.
	 */
	public static <E> Set<E> immutableSetOf(final E... elements) //TODO improve to return an ImmutableSet<E>
	{
		return immutableSetOf(java.util.Collections.<E> emptySet(), elements);
	}

	/**
	 * Creates a read-only set containing the elements of the provided iterable along with the given elements.
	 * @param <E> The type of element contained in the set.
	 * @param iterable The existing iterable to augment.
	 * @param elements The elements to be contained in the set.
	 * @throws NullPointerException if the given iterable and/or array of elements is <code>null</code>.
	 */
	public static <E> Set<E> immutableSetOf(final Iterable<? extends E> iterable, final E... elements) //TODO improve to return an ImmutableSet<E>
	{
		if(iterable instanceof Collection<?>) //if a collection was given
		{
			return immutableSetOf((Collection<? extends E>)iterable, elements); //delegate to the collection version
		}
		@SuppressWarnings("unchecked")
		final Iterator<E> iterator = (Iterator<E>)iterable.iterator();
		if(!iterator.hasNext()) //if the iterable is empty, delegate to the collection version with no elements in the collection
		{
			return immutableSetOf(java.util.Collections.<E> emptySet(), elements);
		}
		E object = null; //we'll store an object here if we have one while doing checks
		if(elements.length == 0) //if no extra elements are given, take some shortcuts
		{
			if(!iterator.hasNext()) //if there are no iterable elements, either
			{
				return emptySet(); //return the shared empty set
			}
			object = iterator.next(); //get the first object
			if(!iterator.hasNext()) //if there is only one object
			{
				return new ObjectSet<E>(object); //return an immutable set containing only one object
			}
		}
		final Set<E> newSet = new HashSet<E>(); //use a normal set TODO improve for enums
		if(object != null) //if we have an object to add
		{
			newSet.add(object);
		}
		Collections.addAll(newSet, iterator); //add all the elements in the iterator
		addAll(newSet, elements); //add all the extra elements
		return unmodifiableSet(newSet); //wrap the set in an unmodifiable set
	}

	/**
	 * Creates a read-only set containing the elements of the provided collection along with the given elements.
	 * @param <E> The type of element contained in the set.
	 * @param collection The existing collection to augment.
	 * @param elements The elements to be contained in the set.
	 * @throws NullPointerException if the given collection and/or array of elements is <code>null</code>.
	 */
	public static <E> Set<E> immutableSetOf(final Collection<? extends E> collection, final E... elements) //TODO improve to return an ImmutableSet<E>
	{
		if(collection.isEmpty()) //if the collection is empty, take some shortcuts
		{
			final int size = elements.length; //find out the size of the set we will create for the elements
			if(size == 0) //if the set will be empty
			{
				return emptySet(); //return the shared empty set
			}
			final E element = elements[0]; //get the first element
			if(size == 1) //if there is only one element
			{
				return new ObjectSet<E>(element); //return an immutable set containing only one object
			}
			Set<E> set = null;
			if(collection instanceof EnumSet<?>) //if the collection is an EnumSet, just clone it
			{
				@SuppressWarnings("unchecked")
				final Set<E> enumSet = (Set<E>)((EnumSet<?>)collection).clone();
				set = enumSet;
			}
			else if(element instanceof Enum) //if the elements are enums
			{
				final Class<?> enumClass = element.getClass(); //get the class of enum we're dealing with
				boolean areAllSameEnums = true; //make sure all elements are of the same class
				for(int i = elements.length - 1; i > 0; --i) //look at all the other elements
				{
					final E enumElement = elements[i];
					if(enumElement != null && !enumClass.isInstance(enumElement)) //if this isn't an enum of the same type
					{
						areAllSameEnums = false;
						break;
					}
				}
				if(areAllSameEnums)
				{
					final Enum<?>[] enumElements = (Enum<?>[])elements;
					@SuppressWarnings({ "unchecked", "rawtypes" })
					final Set<E> enumSet = (Set<E>)EnumSet.<Enum> of(enumElements[0], enumElements);
					set = enumSet;
				}
			}
			if(set == null) //if the elements are of any other type
			{
				set = new HashSet<E>(); //create a new set
				addAll(set, elements); //add all the elements
			}
			return unmodifiableSet(set); //wrap the set in an unmodifiable set
		}
		if(elements.length == 0) //if no extra elements are given, take some shortcuts
		{
			if(collection instanceof Set && collection instanceof ImmutableCollection) //if the collection is already an immutable set TODO fix for Java's immutable sets
			{
				@SuppressWarnings("unchecked")
				final Set<E> set = (Set<E>)collection; //this is already an immutable set, so return it; it doesn't matter if it contains subclasses, we can use it as a Set<E> because it is immutable
				return set;
			}
			final int size = collection.size(); //see how big the collection is
			if(size == 0) //if the collection is empty
			{
				return emptySet(); //return the shared empty set
			}
			if(size == 1) //if the collection only contains one element
			{
				return new ObjectSet<E>(collection.iterator().next()); //return an immutable set containing only one object
			}
		}
		final Set<E> newSet;
		if(collection instanceof EnumSet) //if the collection is an EnumSet
		{
			@SuppressWarnings("unchecked")
			final Set<E> newEnumSet = (Set<E>)EnumSet.copyOf((EnumSet<?>)collection);
			newSet = newEnumSet;
		}
		else
		//if we don't know of any enums
		{
			newSet = new HashSet<E>(); //use a normal set
		}
		newSet.addAll(collection); //add all the elements
		addAll(newSet, elements); //add all the extra elements
		return unmodifiableSet(newSet); //wrap the set in an unmodifiable set
	}

}
