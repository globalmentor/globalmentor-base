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
import java.util.Collections;

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
		if(elements.length == 1) //if there is only one element
		{
			return new ObjectSet<E>(elements[0]); //return an immutable set containing only one object
		}
		//TODO check for Enum and return an EnumSet
		final Set<E> set = new HashSet<E>(); //create a new set
		Collections.addAll(set, elements); //add all the elements
		return Collections.unmodifiableSet(set); //wrap the set in an unmodifiable set
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
		//TODO check for Enum and return an EnumSet
		final Set<E> set = new HashSet<E>(collection); //create a new set, starting with the elements in the given collection
		Collections.addAll(set, elements); //add all the elements
		return Collections.unmodifiableSet(set); //wrap the set in an unmodifiable set
	}

	/**
	 * Creates a read-only copy of the given set. If the set is already read-only, the set itself is returned.
	 * @param <E> The type of element contained in the set.
	 * @param set The set which should be returned in read-only form.
	 * @throws NullPointerException if the given set is <code>null</code>.
	 */
	public static <E> Set<E> toImmutableSet(final Set<E> set) //TODO improve to return an ImmutableSet<E>
	{
		if(set instanceof ImmutableCollection) //if the set is already immutable TODO fix for Java's immutable sets
		{
			return set;
		}
		final int size = set.size(); //see how big the set is
		if(size == 1) //if the set only contains one element
		{
			return new ObjectSet<E>(set.iterator().next()); //return an immutable set containing only one object
		}
		return Collections.unmodifiableSet(new HashSet<E>(set)); //copy the set and wrap it in an unmodifiable set
	}

}
