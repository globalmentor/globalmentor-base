/*
 * Copyright © 1996-2012 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.collections;

import static com.globalmentor.collections.iterables.Iterables.*;
import static java.util.Objects.*;
import static java.util.stream.Collectors.*;
import static java.util.stream.Stream.*;

import java.util.*;

import javax.annotation.*;

/**
 * Utilities to be used with sets.
 * @author Garret Wilson
 */
public class Sets {

	/**
	 * Returns some set representing the union of two given sets.
	 * @implNote No guarantees are made about whether the returned set is mutable, nor whether the returned set is a new instance or a reference to one of the
	 *           given sets.
	 * @param <T> The common type found in the sets.
	 * @param set1 The first set of the union.
	 * @param set2 The second set of the union.
	 * @return A set containing the logical union of the contents of the given two sets.
	 */
	public static <T> Set<T> union(@Nonnull final Set<T> set1, @Nonnull final Set<T> set2) {
		if(set1.isEmpty()) {
			return requireNonNull(set2);
		}
		if(set2.isEmpty()) {
			return set1;
		}
		return concat(set1.stream(), set2.stream()).collect(toUnmodifiableSet());
	}

	/**
	 * Returns a set representing the union of the given collection and another collection. The returned union set will be a copy of the original collections.
	 * <code>null</code> elements are not supported.
	 * @param <E> The common type of element found in the collections.
	 * @param collection1 The first collection of the union.
	 * @param collection2 The second collection of the union.
	 * @return A set containing the logical union of the two collections.
	 * @throws NullPointerException if either of the collections is <code>null</code>.
	 * @see Set#copyOf(Collection)
	 */
	public static <E> Set<E> unionCopyOf(@Nonnull final Collection<? extends E> collection1, @Nonnull final Collection<? extends E> collection2) {
		if(collection1.isEmpty()) {
			if(collection2.isEmpty()) {
				return Set.of();
			}
			return Set.copyOf(collection2);
		}
		if(collection2.isEmpty()) {
			return Set.copyOf(collection1);
		}
		return concat(collection1.stream(), collection2.stream()).collect(toUnmodifiableSet());
	}

	/**
	 * Returns a set representing the union of the given collection and another element. The returned set will be a copy of the original collection and the given
	 * element, in effect adding the element to the union set. <code>null</code> elements are not supported.
	 * @param <E> The common type of element found in the collections.
	 * @param collection The collection to copy and to which to add an element.
	 * @param element The element to add to the collection.
	 * @return A set containing the logical union of the given collection and some set containing the given element.
	 * @throws NullPointerException if the collection and/or element is <code>null</code>.
	 * @see Set#copyOf(Collection)
	 */
	public static <E> Set<E> unionCopyOf(@Nonnull final Collection<? extends E> collection, @Nonnull final E element) {
		if(collection.isEmpty()) {
			return Set.of(element);
		}
		return toStreamConcat(collection, element).collect(toUnmodifiableSet());
	}

}
