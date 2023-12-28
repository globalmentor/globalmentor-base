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

import static com.globalmentor.collections.iterators.Iterators.*;
import static java.util.Collections.*;
import static java.util.Objects.*;

import java.util.*;

import javax.annotation.*;

import com.globalmentor.collections.iterators.Iterators;

/**
 * Utilities to be used with lists.
 * @author Garret Wilson
 */
public final class Lists {

	private Lists() {
	}

	/**
	 * Provides a reversed view of the list by returning an iterable that produces iterators that iterate from the end to the start of the list. The list itself
	 * is not changed.
	 * @apiNote The name "reverse" was not chosen because the given list itself is not reversed; nor is a complete {@link List} view returned as with Guava's
	 *          {@code Lists.reverse(List<T>)}. The name "reversing" implies a continual process; a reversed iterator will be supplied as many times as needed, as
	 *          is typical with {@link Iterable}.
	 * @param <E> The type of element contained in the list.
	 * @param list The list to be iterated in reverse order.
	 * @return An iterable that supplies a reversed iterator to the list.
	 * @see Iterators#reverse(ListIterator)
	 * @see <a href="https://guava.dev/releases/snapshot-jre/api/docs/com/google/common/collect/Lists.html#reverse(java.util.List)">Guava
	 *      <code>Lists.reverse(List&lt;T&gt;)</code></a>
	 */
	public static <E> Iterable<E> reversing(@Nonnull final List<E> list) {
		return () -> reverse(list.listIterator(list.size()));
	}

	//sublists

	/**
	 * Determines the longest common suffix sublist from a list of lists. No element element is allowed to be <code>null</code>.
	 * @param <T> The common type of element in each of the lists.
	 * @param lists The lists of lists among which to find a common suffix sublist.
	 * @return A list representing the common suffix sublist of elements, compared using {@link Object#equals(Object)}.
	 * @throws NullPointerException if one of the lists is <code>null</code> or contains a <code>null</code> value.
	 */
	public static <T> List<T> longestCommonSuffix(@Nonnull final List<List<T>> lists) {
		if(lists.isEmpty()) {
			return emptyList();
		}
		assert lists.size() > 0;
		final List<T> commonSuffix = new LinkedList<>();
		int index = 0;
		boolean finished = false;
		do {
			T common = null;
			for(final List<? extends T> list : lists) {
				final int length = list.size();
				if(index >= length) {
					finished = true;
					break;
				}
				final T element = requireNonNull(list.get(length - index - 1));
				if(common == null) { //first list
					common = element;
				} else { //other lists
					if(!element.equals(common)) {
						finished = true;
						break;
					}
				}
			}
			if(!finished) {
				assert common != null : "The outer list isn't empty, and if the inner list were empty we would be noted as finished.";
				commonSuffix.add(0, common);
				index++;
			}
		} while(!finished);
		return commonSuffix;
	}
}
