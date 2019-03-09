/*
 * Copyright © 2019 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.collections.iterables;

import java.util.*;
import java.util.stream.*;

import javax.annotation.*;

import com.globalmentor.collections.iterators.Iterators;

/**
 * Various utilities to be used with iterables.
 * @author Garret Wilson
 * @see Iterable
 */
public class Iterables {

	/**
	 * Returns an {@link Optional} describing the first element of this iterable, or an empty {@code Optional} if the iterable is empty.
	 * @implSpec This implementation efficiently short-circuits creating an iterator if the iterable is an instance of a {@link Collection}, which has a known
	 *           size.
	 * @param <T> the type of elements returned by the iterator.
	 * @param iterable The iterable from which the first object should be retrieved.
	 * @return An {@code Optional} describing the first element of this iterable, or an empty {@code Optional} if the iterable is empty.
	 * @see <a href="https://stackoverflow.com/q/13692700/421049">Good way to get *any* value from a Java Set?</a>
	 * @see Iterator#next()
	 * @see Stream#findFirst()
	 * @see Iterators#findNext(Iterator)
	 */
	public static <T> Optional<T> findFirst(@Nonnull final Iterable<T> iterable) {
		if(iterable instanceof Collection) { //short-circuit for empty collections
			final Collection<T> collection = (Collection<T>)iterable;
			if(collection.isEmpty()) {
				return Optional.empty();
			}
		}
		return Iterators.findNext(iterable.iterator());
	}

}
