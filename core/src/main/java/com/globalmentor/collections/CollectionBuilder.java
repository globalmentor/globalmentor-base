/*
 * Copyright © 2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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
 * Utility methods for building collections using a fluent interface.
 * @author Garret Wilson
 */
public class CollectionBuilder {

	/**
	 * Appends the contents of an iterable to a collection. This method functions identical to {@link List#addAll(Collection)}, except that this method returns
	 * the given collection.
	 * @param <E> The type of element contained in the collection.
	 * @param <C> The type of collection.
	 * @param collection The collection to which objects will be added.
	 * @param iterable The source of the added objects.
	 * @return The given collection.
	 */
	public static <E, C extends Collection<E>> C addAll(final C collection, final Iterable<? extends E> iterable) {
		return addAll(collection, iterable.iterator());
	}

	/**
	 * Adds all of the elements specified by the given iterator to the specified collection and returns the given collection.
	 * @param <E> The type of element contained in the collection.
	 * @param <C> The type of collection.
	 * @param collection The collection to which objects will be added.
	 * @param iterator The source of the added objects.
	 * @return The given collection.
	 */
	public static <E, C extends Collection<E>> C addAll(final C collection, final Iterator<? extends E> iterator) {
		Collections.addAll(collection, iterator); //add all the elements from the iterator
		return collection; //return the collection
	}

	/**
	 * Adds all of the specified elements to the specified collection and returns the given collection.
	 * @param <E> The type of element contained in the collection.
	 * @param <C> The type of collection.
	 * @param collection The collection to which objects will be added.
	 * @param elements The source of the added objects.
	 * @return The given collection.
	 */
	public static <E, C extends Collection<E>> C addAll(final C collection, final E... elements) {
		java.util.Collections.addAll(collection, elements); //add all the elements
		return collection; //return the collection
	}

}
