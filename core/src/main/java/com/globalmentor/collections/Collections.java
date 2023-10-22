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

import java.util.*;

/**
 * Various utilities to be used with collections.
 * @author Garret Wilson
 * @see java.util.Collection
 */
public class Collections {

	/**
	 * Adds all of the elements specified by the given iterable to the specified collection.
	 * @param <CT> The type of the elements contained on the collection.
	 * @param <IT> The type of the elements contained on the iterable.
	 * @param collection The collection to which objects will be added.
	 * @param iterable The source of the added objects.
	 * @return <code>true</code> if the collection changed as a result of the call.
	 */
	public static <CT, IT extends CT> boolean addAll(final Collection<CT> collection, final Iterable<IT> iterable) {
		if(iterable instanceof Collection<IT> otherCollection) {
			return collection.addAll(otherCollection);
		}
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
	 * @param <CT> The type of the elements contained on the collection.
	 * @param <IT> The type of the elements contained on the iterable.
	 * @param collection The collection from which objects will be removed.
	 * @param iterable The source of the removed objects.
	 * @return <code>true</code> if the collection changed as a result of the call.
	 */
	public static <CT, IT extends CT> boolean removeAll(final Collection<CT> collection, final Iterable<IT> iterable) {
		if(iterable instanceof Collection<IT> otherCollection) {
			return collection.removeAll(otherCollection);
		}
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

}
