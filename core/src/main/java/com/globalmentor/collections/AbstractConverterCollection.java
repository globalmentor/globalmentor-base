/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import com.globalmentor.collections.iterators.IteratorDecorator;

/**
 * A collection that provides access to another collection, automatically converting elements to objects possibly of a different type. The conversion is done on
 * the fly as elements are requested, and not before.
 * @param <S> The type of element contained in the source collection.
 * @param <D> The type of element contained in the destination collection.
 * @author Garret Wilson
 */
public abstract class AbstractConverterCollection<S, D> extends CollectionDecorator<D> { //TODO add conversion access for all necessary methods

	/**
	 * Collection constructor.
	 * @param collection The collection the elements of which this collection should convert.
	 */
	@SuppressWarnings("unchecked")
	public AbstractConverterCollection(final Collection<S> collection) {
		super((Collection<D>)collection); //construct the parent class with the source collection, even though we know the list may be of a different type; we'll do the correct conversions for the access methods
	}

	/**
	 * Converts an object in the collection to another object.
	 * @param source The object to convert.
	 * @return The converted object.
	 */
	protected abstract D convert(final S source);

	/**
	 * Returns a custom proxied iterator that will convert returned elements on the fly.
	 * @return A custom proxied iterator that will convert returned elements on the fly.
	 */
	@SuppressWarnings("unchecked")
	public Iterator<D> iterator() {
		return new ConverterIterator((Iterator<S>)super.iterator()); //create an iterator that will convert the elements
	}

	/**
	 * A custom proxied iterator that converts returned elements on the fly.
	 * @author Garret Wilson
	 */
	protected class ConverterIterator extends IteratorDecorator<D> {

		/**
		 * Iterator constructor.
		 * @param iterator The iterator of source objects.
		 */
		@SuppressWarnings("unchecked")
		public ConverterIterator(final Iterator<S> iterator) {
			super((Iterator<D>)iterator); //construct the parent class source iterator, even though we know the list may be of a different type; we'll do the correct conversions for the access methods
		}

		/**
		 * Converts and returns the next element in the iteration.
		 * @return An object representing the converted next element in the iteration.
		 * @throws NoSuchElementException Thrown if the iteration has no more elements.
		 */
		@SuppressWarnings("unchecked")
		public D next() {
			return convert((S)super.next()); //get the next entry, convert it, and return it 			
		}

		//TODO fix conversion versions of the other iterator methods

	}

}
