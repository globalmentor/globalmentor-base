/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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
import java.util.function.Predicate;
import java.util.stream.Stream;

import static java.util.Objects.*;

/**
 * A collection that wraps an existing collection, providing access through the {@link Collection} interface.
 * @param <E> The type of element contained in the collection.
 * @author Garret Wilson
 */
public class CollectionDecorator<E> implements Collection<E> {

	/** The collection this class decorates. */
	protected final Collection<E> collection;

	/**
	 * Collection constructor.
	 * @param collection The collection this collection should decorate.
	 * @throws NullPointerException if the provided collection is <code>null</code>.
	 */
	public CollectionDecorator(final Collection<E> collection) {
		this.collection = requireNonNull(collection, "Collection cannot be null"); //save the collection
	}

	@Override
	public int size() {
		return collection.size();
	}

	@Override
	public boolean isEmpty() {
		return collection.isEmpty();
	}

	@Override
	public boolean contains(Object o) {
		return collection.contains(o);
	}

	@Override
	public Iterator<E> iterator() {
		return collection.iterator();
	}

	@Override
	public Object[] toArray() {
		return collection.toArray();
	}

	@Override
	public <T> T[] toArray(T a[]) {
		return collection.toArray(a);
	}

	// Modification Operations

	@Override
	public boolean add(E o) {
		return collection.add(o);
	}

	@Override
	public boolean remove(Object o) {
		return collection.remove(o);
	}

	// Bulk Operations

	@Override
	public boolean containsAll(Collection<?> c) {
		return collection.containsAll(c);
	}

	@Override
	public boolean addAll(Collection<? extends E> c) {
		return collection.addAll(c);
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		return collection.removeAll(c);
	}

	@Override
	public boolean removeIf(Predicate<? super E> filter) {
		return collection.removeIf(filter);
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		return collection.retainAll(c);
	}

	@Override
	public void clear() {
		collection.clear();
	}

	// Comparison and hashing

	@Override
	public boolean equals(Object o) {
		return collection.equals(o);
	}

	@Override
	public int hashCode() {
		return collection.hashCode();
	}

	@Override
	public Spliterator<E> spliterator() {
		return collection.spliterator();
	}

	@Override
	public Stream<E> stream() {
		return collection.stream();
	}

	@Override
	public Stream<E> parallelStream() {
		return collection.parallelStream();
	}

}
