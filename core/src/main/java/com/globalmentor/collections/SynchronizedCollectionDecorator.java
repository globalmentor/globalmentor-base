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
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import static java.util.Objects.*;

/**
 * A collection that wraps an existing collection, providing access through the {@link Collection} interface. All collection access is synchronized on the
 * provided synchronization object.
 * @param <E> The type of element contained in the collection.
 * @author Garret Wilson
 */
public class SynchronizedCollectionDecorator<E> implements Collection<E> {

	/** The collection this class decorates. */
	protected final Collection<E> collection;

	/** The mutual exclusion synchronization object. */
	protected final Object mutex;

	/**
	 * Collection constructor. The new instance of this class is used as a mutex.
	 * @param collection The collection this collection should decorate.
	 * @throws NullPointerException if the provided collection is <code>null</code>.
	 */
	public SynchronizedCollectionDecorator(final Collection<E> collection) {
		this.collection = requireNonNull(collection, "Collection cannot be null"); //save the collection
		this.mutex = this; //use this instance as a mutex		
	}

	/**
	 * Collection and mutex constructor.
	 * @param collection The collection this collection should decorate.
	 * @param mutex The mutual exclusion synchronization object.
	 * @throws NullPointerException if the provided collection and/or mutex is <code>null</code>.
	 */
	public SynchronizedCollectionDecorator(final Collection<E> collection, final Object mutex) {
		this.collection = requireNonNull(collection, "Collection cannot be null"); //save the collection
		this.mutex = requireNonNull(mutex, "Mutex cannot be null"); //save the mutex
	}

	@Override
	public int size() {
		synchronized(mutex) {
			return collection.size();
		}
	}

	@Override
	public boolean isEmpty() {
		synchronized(mutex) {
			return collection.isEmpty();
		}
	}

	@Override
	public boolean contains(Object o) {
		synchronized(mutex) {
			return collection.contains(o);
		}
	}

	@Override
	public Iterator<E> iterator() {
		synchronized(mutex) {
			return collection.iterator();
		}
	}

	@Override
	public Object[] toArray() {
		synchronized(mutex) {
			return collection.toArray();
		}
	}

	@Override
	public <T> T[] toArray(T a[]) {
		synchronized(mutex) {
			return collection.toArray(a);
		}
	}

	// Modification Operations

	@Override
	public boolean add(E o) {
		synchronized(mutex) {
			return collection.add(o);
		}
	}

	@Override
	public boolean remove(Object o) {
		synchronized(mutex) {
			return collection.remove(o);
		}
	}

	// Bulk Operations

	@Override
	public boolean containsAll(Collection<?> c) {
		synchronized(mutex) {
			return collection.containsAll(c);
		}
	}

	@Override
	public boolean addAll(Collection<? extends E> c) {
		synchronized(mutex) {
			return collection.addAll(c);
		}
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		synchronized(mutex) {
			return collection.removeAll(c);
		}
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		synchronized(mutex) {
			return collection.retainAll(c);
		}
	}

	@Override
	public void clear() {
		synchronized(mutex) {
			collection.clear();
		}
	}

	// Comparison and hashing

	@Override
	public boolean equals(Object o) {
		synchronized(mutex) {
			return collection.equals(o);
		}
	}

	@Override
	public int hashCode() {
		synchronized(mutex) {
			return collection.hashCode();
		}
	}

	@Override
	public Spliterator<E> spliterator() {
		synchronized(mutex) {
			return Spliterators.spliterator(this, 0);
		}
	}

	@Override
	public Stream<E> stream() {
		synchronized(mutex) {
			return StreamSupport.stream(spliterator(), false);
		}
	}

	@Override
	public Stream<E> parallelStream() {
		synchronized(mutex) {
			return StreamSupport.stream(spliterator(), true);
		}
	}

}
