/*
 * Copyright © 2011-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.io.IOException;
import java.util.*;

import static java.util.Objects.*;

import com.globalmentor.collections.iterators.*;
import com.globalmentor.java.Arrays;

import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.text.TextFormatter.*;

/**
 * A read-only set containing a single object.
 * 
 * @author Garret Wilson
 * 
 * @param <E> The type of object contained in the set.
 */
public class ObjectSet<E> implements Set<E>, ImmutableCollection<E> {

	/** The object held in the set. */
	private E object;

	/**
	 * Object constructor.
	 * @param object The object to hold in the set.
	 * @throws NullPointerException if the given object is <code>null</code>.
	 */
	public ObjectSet(final E object) {
		this.object = requireNonNull(object, "Object cannot be null.");
	}

	/** {@inheritDoc} */
	@Override
	public int size() {
		return 1;
	}

	/** {@inheritDoc} */
	@Override
	public boolean isEmpty() {
		return false;
	}

	/** {@inheritDoc} */
	@Override
	public boolean contains(Object o) {
		return object.equals(o);
	}

	/** {@inheritDoc} */
	@Override
	public Iterator<E> iterator() {
		return new ObjectIterator<E>(object);
	}

	/** {@inheritDoc} */
	@Override
	public Object[] toArray() {
		return new Object[] { object };
	}

	/** {@inheritDoc} */
	@Override
	@SuppressWarnings("unchecked")
	public <T> T[] toArray(T[] a) {
		a = Arrays.getArray(a, 1); //make sure our array is large enough
		a[0] = (T)object;
		if(a.length > 1) {
			a[1] = null;
		}
		return a;
	}

	/** {@inheritDoc} */
	@Override
	public boolean add(E e) {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	@Override
	public boolean remove(Object o) {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	@Override
	public boolean containsAll(Collection<?> c) {
		final int size = c.size();
		return size == 0 || (size == 1 && object.equals(c.iterator().next()));
	}

	/** {@inheritDoc} */
	@Override
	public boolean addAll(Collection<? extends E> c) {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	@Override
	public boolean retainAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	@Override
	public boolean removeAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	@Override
	public void clear() {
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	@Override
	public boolean equals(Object o) {
		if(!(o instanceof Set)) {
			return false;
		}
		final Set<?> set = (Set<?>)o;
		return set.size() == 1 && object.equals(set.iterator().next());
	}

	/** {@inheritDoc} */
	@Override
	public int hashCode() {
		return object.hashCode();
	}

	@Override
	public String toString() {
		try {
			final StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.append('[');
			formatList(stringBuilder, this);
			stringBuilder.append(']');
			return stringBuilder.toString();
		} catch(final IOException ioException) {
			throw unexpected(ioException);
		}
	}
}
