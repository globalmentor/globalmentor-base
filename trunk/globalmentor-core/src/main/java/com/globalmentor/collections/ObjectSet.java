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

import java.lang.reflect.Array;
import java.util.*;

import com.globalmentor.collections.iterators.ObjectIterator;

import static com.globalmentor.java.Objects.*;

/**
 * A read-only set containing a single object.
 * 
 * @author Garret Wilson
 * 
 * @param <E> The type of object contained in the set.
 */
public class ObjectSet<E> implements Set<E>, ImmutableCollection<E>
{
	/** The object held in the set. */
	private E object;

	/**
	 * Object constructor.
	 * @param object The object to hold in the set.
	 * @throws NullPointerException if the given object is <code>null</code>.
	 */
	public ObjectSet(final E object)
	{
		this.object = checkInstance(object, "Object cannot be null.");
	}

	/** {@inheritDoc} */
	public int size()
	{
		return 1;
	}

	/** {@inheritDoc} */
	public boolean isEmpty()
	{
		return false;
	}

	/** {@inheritDoc} */
	public boolean contains(Object o)
	{
		return object.equals(o);
	}

	/** {@inheritDoc} */
	public Iterator<E> iterator()
	{
		return new ObjectIterator<E>(object);
	}

	/** {@inheritDoc} */
	public Object[] toArray()
	{
		return new Object[] { object };
	}

	/** {@inheritDoc} */
	@SuppressWarnings("unchecked")
	public <T> T[] toArray(T[] a)
	{
		if(a.length < 1)
		{
			a = (T[]) Array.newInstance(a.getClass().getComponentType(), 1);
		}
		a[0] = (T) object;
		if(a.length > 1)
		{
			a[1] = null;
		}
		return a;
	}

	/** {@inheritDoc} */
	public boolean add(E e)
	{
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	public boolean remove(Object o)
	{
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	public boolean containsAll(Collection<?> c)
	{
		final int size = c.size();
		return size == 0 || (size == 1 && object.equals(c.iterator().next()));
	}

	/** {@inheritDoc} */
	public boolean addAll(Collection<? extends E> c)
	{
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	public boolean retainAll(Collection<?> c)
	{
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	public boolean removeAll(Collection<?> c)
	{
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	public void clear()
	{
		throw new UnsupportedOperationException();
	}

	/** {@inheritDoc} */
	public boolean equals(Object o)
	{
		if(!(o instanceof Set))
		{
			return false;
		}
		final Set<?> set = (Set<?>) o;
		return set.size() == 1 && object.equals(set.iterator().next());
	}

	/** {@inheritDoc} */
	public int hashCode()
	{
		return object.hashCode();
	}

}
