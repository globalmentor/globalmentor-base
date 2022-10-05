/*
 * Copyright © 1996-2016 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.collections.iterators;

import java.util.*;
import java.util.function.Consumer;

/**
 * An abstract base implementation of an iterator that wraps an existing {@link Iterator}.
 * @apiNote This iterator also serves as an adapter, converting an iterator to an {@link Enumeration}.
 * @implNote Subclasses may override {@link #hasNext()} and/or {@link #next()}, and {@link Enumeration} compatibility will be maintained.
 * @implNote This implementation specifically does <em>not</em> override {@link #forEachRemaining(Consumer)} to delegate to the underlying iterator's version of
 *           the method, because subclasses are allowed to change the decorated iterator during iteration. Therefore the default method version is retained,
 *           which calls {@link #hasNext()} and {@link #next()} repeatedly, allowing a subclass to acquire a different underlying iterator if it desires.
 * 
 * @param <E> the type of elements returned by this iterator.
 * @author Garret Wilson
 */
public abstract class AbstractIteratorDecorator<E> implements Iterator<E>, Enumeration<E> {

	/** @return The iterator this class decorates. */
	protected abstract Iterator<E> getIterator();

	//Iterator<E>

	@Override
	public boolean hasNext() {
		return getIterator().hasNext();
	}

	@Override
	public E next() {
		return getIterator().next();
	}

	@Override
	public void remove() {
		getIterator().remove();
	}

	//Enumeration<E>

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation delegates to {@link #hasNext()}.
	 */
	@Override
	public final boolean hasMoreElements() {
		return hasNext();
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation delegates to {@link #next()}.
	 */
	@Override
	public final E nextElement() {
		return next();
	}

}
