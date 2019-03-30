/*
 * Copyright © 2016 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.collections.iterators;

import java.util.*;

/**
 * An abstract base implementation of an enumeration that wraps an existing {@link Enumeration}.
 * <p>
 * This enumeration also serves as an adapter, converting an enumeration to an {@link Iterator}. This implementation does not support element removal.
 * </p>
 * <p>
 * Subclasses may override {@link #hasMoreElements()} and/or {@link #nextElement()}, and {@link Iterator} compatibility will be maintained.
 * </p>
 * @param <E> the type of elements returned by this enumeration.
 * @author Garret Wilson
 */
public abstract class AbstractEnumerationDecorator<E> implements Enumeration<E>, Iterator<E> {

	/** @return The enumeration this class decorates. */
	protected abstract Enumeration<E> getEnumeration();

	//Enumeration<E>

	@Override
	public boolean hasMoreElements() {
		return getEnumeration().hasMoreElements();
	}

	@Override
	public E nextElement() {
		return getEnumeration().nextElement();
	}

	//Iterator<E>

	/**
	 * {@inheritDoc}
	 * <p>
	 * This implementation delegates to {@link #hasMoreElements()}.
	 * </p>
	 */
	@Override
	public final boolean hasNext() {
		return hasMoreElements();
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This implementation delegates to {@link #nextElement()}.
	 * </p>
	 */
	@Override
	public final E next() {
		return nextElement();
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This implementation thrown an {@link UnsupportedOperationException}.
	 * </p>
	 */
	@Override
	public void remove() {
		throw new UnsupportedOperationException("An enumeration decorator does not support removal.");
	}

}