/*
 * Copyright © 2011 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import java.util.ListIterator;

import com.globalmentor.model.Converter;

/**
 * A list iterator that returns its objects converted using a {@link Converter}.
 * <p>
 * This implementation does not support adding or setting elements.
 * @author Garret Wilson
 * @param <I> The input type.
 * @param <O> The output type.
 * @see Converter
 */
public class ConverterListIterator<I, O> extends ConverterIterator<I, O> implements ListIterator<O> {

	/**
	 * {@inheritDoc} This version returns the iterator as a {@link ListIterator}.
	 */
	@Override
	protected ListIterator<I> getIterator() {
		return (ListIterator<I>)super.getIterator();
	}

	/**
	 * Iterator and converter constructor.
	 * @param iterator The iterator of source objects.
	 * @param converter The conversor to be used on the iterable.
	 * @throws NullPointerException of if the given iterator and/or converter is <code>null</code>.
	 */
	public ConverterListIterator(final ListIterator<I> iterator, final Converter<I, O> converter) {
		super(iterator, converter);
	}

	/** {@inheritDoc} */
	public boolean hasNext() {
		return getIterator().hasNext();
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This implementation converts the next object before returning it.
	 * </p>
	 * @see Converter#convert(Object)
	 */
	public O next() {
		return getConverter().convert(getIterator().next());
	}

	/** {@inheritDoc} */
	public boolean hasPrevious() {
		return getIterator().hasPrevious();
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This implementation converts the previous object before returning it.
	 * </p>
	 * @see Converter#convert(Object)
	 */
	public O previous() {
		return getConverter().convert(getIterator().previous());
	}

	/** {@inheritDoc} */
	public int nextIndex() {
		return getIterator().nextIndex();
	}

	/** {@inheritDoc} */
	public int previousIndex() {
		return getIterator().previousIndex();
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This implementation throws an {@link UnsupportedOperationException}.
	 * </p>
	 */
	public void set(final O e) {
		throw new UnsupportedOperationException("Converter list iterators do not support setting elements.");
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This implementation throws an {@link UnsupportedOperationException}.
	 * </p>
	 */
	public void add(final O e) {
		throw new UnsupportedOperationException("Converter list iterators do not support adding elements.");
	}

}
