/*
 * Copyright © 2011-2013 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.collections.iterables;

import java.util.Iterator;

import static java.util.Objects.*;

import com.globalmentor.collections.iterators.ConverterIterator;
import com.globalmentor.model.Converter;

/**
 * An iterable that returns instances of a {@link ConverterIterator}..
 * @param <I> The input type.
 * @param <O> The output type.
 * @author Garret Wilson
 * @see Converter
 */
public class ConverterIterable<I, O> implements Iterable<O> {

	/** The iterable this class decorates. */
	private final Iterable<I> iterable;

	/** @return The iterable this class decorates. */
	protected Iterable<I> getIterable() {
		return iterable;
	}

	/** The converter for converting the elements. */
	private final Converter<I, O> converter;

	/** @return The converter for converting the elements. */
	public Converter<I, O> getConverter() {
		return converter;
	}

	/**
	 * Iterable and converter constructor.
	 * @param iterable The iterable of source objects.
	 * @param converter The conversor to be used on the iterable.
	 * @throws NullPointerException of if the given iterable and/or converter is <code>null</code>.
	 */
	public ConverterIterable(final Iterable<I> iterable, final Converter<I, O> converter) {
		this.iterable = requireNonNull(iterable);
		this.converter = requireNonNull(converter);
	}

	@Override
	public Iterator<O> iterator() {
		return new ConverterIterator<I, O>(getIterable().iterator(), converter);
	}

}
