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

package com.globalmentor.collections.iterators;

import static com.globalmentor.java.Objects.*;

import java.util.Iterator;

import com.globalmentor.model.Converter;

/**
 * An iterator that returns its objects converted using a {@link Converter}.
 * @author Garret Wilson
 * @param <I> The input type.
 * @param <O> The output type.
 * @see Converter
 */
public class ConverterIterator<I, O> implements Iterator<O>
{

	/** The iterator this class decorates. */
	private final Iterator<I> iterator;

	/** @return The iterator this class decorates. */
	protected Iterator<I> getIterator()
	{
		return iterator;
	}

	/** The converter for converting the elements. */
	private final Converter<I, O> converter;

	/** @return The converter for converting the elements. */
	public Converter<I, O> getConverter()
	{
		return converter;
	}

	/**
	 * Iterator and converter constructor.
	 * @param iterator The iterator of source objects.
	 * @throws NullPointerException of if the given iterator and/or converter is <code>null</code>.
	 */
	public ConverterIterator(final Iterator<I> iterator, final Converter<I, O> converter)
	{
		this.iterator = checkInstance(iterator);
		this.converter = checkInstance(converter);
	}

	/** {@inheritDoc} */
	public boolean hasNext()
	{
		return getIterator().hasNext();
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This implementation converts the next object before returning it.
	 * </p>
	 * @see Converter#convert(Object)
	 */
	public O next()
	{
		return getConverter().convert(getIterator().next());
	}

	/** {@inheritDoc} */
	public void remove()
	{
		getIterator().remove();
	}

}