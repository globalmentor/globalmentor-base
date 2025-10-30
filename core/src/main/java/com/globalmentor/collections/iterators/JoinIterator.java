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

import static java.util.Collections.*;
import static java.util.stream.Collectors.*;

import java.util.*;
import java.util.stream.Stream;

import org.jspecify.annotations.*;

/**
 * An iterator that joins the contents of zero or more iterators. This class also implements the {@link Enumeration} interface. Element removal is not
 * supported.
 * @param <E> The type of element being iterated.
 * @author Garret Wilson
 */
public class JoinIterator<E> extends AbstractIteratorDecorator<E> {

	/** The queue of iterators to join. */
	private final Queue<Iterator<E>> iteratorQueue;

	/**
	 * {@inheritDoc}
	 * <p>
	 * This implementation returns the first iterator in the queue that is not empty. If no further iterators remain in the queue, an empty iterator is returned.
	 * This ensures that empty iterators are discarded as soon as possible, as well as allowing the actual iteration implementation to be delegated to the base
	 * classes.
	 * </p>
	 */
	@Override
	protected Iterator<E> getIterator() {
		Iterator<E> iterator = null;
		//remove all empty iterators
		while(!iteratorQueue.isEmpty() && !(iterator = iteratorQueue.peek()).hasNext()) {
			iteratorQueue.remove();
		}
		assert iterator != null || iteratorQueue.isEmpty();
		//return the last accessed iterator, or an empty iterator if we ran out
		return iteratorQueue.isEmpty() ? emptyIterator() : iterator;
	}

	/**
	 * Iterators constructor.
	 * @param iterators The iterators to join.
	 * @throws NullPointerException if any of the given iterators is <code>null</code>.
	 */
	public JoinIterator(@NonNull final Stream<Iterator<E>> iterators) {
		iteratorQueue = iterators.collect(toCollection(LinkedList::new));
		iteratorQueue.forEach(Objects::requireNonNull);
	}

	/**
	 * Iterators varargs constructor.
	 * @param iterators The iterators to join.
	 * @throws NullPointerException if any of the given iterators is <code>null</code>.
	 */
	@SafeVarargs
	@SuppressWarnings("varargs")
	JoinIterator(@NonNull final Iterator<E>... iterators) {
		this(Stream.of(iterators));
	}

	@Override
	public void remove() {
		//we might have already thrown away the last iterator we used, if next() was called and it was empty
		throw new UnsupportedOperationException("A join iterator does not support removal.");
	}

}
