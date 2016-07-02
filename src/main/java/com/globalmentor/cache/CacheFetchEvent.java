/*
 * Copyright © 1996-2009 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.cache;

import java.util.EventObject;

import static java.util.Objects.*;

/**
 * An event that is fired when a value is fetched in the cache.
 * @param <Q> The type of query used to request data from the cache.
 * @param <V> The type of value stored in the cache.
 * @see Cache
 * @see CacheFetchListener
 * @author Garret Wilson
 */
public class CacheFetchEvent<Q, V> extends EventObject {

	/**
	 * The cache which fetched the value.
	 * @return The object on which the Event initially occurred.
	 */
	@SuppressWarnings("unchecked")
	public Cache<Q, V> getSource() {
		return (Cache<Q, V>)super.getSource();
	}

	/** The query used to request data from the cache. */
	private final Q query;

	/** @return The query used to request data from the cache. */
	public Q getQuery() {
		return query;
	}

	/** The fetched value. */
	private final V value;

	/** @return The fetched value. */
	public V getValue() {
		return value;
	}

	/**
	 * Source and property name constructor with old and new values. The target will be set to be the same as the given source.
	 * @param source The bean that fired the event.
	 * @param query The query used to request data from the cache.
	 * @param value The fetched value.
	 * @throws NullPointerException if the given source, query, and/or value is <code>null</code>.
	 */
	public CacheFetchEvent(final Cache<Q, V> source, final Q query, final V value) {
		super(requireNonNull(source, "Event source object cannot be null.")); //construct the parent class
		this.query = requireNonNull(query, "Query cannot be null.");
		this.value = requireNonNull(value, "Value cannot be null.");
	}

}
