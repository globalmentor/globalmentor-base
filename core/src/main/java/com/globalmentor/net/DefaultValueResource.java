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

package com.globalmentor.net;

import java.net.URI;

/**
 * An abstract implementation of a resource that represents some value object instance.
 * @param <V> The type of value represented by the resource.
 * @author Garret Wilson
 */
public class DefaultValueResource<V> extends AbstractValueResource<V> {

	/** The resource identifier URI, or <code>null</code> if the identifier is not known. */
	private final URI uri;

	/** @return The resource identifier URI, or <code>null</code> if the identifier is not known. */
	public URI getURI() {
		return uri;
	}

	/**
	 * Constructor.
	 * @param uri The resource identifier URI, or <code>null</code> if the identifier is not known.
	 * @param valueClass The class representing the type of value represented by the resource.
	 * @param value The non-<code>null</code> value represented by the resource.
	 * @throws NullPointerException if the given value class and/or value is <code>null</code>.
	 */
	public DefaultValueResource(final URI uri, final Class<V> valueClass, final V value) {
		super(valueClass, value);
		this.uri = uri;
	}
}