/*
 * Copyright © 2013 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static java.util.Objects.*;

import java.net.URI;

/**
 * The abstract base class for a Java object that is also a resource.
 * <p>
 * This implementation considers objects equal if they are of the the specified runtime type and have the same URI.
 * </p>
 * @param <C> The recursive <code>THIS</code> type.
 * @author Garret Wilson
 */
public abstract class AbstractObjectResource<C extends AbstractObjectResource<C>> extends DefaultResource implements Comparable<Resource> {

	/**
	 * The type of class with which other object resources must be instances of to be equal. This is a private variable to prevent interpretation as a property in
	 * subclasses.
	 */
	private final Class<? extends C> type;

	/**
	 * URI constructor.
	 * @param uri The URI for the new resource.
	 * @param type The type of the new resource.
	 */
	public AbstractObjectResource(final URI uri, final Class<? extends C> type) {
		super(uri);
		this.type = requireNonNull(type);
	}

	/**
	 * {@inheritDoc} This implementation compares the specified runtime type and resource URIs. If neither object has a reference URI, the default identity
	 * comparison is performed.
	 * @see #getURI()
	 */
	public boolean equals(final Object object) {
		if(!type.isInstance(object)) { //require the specified type
			return false;
		}
		return super.equals(object);
	}

}
