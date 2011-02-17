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

package com.globalmentor.net;

import static com.globalmentor.java.Objects.*;

/**
 * An abstract implementation of a resource that represents some value object instance.
 * <p>
 * This implementation provides no representation of URI, on the basis that many implementations may wish to dynamically generate a URI based upon the value
 * being held.
 * </p>
 * @param <V> The type of value represented by the resource.
 * @author Garret Wilson
 */
public abstract class AbstractValueResource<V> implements ValueResource<V>
{

	/** The class representing the type of value represented by the resource. */
	private final Class<V> valueClass;

	/** @return The class representing the type of value represented by the resource. */
	public Class<V> getValueClass()
	{
		return valueClass;
	}

	/** The non-<code>null</code> value represented by the resource. */
	private final V value;

	/** @return The non-<code>null</code> value represented by the resource. */
	public V getValue()
	{
		return value;
	}

	/**
	 * Constructor.
	 * @param valueClass The class representing the type of value represented by the resource.
	 * @param value The non-<code>null</code> value represented by the resource.
	 * @throws NullPointerException if the given value class and/or value is <code>null</code>.
	 */
	public AbstractValueResource(final Class<V> valueClass, final V value)
	{
		this.valueClass = checkInstance(valueClass, "Value class cannot be null.");
		this.value = checkInstance(value, "Value cannot be null.");
	}
}