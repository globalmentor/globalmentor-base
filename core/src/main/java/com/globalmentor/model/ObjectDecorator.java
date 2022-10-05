/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.model;

import static java.util.Objects.*;

/**
 * An object that decorates another object, preserving the {@link Object#hashCode()} and {@link Object#equals(Object)} of the decorated object. Equality is only
 * supported for exact top-level types. This class does not permit a <code>null</code> decorated object.
 * @param <T> The type of object being decorated.
 * @author Garret Wilson
 */
public class ObjectDecorator<T> extends AbstractObjectDecorator<T> {

	/**
	 * Decorated object constructor.
	 * @param decoratedObject The object to decorate.
	 * @throws NullPointerException if the given object is <code>null</code>.
	 */
	public ObjectDecorator(final T decoratedObject) {
		super(requireNonNull(decoratedObject, "Decorated object cannot be null.")); //construct the parent class, making sure the decorated object is not null
	}

}
