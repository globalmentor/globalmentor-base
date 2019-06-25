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

import javax.annotation.*;

import static java.util.Objects.*;

/**
 * An enumeration that wraps an existing enumeration.
 * @apiNote This enumeration also serves as an adapter, converting an enumeration to an {@link Iterator}.
 * @implNote This implementation does not support element removal. Subclasses may override {@link #hasMoreElements()} and/or {@link #nextElement()}, and
 *           {@link Iterator} compatibility will be maintained.
 * 
 * @param <E> the type of elements returned by this enumeration.
 * @author Garret Wilson
 */
public class EnumerationDecorator<E> extends AbstractEnumerationDecorator<E> {

	/** The enumeration this class decorates. */
	private final Enumeration<E> enumeration;

	@Override
	protected Enumeration<E> getEnumeration() {
		return enumeration;
	}

	/**
	 * Enumeration constructor.
	 * @param enumeration The enumeration this enumeration will decorate.
	 * @throws NullPointerException if the given enumeration is <code>null</code>.
	 */
	public EnumerationDecorator(@Nonnull final Enumeration<E> enumeration) {
		this.enumeration = requireNonNull(enumeration, "Enumeration cannot be null."); //save the enumeration
	}

}
