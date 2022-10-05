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
 * A default implementation of a named object comparable by name. This implementation does not allow a <code>null</code> name.
 * @author Garret Wilson
 */
public class DefaultComparableNamed<N extends Comparable<N>> extends DefaultNamed<N> implements Comparable<Named<N>> {

	/**
	 * Constructor specifying the name.
	 * @param newName The object's new name.
	 * @throws NullPointerException if the given name is <code>null</code>;
	 */
	public DefaultComparableNamed(final N newName) {
		super(requireNonNull(newName, "Name cannot be null.")); //construct the parent class
	}

	/**
	 * Compares this object to another object. This method determines order based upon the name.
	 * @param namedObject The object with which to compare this object.
	 * @return A negative integer, zero, or a positive integer as this name is less than, equal to, or greater than the name of the specified object,
	 *         respectively.
	 * @see #getName()
	 */
	public int compareTo(final Named<N> namedObject) throws ClassCastException {
		return getName().compareTo(namedObject.getName()); //compare names
	}

}