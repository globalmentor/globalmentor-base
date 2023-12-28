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

import java.util.Objects;

/**
 * A default implementation of a named object.
 * @param <N> The type of name.
 * @author Garret Wilson
 */
public class DefaultNamed<N> implements Named<N> {

	/** The name of the object, or <code>null</code> if the object has no name. */
	private final N name;

	/** @return The name of the object, or <code>null</code> if the object has no name. */
	public N getName() {
		return name;
	}

	/**
	 * Constructor specifying the name.
	 * @param name The object's new name, or <code>null</code> if the object has no name.
	 */
	public DefaultNamed(final N name) {
		this.name = name; //set the name
	}

	/**
	 * Compares the names of two objects if the other object is a {@link Named}.
	 * @param object The object with which to compare this named object; should be another {@link Named}.
	 * @return <code>true</code> if the other object is a named object with the same name.
	 * @see #getName()
	 */
	public boolean equals(final Object object) {
		return this == object || object instanceof Named && Objects.equals(getName(), ((Named<?>)object).getName()); //see if the other object is a named object with the same named
	}

	/** @return A hashcode value composed from the name. */
	public int hashCode() {
		final N name = getName(); //get the name, if any
		return name != null ? getName().hashCode() : super.hashCode(); //return the hash code of the name, if we can
	}

	/** @return A string representation of this object's name. */
	public String toString() {
		return super.toString() + ": " + getName(); //return a string constructed from the name
	}
}
