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

/**
 * A convenience class for storing an ID and a name. This class is useful to serve as a base class to other classes that are identified by an ID and a name.
 * @param <I> The type of ID.
 * @param <N> The type of name.
 * @author Garret Wilson
 */
public class DefaultIDNamed<I, N> extends DefaultIDed<I> implements Named<N> {

	/** The name of the object, or <code>null</code> if the object has no name. */
	private N name;

	/** @return The name of the object. */
	public N getName() {
		return name;
	}

	/**
	 * Sets the name of the object.
	 * @param name The new name of the object, or <code>null</code> if the object should have no name.
	 */
	protected void setName(final N name) {
		this.name = name;
	}

	/**
	 * Constructor specifying the ID and name.
	 * @param id The ID of the object, or <code>null</code> if hte object shoudl have no name.
	 * @param name The name of the object.
	 */
	public DefaultIDNamed(final I id, final N name) {
		super(id); //construct the base class
		this.name = name;
	}

	/** @return A string representation of this object in the format "objectinfo: [ID] name". */
	public String toString() {
		return super.toString() + ' ' + getName(); //return a string constructed from the default string and the name
	}

}
