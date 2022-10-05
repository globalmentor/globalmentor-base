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
 * A convenience class for storing an ID, a name, and a description. Is useful to serve as a base class to other classes that are identified by an ID and have a
 * name and a description.
 * @author Garret Wilson
 */
public class DefaultIDNameDescriptioned<I, N extends Comparable<N>, D> extends DefaultIDNamed<I, N> {

	/** The description of the object, or <code>null</code> if the object has no description. */
	private D description;

	/** @return The description of the object, or <code>null</code> if the object has no description. */
	public D getDescription() {
		return description;
	}

	/**
	 * Sets the description of the object.
	 * @param description The new description of the object, or <code>null</code> if the object should have no description.
	 */
	protected void setDescription(final D description) {
		this.description = description;
	}

	/**
	 * Constructor specifying the ID, name, and description.
	 * @param id The ID of the object, or <code>null</code> if the object should have no ID.
	 * @param name The name of the object, or <code>null</code> if the object should have no name.
	 * @param description The description of the object, or <code>null</code> if the object should have no description.
	 */
	public DefaultIDNameDescriptioned(final I id, final N name, final D description) {
		super(id, name); //construct the base class
		this.description = description;
	}

	/** @return A string representation of this object in the format "objectinfo: [ID] name (description)". */
	public String toString() {
		return super.toString() + " (" + getDescription() + ")"; //return a string constructed from the default string and the description
	}

}
