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
 * An object that holds another object.
 * @param <T> The type of object being held.
 * @author Garret Wilson
 */
public class ObjectHolder<T> extends AbstractObjectDecorator<T> {

	/** @return The held object. */
	public T getObject() {
		return super.getObject();
	}

	/**
	 * Sets the held object.
	 * @param object The object to hold.
	 */
	public void setObject(final T object) {
		super.setObject(object);
	}

	/** Default constructor to hold <code>null</code>. */
	public ObjectHolder() {
		this(null); //hold null
	}

	/**
	 * Held object constructor.
	 * @param object The object to hold.
	 */
	public ObjectHolder(final T object) {
		super(object); //construct the parent class
	}

	/** @return <code>true</code> if the object being held is not equal to <code>null</code>. */
	public boolean isPresent() {
		return getObject() != null;
	}

}
