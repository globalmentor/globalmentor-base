/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.model;

import com.globalmentor.java.Objects;

/**
 * An object that decorates another object, preserving the {@link Object#hashCode()} and {@link Object#equals(Object)} of the decorated object. Equality is only
 * supported for exact top-level types.
 * @param <T> The type of object being decorated.
 * @author Garret Wilson
 */
public abstract class AbstractObjectDecorator<T> {

	/** The decorated object. */
	private T object;

	/** @return The decorated object. */
	protected T getObject() {
		return object;
	}

	/**
	 * Sets the decorated object.
	 * @param decoratedObject The object to decorate.
	 */
	protected void setObject(final T decoratedObject) {
		this.object = decoratedObject;
	}

	/**
	 * Decorated object constructor.
	 * @param decoratedObject The object to decorate.
	 */
	public AbstractObjectDecorator(final T decoratedObject) {
		this.object = decoratedObject; //save the decorated object
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This version returns the hash code of the decorated object if there is one; otherwise this method delegates to the parent class.
	 */
	@Override
	public int hashCode() {
		final T decoratedObject = getObject(); //get the decorated object
		return decoratedObject != null ? getObject().hashCode() : super.hashCode(); //return the hash code of the decorated object if possible
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This version considers the given object equal to this object if it is of the same type as this object, and this object's decorated object's
	 *           {@link Object#equals(Object)} method also returns <code>true</code> for the objects's decorated object or both decorated objects are
	 *           <code>null</code>.
	 */
	@Override
	public boolean equals(final Object object) {
		return getClass().isInstance(object) && Objects.equals(getObject(), ((ObjectDecorator<?>)object).getObject()); //see if the object is of this class and our decorated object is equal to its decorated object
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This version returns a string version of the decorated object if there is one; otherwise this method delegates to the parent class.
	 */
	@Override
	public String toString() {
		final T decoratedObject = getObject(); //get the decorated object
		return decoratedObject != null ? getObject().toString() : super.toString(); //delegate to the decorated object, if possible
	}

}
