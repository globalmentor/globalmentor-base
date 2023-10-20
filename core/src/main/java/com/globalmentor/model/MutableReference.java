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

import javax.annotation.Nullable;

/**
 * A lightweight reference to another object that can be updated.
 * @implSpec This reference is <em>not</em> thread safe! For thread safety, consider either an {@link java.util.concurrent.atomic.AtomicReference} or a
 *           {@link ReadWriteLockReference} instead.
 * @param <T> The type of object being referenced.
 * @author Garret Wilson
 */
public final class MutableReference<T> {

	private T object;

	/** @return The referenced object. */
	@Nullable
	public T get() {
		return object;
	}

	/**
	 * Sets the referenced object.
	 * @param object The object to reference.
	 */
	public void set(@Nullable final T object) {
		this.object = object;
	}

	/** Default constructor to reference <code>null</code>. */
	public MutableReference() {
		this(null);
	}

	/**
	 * Referenced object constructor.
	 * @param object The object to reference, which may be <code>null</code>
	 */
	public MutableReference(@Nullable final T object) {
		this.object = object;
	}

	/** @return <code>true</code> if the object being referenced is not equal to <code>null</code>. */
	public boolean isPresent() {
		return get() != null;
	}

	@Override
	public int hashCode() {
		final Object object = get();
		return object != null ? object.hashCode() : 0;
	}

	@Override
	public boolean equals(final Object object) {
		if(this == object) {
			return true;
		}
		if(object instanceof MutableReference<?> mutableReference) {
			return Objects.equals(get(), mutableReference.get());
		}
		return false;
	}

	@Override
	public String toString() {
		return String.valueOf(get());
	}

}
