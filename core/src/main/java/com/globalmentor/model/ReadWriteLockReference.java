/*
 * Copyright © 2011 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import java.util.concurrent.locks.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import static java.util.Objects.*;

import java.util.Objects;

/**
 * A reference to another object that provides read/write lock access to the object. The reference instance itself also functions as read/write lock access.
 * Equality is determined by another {@link ReadWriteLockReference} with an object that is {@link Object#equals(Object)}.
 * @apiNote This is a convenience class that is the equivalent of a {@link MutableReference} governed by a {@link ReadWriteLock}.
 * @param <T> The type of object being referenced.
 * @author Garret Wilson
 */
public final class ReadWriteLockReference<T> implements ReadWriteLock {

	/** The decorated read write lock. */
	private final ReadWriteLock readWriteLock;

	/** {@inheritDoc} */
	@Override
	public Lock readLock() {
		return readWriteLock.readLock();
	}

	/** {@inheritDoc} */
	@Override
	public Lock writeLock() {
		return readWriteLock.writeLock();
	}

	/** Default constructor to reference <code>null</code>. */
	public ReadWriteLockReference() {
		this(new ReentrantReadWriteLock());
	}

	/**
	 * Read write lock constructor.
	 * @param readWriteLock The lock for controlling access to the properties.
	 * @throws NullPointerException if the given lock is <code>null</code>.
	 */
	public ReadWriteLockReference(@Nonnull final ReadWriteLock readWriteLock) {
		this(null, readWriteLock);
	}

	/**
	 * Referenced object constructor.
	 * @param object The object to reference, which may be <code>null</code>.
	 */
	public ReadWriteLockReference(@Nullable final T object) {
		this(object, new ReentrantReadWriteLock());
	}

	private T object;

	/**
	 * Referenced object and read write lock constructor.
	 * @param object The object to reference.
	 * @param readWriteLock The lock for controlling access to the properties.
	 * @throws NullPointerException if the given lock is <code>null</code>.
	 */
	public ReadWriteLockReference(@Nullable final T object, @Nonnull final ReadWriteLock readWriteLock) {
		this.object = object;
		this.readWriteLock = requireNonNull(readWriteLock, "Read write lock cannot be null.");
	}

	/**
	 * Returns whether the referenced object is non-<code>null</code>.
	 * @apiNote This is a convenience method that is equivalent to checking the value of {@link #get()}.
	 * @implSpec The object is retrieved under a read lock, but the comparison is not performed under a read lock.
	 * @return <code>true</code> if the object being referenced is not equal to <code>null</code>.
	 */
	public boolean isPresent() {
		return get() != null;
	}

	/**
	 * Returns the referenced object under a read lock.
	 * @return The referenced object.
	 * @see #readLock()
	 * @see java.util.concurrent.atomic.AtomicReference#get()
	 */
	@Nullable
	public T get() {
		readLock().lock();
		try {
			return object;
		} finally {
			readLock().unlock();
		}
	}

	/**
	 * Sets the referenced object under a write lock.
	 * @param object The new object to reference.
	 * @see #writeLock()
	 * @see java.util.concurrent.atomic.AtomicReference#set(Object)
	 */
	public void set(@Nullable final T object) {
		writeLock().lock();
		try {
			this.object = object;
		} finally {
			writeLock().unlock();
		}
	}

	/**
	 * {@inheritDoc}
	 * @implSpec The object is retrieved under a read lock, but the hash code is not calculated under a read lock.
	 * @see #readLock()
	 */
	@Override
	public int hashCode() {
		final Object object = get();
		return object != null ? object.hashCode() : 0;
	}

	/**
	 * {@inheritDoc}
	 * @implSpec The object is retrieved under a read lock as needed, but {@link Object#equals(Object)} is not performed under a read lock.
	 * @see #readLock()
	 */
	@Override
	public boolean equals(final Object object) {
		if(this == object) {
			return true;
		}
		if(object instanceof ReadWriteLockReference<?> readWriteLockReference) {
			return Objects.equals(get(), readWriteLockReference.get());
		}
		return false;
	}

	@Override
	public String toString() {
		return String.valueOf(get());
	}

}
