/*
 * Copyright © 2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.java.Objects.*;

import java.util.concurrent.locks.*;

/**
 * An object that holds another object and provides read/write lock access to the object. This is a convenience class that is the equivalent of an
 * {@link ObjectHolder} governed by a {@link ReadWriteLock}. The object holder itself also functions as read/write lock access.
 * @param <T> The type of object being held.
 * @author Garret Wilson
 */
public class ReadWriteLockObjectHolder<T> extends ObjectHolder<T> implements ReadWriteLock {

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

	/** Default constructor to hold <code>null</code>. */
	public ReadWriteLockObjectHolder() {
		this(new ReentrantReadWriteLock());
	}

	/**
	 * Read write lock constructor.
	 * @param readWriteLock The lock for controlling access to the properties.
	 * @throws NullPointerException if the given lock is <code>null</code>.
	 */
	public ReadWriteLockObjectHolder(final ReadWriteLock readWriteLock) {
		this(null, readWriteLock);
	}

	/**
	 * Held object constructor.
	 * @param object The object to hold.
	 */
	public ReadWriteLockObjectHolder(final T object) {
		this(object, new ReentrantReadWriteLock());
	}

	/**
	 * Held object and read write lock constructor.
	 * @param object The object to hold.
	 * @param readWriteLock The lock for controlling access to the properties.
	 * @throws NullPointerException if the given lock is <code>null</code>.
	 */
	public ReadWriteLockObjectHolder(final T object, final ReadWriteLock readWriteLock) {
		super(object); //construct the parent class
		this.readWriteLock = checkInstance(readWriteLock, "Read write lock cannot be null.");
	}

	/**
	 * {@inheritDoc} This version returns the object under a read lock.
	 * @see #readLock()
	 */
	@Override
	public T getObject() {
		readLock().lock();
		try {
			return super.getObject();
		} finally {
			readLock().unlock();
		}
	}

	/**
	 * {@inheritDoc} This version sets the object under a write lock.
	 * @see #writeLock()
	 */
	@Override
	public void setObject(final T object) {
		writeLock().lock();
		try {
			super.setObject(object);
		} finally {
			writeLock().unlock();
		}
	}

}
