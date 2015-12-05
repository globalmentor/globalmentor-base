/*
 * Copyright © 2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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
 * Abstract implementation of some operation that can be executed, with state governed by a read/write lock.
 * 
 * <p>
 * This implementation is thread-safe, providing its own read/write lock. The state is governed by this read/write lock, which can also be used for locking
 * other operation resources.
 * </p>
 * 
 * @author Garret Wilson
 */
public abstract class AbstractReadWriteLockOperation extends AbstractOperation implements ReadWriteLock {

	/** The decorated read write lock. */
	private final ReadWriteLock readWriteLock;

	/**
	 * Returns the lock used for reading.
	 * @return the lock used for reading.
	 */
	public Lock readLock() {
		return readWriteLock.readLock();
	}

	/**
	 * Returns the lock used for writing.
	 * @return the lock used for writing.
	 */
	public Lock writeLock() {
		return readWriteLock.writeLock();
	}

	/** Default constructor using a reentrant read/write lock. */
	public AbstractReadWriteLockOperation() {
		this(new ReentrantReadWriteLock());
	}

	/**
	 * Read write lock constructor.
	 * @param readWriteLock The lock for controlling access to the properties.
	 * @throws NullPointerException if the given lock is <code>null</code>.
	 */
	public AbstractReadWriteLockOperation(final ReadWriteLock readWriteLock) {
		this.readWriteLock = checkInstance(readWriteLock, "Read write lock cannot be null.");
	}

	@Override
	public TaskState getState() {
		readLock().lock();
		try {
			return super.getState();
		} finally {
			readLock().unlock();
		}
	}

	@Override
	public synchronized void setState(final TaskState newState) {
		writeLock().lock();
		try {
			super.setState(newState);
		} finally {
			writeLock().unlock();
		}
	}

}
