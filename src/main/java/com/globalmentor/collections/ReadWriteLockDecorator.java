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

package com.globalmentor.collections;

import java.util.concurrent.locks.*;

import static com.globalmentor.java.Objects.*;

/**
 * A read/write lock that decorates another read/write lock.
 * @author Garret Wilson
 */
public class ReadWriteLockDecorator implements ReadWriteLock {

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

	/**
	 * Read write lock constructor.
	 * @param readWriteLock The lock for controlling access to the properties.
	 * @throws NullPointerException if the given lock is <code>null</code>.
	 */
	public ReadWriteLockDecorator(final ReadWriteLock readWriteLock) {
		this.readWriteLock = checkInstance(readWriteLock, "Read write lock cannot be null.");
	}

}