package com.garretwilson.util;

import java.util.concurrent.locks.*;

import static com.garretwilson.lang.ObjectUtilities.*;

/**A read/write lock that decorates another read/write lock.
@author Garret Wilson
*/
public class ReadWriteLockDecorator implements ReadWriteLock
{

	/**The decorated read write lock.*/
	private final ReadWriteLock readWriteLock;

	/**Returns the lock used for reading.
	@return the lock used for reading.
	*/
	public Lock readLock() {return readWriteLock.readLock();}

	/**Returns the lock used for writing.
	@return the lock used for writing.
	*/
	public Lock writeLock() {return readWriteLock.writeLock();}

	/**Read write lock constructor.
	@param readWriteLock The lock for controlling access to the properties.
	@exception NullPointerException if the given lock is <code>null</code>.
	*/
	public ReadWriteLockDecorator(final ReadWriteLock readWriteLock)
	{
		this.readWriteLock=checkInstance(readWriteLock, "Read write lock cannot be null.");
	}

}
