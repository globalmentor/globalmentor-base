package com.garretwilson.urf;

import java.util.concurrent.locks.*;

import com.garretwilson.net.BoundPropertyResource;

/**The default implementation of an URF resource.
This class provides compare functionality that sorts according to the resource URI.
@author Garret Wilson
*/
public class DefaultURFResource extends BoundPropertyResource implements URFResource, ReadWriteLock, Cloneable
{

	/**The decorated read write lock.*/
	private final ReadWriteLock readWriteLock=new ReentrantReadWriteLock();

	/**Returns the lock used for reading.
	@return the lock used for reading.
	*/
	public Lock readLock() {return readWriteLock.readLock();}

	/**Returns the lock used for writing.
	@return the lock used for writing.
	*/
	public Lock writeLock() {return readWriteLock.writeLock();}


}