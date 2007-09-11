package com.garretwilson.urf;

import java.util.concurrent.locks.*;

/**Default implementation of a scope of URF properties.
@author Garret Wilson
*/
public class DefaultURFScope extends AbstractURFScope
{

	/**Read write lock and parent scope constructor.
	@param readWriteLock The lock for controlling access to the properties.
	@param parentScope The parent scope of this scope, or <code>null</code> if this scope has no parent scope.
	@exception NullPointerException if the given lock is <code>null</code>.
	*/
	public DefaultURFScope(final ReadWriteLock readWriteLock, final URFScope parentScope)
	{
		super(readWriteLock, parentScope);	//construct the parent class
	}

}
