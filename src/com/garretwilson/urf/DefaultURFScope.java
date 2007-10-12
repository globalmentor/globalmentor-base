package com.garretwilson.urf;

import java.util.concurrent.locks.*;

/**Default implementation of a scope of URF properties.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) <http://www.urf.name/> specification and processing
written by Garret Wilson <http://www.garretwilson.com/> and Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
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
