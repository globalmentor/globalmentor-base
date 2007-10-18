package com.garretwilson.urf;

import static com.garretwilson.lang.ObjectUtilities.checkInstance;

import java.util.concurrent.locks.*;

/**Default implementation of a scope of URF properties.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class DefaultURFScope extends AbstractURFScope
{

	/**Read write lock constructor with no parent scope.
	@param readWriteLock The lock for controlling access to the properties.
	@exception NullPointerException if the given lock is <code>null</code>.
	*/
	public DefaultURFScope(final ReadWriteLock readWriteLock)
	{
		this(readWriteLock, null);	//construct the class with no parent scope
	}

	/**Parent scope constructor.
	The parent scope lock will be used for controlling access to the scope properties.
	@param parentScope The parent scope of this scope.
	@exception NullPointerException if the given parent scope is <code>null</code>.
	*/
	public DefaultURFScope(final URFScope parentScope)
	{
		this(parentScope, checkInstance(parentScope, "Parentscope cannot be null."));	//construct the class using the parent scope as a lock
	}

	/**Read write lock and parent scope constructor.
	@param readWriteLock The lock for controlling access to the properties.
	@param parentScope The parent scope of this scope, or <code>null</code> if this scope has no parent scope.
	@exception NullPointerException if the given lock is <code>null</code>.
	*/
	protected DefaultURFScope(final ReadWriteLock readWriteLock, final URFScope parentScope)
	{
		super(readWriteLock, parentScope);	//construct the parent class
	}

}
