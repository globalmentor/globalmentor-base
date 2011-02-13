/*
 * Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.urf;

import static com.globalmentor.java.Objects.checkInstance;

import java.util.concurrent.locks.*;

/**Default implementation of a scope of URF properties.
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
