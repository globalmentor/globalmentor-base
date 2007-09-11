package com.garretwilson.urf;

import java.util.concurrent.locks.ReadWriteLock;

import static com.garretwilson.lang.ObjectUtilities.*;
import com.garretwilson.util.ReadWriteLockDecorator;

/**An URF value with its context.
@author Garret Wilson
*/
public class URFValueContext extends ReadWriteLockDecorator
{

	/**The URF resource serving as the value.*/
	private final URFResource value;

		/**@return The URF resource serving as the value.*/
		public URFResource getValue() {return value;}

	/**The scope of this context.*/ 
	private final URFScope scope;

		/**The scope of this context.*/ 
		public URFScope getScope() {return scope;}

	/**Read write lock and value constructor.
	@param readWriteLock The lock for controlling access to the properties.
	@param value The property value.
	@param scope The scope of this context.
	@exception NullPointerException if the given lock, value, and/or scope is <code>null</code>.
	*/
	public URFValueContext(final ReadWriteLock readWriteLock, final URFResource value, final URFScope scope)
	{
		super(readWriteLock);	//construct the parent class
		this.value=checkInstance(value, "Value cannot be null.");
		this.scope=checkInstance(scope, "Scope cannot be null.");
	}
}
