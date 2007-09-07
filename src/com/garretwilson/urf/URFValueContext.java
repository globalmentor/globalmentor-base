package com.garretwilson.urf;

import java.util.concurrent.locks.ReadWriteLock;

import static com.garretwilson.lang.ObjectUtilities.*;

/**An URF value with its context.
@author Garret Wilson
*/
public class URFValueContext extends URFPropertyManager
{

	/**The URF resource serving as the value.*/
	private final URFResource value;

		/**@return The URF resource serving as the value.*/
		public URFResource getValue() {return value;}

	/**Read write lock and value constructor.
	@param readWriteLock The lock for controlling access to the properties.
	@param value The property value.
	@exception NullPointerException if the given lock and/or value is <code>null</code>.
	*/
	public URFValueContext(final ReadWriteLock readWriteLock, final URFResource value)
	{
		super(readWriteLock);	//construct the parent class
		this.value=checkInstance(value, "Value cannot be null.");
	}
}
