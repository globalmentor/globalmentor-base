package com.garretwilson.urf;

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

	/**Value constructor.
	@param value The property value.
	@exception NullPointerException if the given value is <code>null</code>.
	*/
	public URFValueContext(final URFResource value)
	{
		this.value=checkInstance(value, "Value cannot be null.");
	}

}
