package com.garretwilson.lang;

/**Utilities for manipulating boolean objects.
@author Garret Wilson
*/
public class BooleanUtilities
{

	/**This class cannot be publicly instantiated.*/
	private BooleanUtilities() {}

	/**Returns the boolean value of the object, if the object is an instance of
		<code>Boolean</code>.
	@param booleanObject The object for which a boolean value should be returned.
	@return <code>true</code> if the object is an instance of <code>Boolean</code>
		and contains the value <code>true</code>, else <code>false</code>.
	*/
	public final static boolean booleanValue(final Object booleanObject)
	{
		return booleanObject instanceof Boolean ? ((Boolean)booleanObject).booleanValue() : false;  //return false for null or any other non-Boolean object
	}

}