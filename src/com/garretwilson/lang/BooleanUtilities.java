package com.garretwilson.lang;

/**Utilities for manipulating boolean objects.
@author Garret Wilson
*/
public class BooleanUtilities
{

	/**This class cannot be publicly instantiated.*/
	private BooleanUtilities() {}

	/**Returns a <code>Boolean</code> object with the given boolean value.
	@param b The boolean value to wrap in a <code>Boolean</code> object.
	@return <code>Boolean.TRUE</code> or <code>Boolean.FALSE</code> if the value
		is <code>true</code> or <code>false</code>, respectively.
	*/
	public final static Boolean toBoolean(final boolean b)
	{
		return b ? Boolean.TRUE : Boolean.FALSE;
	}

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