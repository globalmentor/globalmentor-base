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

	/**Parses a string and returns its boolean value.
	@param string A string expected to contain "true" or "false".
	@return <code>Boolean.TRUE</code> or <code>Boolean.TRUE</code> if the value is "true" or "false",
		respectively, and <code>null</code> if the string is <code>null</code>.
	@exception IllegalArgumentException if the string is not <code>null</code> and neither "true" nor "false".
	*/
	public final static Boolean parseBoolean(final String string)
	{
		if(string!=null)
		{
			if(Boolean.TRUE.toString().equals(string))	//"true"
			{
				return Boolean.TRUE;	//return true
			}
			else if(Boolean.FALSE.toString().equals(string))	//"false"
			{
				return Boolean.FALSE;	//return false
			}
			else	//if the string is neither "true" nor "false"
			{
				throw new IllegalArgumentException("The string \""+string+"\" is neither \"true\" nor \"false\".");
			}
		}
		else	//if the string is null
		{
			return null;	//return null
		}
		
	}

}