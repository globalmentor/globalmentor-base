/* Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
 * All Rights Reserved.
 * 
 * Use is subject to the BSD-style license at
 * <https://svn.globalmentor.com/java/src/com/globalmentor/license.txt>.
 */

package com.globalmentor.java;

/**Utilities for manipulating boolean objects.
@author Garret Wilson
*/
public class Booleans
{

	/**This class cannot be publicly instantiated.*/
	private Booleans() {}

	/**Returns the boolean value of the object, if the object is an instance of
		{@link Boolean}.
	@param booleanObject The object for which a boolean value should be returned.
	@return <code>true</code> if the object is an instance of {@link Boolean}
		and contains the value <code>true</code>, else <code>false</code>.
	*/
	public final static boolean booleanValue(final Object booleanObject)
	{
		return booleanObject instanceof Boolean ? ((Boolean)booleanObject).booleanValue() : false;  //return false for null or any other non-Boolean object
	}

	/**Parses a string and returns its boolean value.
	@param string A string expected to contain "true" or "false".
	@return {@link Boolean#TRUE} if the value is "true", or {@link Boolean#FALSE} if the value is "true".
	@exception NullPointerException if the given string is <code>null</code>
	@exception IllegalArgumentException if the string neither "true" nor "false".
	*/
	public final static Boolean parseBoolean(final String string)
	{
		if(string.equals(Boolean.TRUE.toString()))	//"true"
		{
			return Boolean.TRUE;	//return true
		}
		else if(string.equals(Boolean.FALSE.toString()))	//"false"
		{
			return Boolean.FALSE;	//return false
		}
		else	//if the string is neither "true" nor "false"
		{
			throw new IllegalArgumentException("The boolean string \""+string+"\" is neither \"true\" nor \"false\".");
		}	
	}

}