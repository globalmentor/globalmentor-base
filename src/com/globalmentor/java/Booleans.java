/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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