package com.garretwilson.util;

/**Indicates that an object can be verified in some way.
@author Garret Wilson
*/
public interface Verifiable
{

	/**Verifies the object, returning the verification result.
	@return The result of verifying the object; <code>true</code> if the object
		verified correctly, <code>false</code> if not.
	*/
	public boolean verify();
}