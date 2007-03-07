package com.globalmentor.common;

import java.security.Principal;

import com.garretwilson.util.IDable;

/**A representation of an individual.
@author Garret Wilson
*/
public interface User<I> extends IDable<I>, Principal
{

	/**@return The first name of the user.*/
	public String getFirstName();

	/**@return The last name of the user.*/
	public String getLastName();

	/**@return The composite name of the user.*/
	public String getFullName();

	/**@return The password of the user.*/
	public char[] getPassword();

}
