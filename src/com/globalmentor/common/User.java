package com.globalmentor.common;

import com.garretwilson.util.IDable;;

/**A representation of an individual.
@author Garret Wilson
*/
public interface User<I> extends IDable<I>
{

	/**@return The first name of the user.*/
	public String getFirstName();

	/**@return The last name of the user.*/
	public String getLastName();

	/**@return The composite name of the user.*/
	public String getName();

	/**@return The password of the user.*/
	public String getPassword();

}
