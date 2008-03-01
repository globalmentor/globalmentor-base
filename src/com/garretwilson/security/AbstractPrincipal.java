package com.garretwilson.security;

import java.security.Principal;

import com.globalmentor.util.DefaultIDed;

/**An abstract implementation of a principal.
@author Garret Wilson
*/
public abstract class AbstractPrincipal extends DefaultIDed<String> implements Principal
{

  /**@return The name of user as a principal.
  @see #getID()
  */
  public String getName() {return getID();}

	/**Constructor specifying the principal name.
	@param name The name of the principal.
	*/
	public AbstractPrincipal(final String name)
	{
		super(name);	//create the parent class with the name
	}

	/**Determines if the object is another principal with the same ID.
	@param object The object with which to compare this object.
	@return <code>true<code> if the object is a principal with the same ID.
	*/
	public boolean equals(Object object)
	{
		return object instanceof Principal && super.equals(object);	//see if the object is a principal and it passes the default tests
	}
}
