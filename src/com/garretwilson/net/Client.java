package com.garretwilson.net;

/**Represents the state of a network client.
@author Garret Wilson
*/
public interface Client
{

	/**@return Whether this client logs its communication.*/
	public boolean isLogged();

	/**Sets whether this client logs its communication.
	@param logged <code>true</code> if this client should log its communication.
	*/
	public void setLogged(final boolean logged);
}