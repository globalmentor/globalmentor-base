package com.garretwilson.security;

import java.util.Date;

/**A generated value to prevent playback attacks in communication protocols.
This interface forces several restrictions on a nonce, allowing recovery of
several information components.
@author Garret Wilson
*/
public interface Nonce
{

	/**@return The time represented by the nonce.*/
	public Date getTime();
	
	/**@return The private key represented by the nonce.*/
	public String getPrivateKey();
	
	/**@returns A string representation of the nonce, suitable for serialization.*/
	public String toString();

}
