package com.garretwilson.net;

import java.net.*;

/**Abstract authenticator implementation.
@author Garret Wilson
*/
public abstract class AbstractAuthenticable /*TODO fix extends Authenticator*/ implements Authenticable
{

	/**Determines password information in relation to a given description.
	This version delegates to {@link #getPasswordAuthentication(URI, String)}.
	@param prompt A description of the authentication.
	@return The password authentication collected from the user, or <code>null</code> if none is provided.
	*/
	public PasswordAuthentication getPasswordAuthentication(final String prompt)
	{
		return getPasswordAuthentication(null, prompt, null);	//get password authentication for no specific URI, allowing any user to be specified
	}

	/**Determines password information in relation to a given URI and description.
	This version delegates to {@link #getPasswordAuthentication(URI, String, String)}.
	@param uri The URI for which authentication is requested, or <code>null</code> if there is no relevant URI.
	@param prompt A description of the authentication.
	@return The password authentication collected from the user, or <code>null</code> if none is provided.
	*/
	public PasswordAuthentication getPasswordAuthentication(final URI uri, final String prompt)
	{
		return getPasswordAuthentication(uri, prompt, null);	//get password authentication, allowing any user to be specified
	}

}
