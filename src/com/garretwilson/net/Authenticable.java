package com.garretwilson.net;

import java.net.PasswordAuthentication;
import java.net.URI;

/**Presents a method for retrieving user authentication credentials.
@author Garret Wilson
*/
public interface Authenticable
{

	/**Determines user and password information.
	@return The password authentication collected from the user, or <code>null</code> if none is provided.
	*/
//G***fix	public PasswordAuthentication getPasswordAuthentication();

	/**Determines password information for a given user.
	The user must not be allowed to change the username.
	@param username The user for which password information should be gathered.
	@return The password authentication collected from the user, or <code>null</code> if none is provided.
	*/
//G***fix	public PasswordAuthentication getPasswordAuthentication(final String username);

	/**Determines password information in relation to a given URI and description.
	@param uri The URI for which authentication is requested.
	@param prompt A description of the authentication.
	@return The password authentication collected from the user, or <code>null</code> if none is provided.
	*/
	public PasswordAuthentication getPasswordAuthentication(final URI uri, final String prompt);

	/**Determines password information for a given user in relation to a given URI and description.
	The user must not be allowed to change the username, if one is provided.
	@param uri The URI for which authentication is requested.
	@param prompt A description of the authentication.
	@param username The user for which password information should be gathered, or <code>null</code> if the username is not restricted.
	@return The password authentication collected from the user, or <code>null</code> if none is provided.
	*/
	public PasswordAuthentication getPasswordAuthentication(final URI uri, final String prompt, final String username);

}
