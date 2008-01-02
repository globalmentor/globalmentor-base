package com.garretwilson.net;

import java.net.PasswordAuthentication;
import java.net.URI;

import static com.globalmentor.java.Objects.*;

/**A default class for providing a username and password.
A single {@link PasswordAuthentication} instance is stored and provided when requested.
@author Garret Wilson
*/
public final class DefaultPasswordAuthenticable extends AbstractAuthenticable
{

	/**The password authentication to provide.*/
	private final PasswordAuthentication passwordAuthentication;

		/**@return The password authentication to provide.*/
		public PasswordAuthentication getPasswordAuthentication() {return passwordAuthentication;}

	/**Username and password constructor.
	<p>The given password is cloned before it is stored in a new {@link PasswordAuthentication} object.</p>
	@param username The user's username.
	@param password the user's password.
	@exception NullPointerException if the given username and/or password is <code>null</code>.
	*/
	public DefaultPasswordAuthenticable(final String username, final char[] password)
	{
		this(new PasswordAuthentication(checkInstance(username, "Username must be provided."), checkInstance(password, "Password must be provided.")));	//construct the class with the username and password stored in a new password authentication objet
	}

	/**Password authentication constructor.
	@param passwordAuthentication The password authentication to provide.
	@exception NullPointerException if the given password authentication is <code>null</code>.
	*/
	public DefaultPasswordAuthenticable(final PasswordAuthentication passwordAuthentication)
	{
		this.passwordAuthentication=checkInstance(passwordAuthentication);	//store the password authentication
	}

	/**Determines password information for a given user in relation to a given URI and description.
	The user must not be allowed to change the username, if one is provided.
	This implementation returns the stored {@link PasswordAuthentication} instance.
	@param uri The URI for which authentication is requested, or <code>null</code> if there is no relevant URI.
	@param prompt A description of the authentication.
	@param username The user for which password information should be gathered, or <code>null</code> if the username is not restricted.
	@return The password authentication collected from the user, or <code>null</code> if none is provided.
	*/
	public PasswordAuthentication getPasswordAuthentication(final URI uri, final String prompt, final String username)
	{
		return getPasswordAuthentication();	//return the stored password authentication object
	}

}