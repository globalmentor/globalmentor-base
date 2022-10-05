/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.net;

import java.net.PasswordAuthentication;
import java.net.URI;

import static java.util.Objects.*;

/**
 * A default class for providing a username and password. A single {@link PasswordAuthentication} instance is stored and provided when requested.
 * @author Garret Wilson
 */
public final class DefaultPasswordAuthenticable extends AbstractAuthenticable {

	/** The password authentication to provide. */
	private final PasswordAuthentication passwordAuthentication;

	/** @return The password authentication to provide. */
	public PasswordAuthentication getPasswordAuthentication() {
		return passwordAuthentication;
	}

	/**
	 * Username and password constructor.
	 * <p>
	 * The given password is cloned before it is stored in a new {@link PasswordAuthentication} object.
	 * </p>
	 * @param username The user's username.
	 * @param password the user's password.
	 * @throws NullPointerException if the given username and/or password is <code>null</code>.
	 */
	public DefaultPasswordAuthenticable(final String username, final char[] password) {
		this(new PasswordAuthentication(requireNonNull(username, "Username must be provided."), requireNonNull(password, "Password must be provided."))); //construct the class with the username and password stored in a new password authentication objet
	}

	/**
	 * Password authentication constructor.
	 * @param passwordAuthentication The password authentication to provide.
	 * @throws NullPointerException if the given password authentication is <code>null</code>.
	 */
	public DefaultPasswordAuthenticable(final PasswordAuthentication passwordAuthentication) {
		this.passwordAuthentication = requireNonNull(passwordAuthentication); //store the password authentication
	}

	/**
	 * Determines password information for a given user in relation to a given URI and description. The user must not be allowed to change the username, if one is
	 * provided. This implementation returns the stored {@link PasswordAuthentication} instance.
	 * @param uri The URI for which authentication is requested, or <code>null</code> if there is no relevant URI.
	 * @param prompt A description of the authentication.
	 * @param username The user for which password information should be gathered, or <code>null</code> if the username is not restricted.
	 * @return The password authentication collected from the user, or <code>null</code> if none is provided.
	 */
	public PasswordAuthentication getPasswordAuthentication(final URI uri, final String prompt, final String username) {
		return getPasswordAuthentication(); //return the stored password authentication object
	}

}