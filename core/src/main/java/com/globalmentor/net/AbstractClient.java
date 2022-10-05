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

import java.util.*;
import static java.util.Collections.*;
import java.net.*;

import com.globalmentor.net.Authenticable;

/**
 * Represents the state of a network client. Keeps a cache of authentication information for visited domains and realms.
 * @author Garret Wilson
 */
public class AbstractClient implements Client {

	/** Whether this client logs its communication. */
	private boolean logged = false;

	/** @return Whether this client logs its communication. */
	public boolean isLogged() {
		return logged;
	}

	/**
	 * Sets whether this client logs its communication.
	 * @param logged <code>true</code> if this client should log its communication.
	 */
	public void setLogged(final boolean logged) {
		this.logged = logged;
	}

	/** @return The default instance of this class, or <code>null</code> if there is no default instance. */
	protected AbstractClient getDefaultInstance() {
		return null; //by default there is no default instance of this class
	}

	/** The authenticator object used to retrieve client authentication. */
	private Authenticable authenticator;

	/**
	 * Sets the authenticator object used to retrieve client authentication.
	 * @param authenticable The object to retrieve authentication information regarding a client.
	 */
	public void setAuthenticator(final Authenticable authenticable) {
		authenticator = authenticable;
	}

	/**
	 * @return The authenticator associated with this client or, if there is no authenticator defined, the authenticator associated with the default instance of
	 *         the client.
	 */
	protected Authenticable getAuthenticator() {
		if(authenticator != null) //if we have an authenticator defined
			return authenticator; //return the authenticator
		else if(getDefaultInstance() != this) //if the default instance is not this object
			return getDefaultInstance().getAuthenticator(); //ask the default instance for the authenticator
		else
			//if this class is the default client
			return null; //we've already determined we don't have an authenticator, so return null
	}

	/** Default constructor with no authenticator. */
	public AbstractClient() {
		this(null); //construct the class with no authenticator
	}

	/**
	 * Authenticator constructor.
	 * @param authenticator The authenticator to use for this client, or <code>null</code> if the default authenticator should be used if available.
	 */
	public AbstractClient(final Authenticable authenticator) {
		this.authenticator = authenticator; //save the authenticator
	}

	/** The ID of the user to which this client is restricted, or <code>null</code> if this client is not restricted to a single user. */
	private String username;

	/** @return The ID of the user to which this client is restricted, or <code>null</code> if this client is not restricted to a single user. */
	public String getUsername() {
		return username;
	}

	/**
	 * Restricts this client to particular user.
	 * @param username The ID of the user to which this client is restricted, or <code>null</code> if this client should not restricted to a single user.
	 */
	public void setUsername(final String username) {
		this.username = username;
	}

	//TODO make these maps synchronized
	/** The map of passwords keyed to users, keyed to realms, keyed to root URIs. */
	private final Map<URI, Map<String, Map<String, char[]>>> authenticationMap = new HashMap<URI, Map<String, Map<String, char[]>>>(); //TODO later store digest H(A1) information rather than raw passwords

	/**
	 * Retrieves the realms stored for a given root URI.
	 * @param rootURI The root URI of the domain governing the realms and user passwords.
	 * @return A read-only set of realms of the given root URI.
	 */
	public Set<String> getRealms(final URI rootURI) {
		final Map<String, Map<String, char[]>> realmMap = authenticationMap.get(rootURI); //get the map with realm keys
		if(realmMap != null) { //if we found this realm
			return unmodifiableSet(realmMap.keySet()); //return the set of realms for this domain
		}
		return emptySet(); //we don't have authentication information for any users for this domain and realm
	}

	/**
	 * Retrieves any users for which authentication information is stored for a given realm of a given root URI.
	 * @param rootURI The root URI of the domain governing the realms and user passwords.
	 * @param realm The realm of protection.
	 * @return A read-only set of usernames for which authentication information is stored in this for the given realm of the given root URI.
	 */
	public Set<String> getUsernames(final URI rootURI, final String realm) {
		final Map<String, Map<String, char[]>> realmMap = authenticationMap.get(rootURI); //get the map with realm keys
		if(realmMap != null) { //if we found this realm
			final Map<String, char[]> userMap = realmMap.get(realm); //get the map of users and passwords
			return unmodifiableSet(userMap.keySet()); //return the set of usernames for this domain and realm
		}
		return emptySet(); //we don't have authentication information for any users for this domain and realm
	}

	/**
	 * Retrieves the password stored for a given user in a given realm of a given root URI.
	 * @param rootURI The root URI of the domain governing the realms and user passwords.
	 * @param realm The realm of protection.
	 * @param username The user for which a password is stored.
	 * @return The password of the given user, or <code>null</code> if no password is present for the given root URI, realm, and user.
	 */
	public char[] getPassword(final URI rootURI, final String realm, final String username) {
		final Map<String, Map<String, char[]>> realmMap = authenticationMap.get(rootURI); //get the map with realm keys
		if(realmMap != null) { //if we found this realm
			final Map<String, char[]> userMap = realmMap.get(realm); //get the map of users and passwords
			if(userMap != null) { //if there is a map for users
				final char[] password = userMap.get(username); //get the password for this user
				return password != null ? password.clone() : null; //return the password TODO check and improve cloning
			}
		}
		return null; //indicate that we don't have a password stored
	}

	/**
	 * Caches the password for a given user in a given realm of a given root URI.
	 * @param rootURI The root URI of the domain governing the realms and user passwords.
	 * @param realm The realm of protection.
	 * @param username The user for which a password is stored.
	 * @param password The password of the given user.
	 */
	public void putPassword(final URI rootURI, final String realm, final String username, final char[] password) {
		Map<String, Map<String, char[]>> realmMap = authenticationMap.get(rootURI); //get the map with realm keys
		if(realmMap == null) { //if we don't have a map for this domain
			realmMap = new HashMap<String, Map<String, char[]>>(); //create a new map keyed to realms
			authenticationMap.put(rootURI, realmMap); //key the map of realms to the domain
		}
		Map<String, char[]> userMap = realmMap.get(realm); //get the map of users and passwords
		if(userMap == null) { //if we don't have a map for this realm
			userMap = new HashMap<String, char[]>(); //create a new map of users and passwords
			realmMap.put(realm, userMap); //key the map of users to the realm
		}
		userMap.put(username, password.clone()); //store the password in the map, keyed to the user TODO see if we can just store the hashes somewhere
	}

	/**
	 * Removes the password stored for a given user in a given realm of a given root URI.
	 * @param rootURI The root URI of the domain governing the realms and user passwords.
	 * @param realm The realm of protection.
	 * @param username The user for which a password is stored.
	 */
	public void removePassword(final URI rootURI, final String realm, final String username) {
		final Map<String, Map<String, char[]>> realmMap = authenticationMap.get(rootURI); //get the map with realm keys
		if(realmMap != null) { //if we found this realm
			final Map<String, char[]> userMap = realmMap.get(realm); //get the map of users and passwords
			if(userMap != null) { //if there is a map for users
				userMap.remove(username); //remove any password stored for this user
				if(userMap.size() == 0) { //if we removed the last password
					realmMap.remove(userMap); //remove the user map from the map of users keyed to realms
					if(realmMap.size() == 0) { //if we removed the last realm
						authenticationMap.remove(realmMap); //remove the map of users keyed to realms
					}
				}
			}
		}
	}

	/**
	 * Determines password information in relation to a given URI and description.
	 * @param uri The URI for which authentication is requested.
	 * @param prompt A description of the authentication.
	 * @return The password authentication collected from the user, or <code>null</code> if none is provided.
	 */
	public PasswordAuthentication getPasswordAuthentication(final URI uri, final String prompt) {
		final Authenticable authenticator = getAuthenticator(); //see if an authenticator has been specified
		if(authenticator != null) { //if we have an authenticator
			return authenticator.getPasswordAuthentication(uri, prompt, username); //ask the authenticator for the password, restricting authentication to a given user if appropriate 
		} else {
			/*TODO fix
			final DigestAuthenticateChallenge digestChallenge=(DigestAuthenticateChallenge)challenge;	//get the challenge as a digest challenge
			final Host host=request.getHost();	//get the host of the request, as we may have been redirected
			final int port=host.getPort()>=0 ? host.getPort() : DEFAULT_PORT;	//TODO maybe force host to have a port
			Log.trace("getting password authentication");
			final PasswordAuthentication passwordAuthentication=Authenticator.requestPasswordAuthentication(host.getName(), getInetAddress(), port,
			response.getVersion().toString(), "You must enter a username and password to access this resource at \""+digestChallenge.getRealm()+"\".", digestChallenge.getScheme().toString());
			Log.trace("got password authentication", passwordAuthentication);
			*/
		}
		return null; //we couldn't retrieve a password
	}

}
