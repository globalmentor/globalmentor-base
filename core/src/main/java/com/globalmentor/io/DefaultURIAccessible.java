/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.io;

import java.io.*;
import java.net.URI;

import com.globalmentor.beans.BoundPropertyObject;
import static com.globalmentor.net.URIs.*;

/**
 * Default implementation of a class that allows access to resources by providing input streams and indicating a base URI against which relative URIs should be
 * resolved.
 * @author Garret Wilson
 * @deprecated
 */
public class DefaultURIAccessible extends BoundPropertyObject implements URIAccessible {

	/** The username, or <code>null</code> for no user information. */
	private String username = null; //TODO later transfer this stuff to an authenticator plugin

	/** @return The username, or <code>null</code> for no user information. */
	public String getUsername() {
		return username;
	}

	/**
	 * Sets the username.
	 * @param username The username, or <code>null</code> for no user information.
	 */
	public void setUsername(final String username) {
		this.username = username;
	}

	/** The user password, or <code>null</code> for no user information. */
	private char[] password = null;

	/** @return The user password, or <code>null</code> for no user information. */
	public char[] getPassword() {
		return null;
	}

	/**
	 * Sets the user password.
	 * @param password The user password, or <code>null</code> for no user information.
	 */
	public void setPassword(final char[] password) {
		this.password = password;
	}

	/** The lazily-created singleton default instance of a default implementation with no base URI and default stream access. */
	private static URIAccessible defaultURIAccessible = null;

	/** @return The lazily-created singleton default instance of a default implementation with no base URI and default stream access. */
	public static URIAccessible getDefaultURIAccessible() {
		if(defaultURIAccessible == null) { //if the default URI accessible object has not yet been created
			defaultURIAccessible = new DefaultURIAccessible(); //create a default instance of the default class
		}
		return defaultURIAccessible; //return the default object
	}

	/** The implementation to use for retrieving an input stream to a URI. */
	private final URIInputStreamable uriInputStreamable;

	/** @return The non-<code>null</code> implementation to use for retrieving an input stream to a URI. */
	//TODO fix		public URIInputStreamable getURIInputStreamable() {return uriInputStreamable;}	//TODO refactor these into a ProxyURIAccessible

	/** The implementation to use for retrieving an output stream to a URI. */
	private final URIOutputStreamable uriOutputStreamable;

	/** @return The non-<code>null</code> implementation to use for retrieving an output stream to a URI. */
	//TODO fix		public URIOutputStreamable getURIOutputStreamable() {return uriOutputStreamable;}

	/** Default constructor. */
	public DefaultURIAccessible() {
		this(null, null);
	}

	/**
	 * URI input stream locator constructor.
	 * @param uriInputStreamable The implementation to use for accessing a URI for input, or <code>null</code> if the default implementation should be used.
	 */
	public DefaultURIAccessible(final URIInputStreamable uriInputStreamable) {
		this(uriInputStreamable, null);
	}

	/**
	 * URI output stream locator constructor.
	 * @param uriOutputStreamable The implementation to use for accessing a URI for output, or <code>null</code> if the default implementation should be used.
	 */
	public DefaultURIAccessible(final URIOutputStreamable uriOutputStreamable) {
		this(null, uriOutputStreamable);
	}

	/**
	 * Full constructor.
	 * @param uriInputStreamable The implementation to use for accessing a URI for input, or <code>null</code> if the default implementation should be used.
	 * @param uriOutputStreamable The implementation to use for accessing a URI for output, or <code>null</code> if the default implementation should be used.
	 */
	protected DefaultURIAccessible(final URIInputStreamable uriInputStreamable, final URIOutputStreamable uriOutputStreamable) {
		this.uriInputStreamable = uriInputStreamable; //save the URI input stream locator
		this.uriOutputStreamable = uriOutputStreamable; //save the URI output stream locator
	}

	/**
	 * Returns an input stream for the given URI. The calling class has the responsibility for closing the input stream.
	 * @param uri A URI to a resource.
	 * @return An input stream to the contents of the resource represented by the given URI.
	 * @throws IOException Thrown if an I/O error occurred.
	 */
	public InputStream getInputStream(final URI uri) throws IOException {
		if(uriInputStreamable != null) { //if we have a delegate input streamable
			return uriInputStreamable.getInputStream(uri); //delegate to the stored implementation
		}
		return uri.toURL().openConnection().getInputStream(); //convert the URI to a URL, open a connection to it, and get an input stream to it
	}

	/**
	 * Returns an output stream for the given URI. The calling class has the responsibility for closing the output stream.
	 * @param uri A URI to a resource.
	 * @return An output stream to the contents of the resource represented by the given URI.
	 * @throws IOException Thrown if an I/O error occurred.
	 */
	public OutputStream getOutputStream(final URI uri) throws IOException {
		if(uriOutputStreamable != null) { //if we have a delegate output streamable
			return uriOutputStreamable.getOutputStream(uri); //delegate to the stored implementation
		}
		final String scheme = uri.getScheme(); //see what type of URI this is
		if(FILE_SCHEME.equals(scheme)) { //if this is a file URI
			return new FileOutputStream(new File(uri)); //create and return an output stream to the file
		}
		return uri.toURL().openConnection().getOutputStream(); //convert the URI to a URL, open a connection to it, and get an output stream to it
	}

}
