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

import java.net.URI;

/**
 * A class indicating that a resource is in the wrong state or a precondition for an operation related to the resource failed.
 * @author Garret Wilson
 */
public class ResourceStateException extends ResourceIOException {

	private static final long serialVersionUID = 1L;

	/**
	 * Constructs a new exception for the specified resource URI.
	 * @param resourceURI The URI of the resource to which the exception is related.
	 * @throws NullPointerException if the given resource URI is <code>null</code>.
	 */
	public ResourceStateException(final URI resourceURI) {
		this(resourceURI, (String)null); //construct the exception with the resource URI and no message
	}

	/**
	 * Constructs a new exception with the specified resource URI and detail message.
	 * @param resourceURI The URI of the resource to which the exception is related.
	 * @param message The detail message.
	 * @throws NullPointerException if the given resource URI is <code>null</code>.
	 */
	public ResourceStateException(final URI resourceURI, final String message) {
		this(resourceURI, message, null); //construct the class with no cause
	}

	/**
	 * Constructs a new exception with the specified resource URI and cause, along with a detail message derived from the cause.
	 * @param resourceURI The URI of the resource to which the exception is related.
	 * @param cause The cause, or <code>null</code> to indicate the cause is nonexistent or unknown.
	 * @throws NullPointerException if the given resource URI is <code>null</code>.
	 */
	public ResourceStateException(final URI resourceURI, final Throwable cause) {
		this(resourceURI, cause != null ? cause.toString() : null, cause); //create an exception with a generated detail message
	}

	/**
	 * Constructs a new exception with the specified resource URI, detail message, and cause.
	 * @param resourceURI The URI of the resource to which the exception is related.
	 * @param message The detail message.
	 * @param cause The cause, or <code>null</code> to indicate the cause is nonexistent or unknown.
	 * @throws NullPointerException if the given resource URI is <code>null</code>.
	 */
	public ResourceStateException(final URI resourceURI, final String message, final Throwable cause) {
		super(resourceURI, message, cause); //construct the parent class
	}

}
