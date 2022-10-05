/*
 * Copyright © 1996-2009 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import com.globalmentor.io.*;

import static com.globalmentor.java.Classes.*;

/**
 * A model of a resource.
 * <p>
 * Bound properties:
 * </p>
 * <dl>
 * <dt>{@link #RESOURCE_PROPERTY} ({@link Resource})</dt>
 * <dd>Indicates that the resource property has been changed.</dd>
 * </dl>
 * @author Garret Wilson
 * @see #RESOURCE_PROPERTY
 */
@Deprecated
public class ResourceModel<R extends Resource> extends URIAccessibleModel {

	/** The resource property. */
	public final String RESOURCE_PROPERTY = getFullName(ResourceModel.class, "resource");

	/** The resource being modeled, or <code>null</code> if there is no resource. */
	private R resource = null;

	/** @return The resource being modeled, or <code>null</code> if there is no resource. */
	public R getResource() {
		return resource;
	}

	/**
	 * Sets the resource being modeled. This is a bound property.
	 * @param newResource The resource being modeled, or <code>null</code> if there is no resource.
	 */
	public void setResource(final R newResource) {
		final R oldResource = resource; //get the old value
		if(oldResource != newResource) { //if the value is really changing
			resource = newResource; //update the value
			//show that the property has changed
			firePropertyChange(RESOURCE_PROPERTY, oldResource, newResource);
		}
	}

	/** Default constructor. */
	public ResourceModel() {
		this((R)null);
	}

	/**
	 * Resource constructor.
	 * @param resource The resource being modeled, or <code>null</code> if there is no resource.
	 */
	public ResourceModel(final R resource) {
		this(resource, resource != null ? resource.getURI() : (URI)null);
	}

	/**
	 * Base URI constructor.
	 * @param baseURI The base URI of the model, or <code>null</code> if unknown.
	 */
	public ResourceModel(final URI baseURI) {
		this((R)null, baseURI);
	}

	/**
	 * Resource and base URI constructor.
	 * @param resource The resource being modeled, or <code>null</code> if there is no resource.
	 * @param baseURI The base URI of the model, or <code>null</code> if unknown.
	 */
	public ResourceModel(final R resource, final URI baseURI) {
		this(resource, baseURI, null, null);
	}

	/**
	 * URI input stream locator constructor.
	 * @param uriInputStreamable The implementation to use for accessing a URI for input, or <code>null</code> if the default implementation should be used.
	 */
	public ResourceModel(final URIInputStreamable uriInputStreamable) {
		this((R)null, uriInputStreamable);
	}

	/**
	 * Resource and URI input stream locator constructor.
	 * @param resource The resource being modeled, or <code>null</code> if there is no resource.
	 * @param uriInputStreamable The implementation to use for accessing a URI for input, or <code>null</code> if the default implementation should be used.
	 */
	public ResourceModel(final R resource, final URIInputStreamable uriInputStreamable) {
		this(resource, uriInputStreamable, (URIOutputStreamable)null);
	}

	/**
	 * Resource and URI output stream locator constructor.
	 * @param resource The resource being modeled, or <code>null</code> if there is no resource.
	 * @param uriOutputStreamable The implementation to use for accessing a URI for output, or <code>null</code> if the default implementation should be used.
	 */
	public ResourceModel(final R resource, final URIOutputStreamable uriOutputStreamable) {
		this(resource, (URIInputStreamable)null, uriOutputStreamable);
	}

	/**
	 * Base URI and input stream locator constructor.
	 * @param baseURI The base URI of the model, or <code>null</code> if unknown.
	 * @param uriInputStreamable The implementation to use for accessing a URI for input, or <code>null</code> if the default implementation should be used.
	 */
	public ResourceModel(final URI baseURI, final URIInputStreamable uriInputStreamable) {
		this(null, baseURI, uriInputStreamable);
	}

	/**
	 * Resource, base URI, and input stream locator constructor.
	 * @param resource The resource being modeled, or <code>null</code> if there is no resource.
	 * @param baseURI The base URI of the model, or <code>null</code> if unknown.
	 * @param uriInputStreamable The implementation to use for accessing a URI for input, or <code>null</code> if the default implementation should be used.
	 */
	public ResourceModel(final R resource, final URI baseURI, final URIInputStreamable uriInputStreamable) {
		this(resource, baseURI, uriInputStreamable, null);
	}

	/**
	 * Resource, base URI, and URI accessible constructor.
	 * @param resource The resource being modeled, or <code>null</code> if there is no resource.
	 * @param baseURI The base URI of the model, or <code>null</code> if unknown.
	 * @param uriAccessible The implementation to use for accessing a URI for input and output, or <code>null</code> if the default implementation should be used.
	 */
	public ResourceModel(final R resource, final URI baseURI, final URIAccessible uriAccessible) {
		this(resource, baseURI, uriAccessible, uriAccessible); //use the URI accessible object for accessing the URI for input and output
	}

	/**
	 * Resource, base URI, and output stream locator constructor.
	 * @param resource The resource being modeled, or <code>null</code> if there is no resource.
	 * @param baseURI The base URI of the model, or <code>null</code> if unknown.
	 * @param uriOutputStreamable The implementation to use for accessing a URI for output, or <code>null</code> if the default implementation should be used.
	 */
	public ResourceModel(final R resource, final URI baseURI, final URIOutputStreamable uriOutputStreamable) {
		this(resource, baseURI, null, uriOutputStreamable);
	}

	/**
	 * Resource and input/output stream locator constructor.
	 * @param resource The resource being modeled, or <code>null</code> if there is no resource.
	 * @param uriInputStreamable The implementation to use for accessing a URI for input, or <code>null</code> if the default implementation should be used.
	 * @param uriOutputStreamable The implementation to use for accessing a URI for output, or <code>null</code> if the default implementation should be used.
	 */
	public ResourceModel(final R resource, final URIInputStreamable uriInputStreamable, final URIOutputStreamable uriOutputStreamable) {
		this(resource, resource != null ? resource.getURI() : (URI)null, uriInputStreamable, uriOutputStreamable);
	}

	/**
	 * Full constructor.
	 * @param resource The resource being modeled, or <code>null</code> if there is no resource.
	 * @param baseURI The base URI of the model, or <code>null</code> if unknown.
	 * @param uriInputStreamable The implementation to use for accessing a URI for input, or <code>null</code> if the default implementation should be used.
	 * @param uriOutputStreamable The implementation to use for accessing a URI for output, or <code>null</code> if the default implementation should be used.
	 */
	public ResourceModel(final R resource, final URI baseURI, final URIInputStreamable uriInputStreamable, final URIOutputStreamable uriOutputStreamable) {
		super(baseURI, uriInputStreamable, uriOutputStreamable); //construct the parent class
		this.resource = resource; //save the resource
	}

}
