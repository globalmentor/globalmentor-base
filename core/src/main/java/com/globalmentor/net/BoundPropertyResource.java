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

import static com.globalmentor.java.Characters.*;

import java.net.URI;

import com.globalmentor.beans.BoundPropertyObject;

/**
 * Represents the default implementation of a resource that recognizes bound properties. This class provides compare functionality that sorts according to the
 * reference URI, if available.
 * @author Garret Wilson
 * @see DefaultResource
 */
public class BoundPropertyResource extends BoundPropertyObject implements Resource, Comparable<Resource> {

	/** The resource identifier URI, or <code>null</code> if the identifier is not known. */
	private URI referenceURI;

	/** @return The resource identifier URI, or <code>null</code> if the identifier is not known. */
	public URI getURI() {
		return referenceURI;
	}

	/**
	 * Sets the reference URI of the resource.
	 * @param uri The new reference URI, or <code>null</code> if the identifier is not known.
	 */
	public void setReferenceURI(final URI uri) {
		referenceURI = uri;
	}

	/** Default constructor that allows the reference URI to be set later. */
	protected BoundPropertyResource() {
		this(null); //construct the class without a reference URI
	}

	/**
	 * Constructs a resource with a reference URI.
	 * @param referenceURI The reference URI for the new resource.
	 */
	protected BoundPropertyResource(final URI referenceURI) {
		this.referenceURI = referenceURI; //set the reference URI
	}

	/**
	 * Compares the resource reference URIs. If neither object has a reference URI, the default identity comparison is performed.
	 * @param object The object with which to compare this resource.
	 * @return <code>true</code> if this resource equals that specified in <code>object</code>.
	 * @see #getURI()
	 * @see DefaultResource#equals(Object)
	 */
	public boolean equals(final Object object) {
		if(object instanceof Resource) { //if we're being compared with another resource
			final URI referenceURI = getURI(); //get the reference URI
			if(referenceURI != null) { //if this resource has a reference URI
				return referenceURI.equals(((Resource)object).getURI()); //compare reference URIs
			} else { //if this resource has no reference URI
				return super.equals(object); //compare normally
			}
		} else { //if the object is not a resource
			return false; //we can't compare this object to a non-resource object
		}
	}

	/** @return A hashcode value composed from the reference URI, if available. */
	public int hashCode() {
		//return the hash code of the reference URI unless there is no reference ID;
		//  in that case, return the default hash code
		return getURI() != null ? getURI().hashCode() : super.hashCode();
	}

	/**
	 * Compares this object to another object.
	 * <p>
	 * This method determines order based upon the reference URI of the resource, if any; otherwise, the hash codes of the resources are compared. The resource
	 * with no URI is sorted before a resource with a URI. The same resource will always be considered equal.
	 * </p>
	 * @param resource The resource with which to compare this resouce.
	 * @return A negative integer, zero, or a positive integer as this resource reference URI is less than, equal to, or greater than the reference URI of the
	 *         specified resource, respectively.
	 * @see #getURI()
	 * @see #hashCode()
	 * @see DefaultResource#compareTo(Resource)
	 */
	public int compareTo(final Resource resource) {
		if(this == resource) { //if this resource is being compared to itself
			return 0; //the resources are identical
		}
		final URI uri = getURI(); //get this resource's URI
		final URI resourceURI = resource.getURI(); //get the other resource's URI
		if(uri != null) { //if this resource has a URI
			if(resourceURI != null) { //if the other resource has a URI
				return uri.compareTo(resourceURI); //compare reference URIs
			} else { //if the other resource has no URI
				return 1; //sort resources with no URI first
			}
		} else { //if this resource has no URI
			if(resourceURI != null) { //if the other resource has a URI
				return -1; //sort resources with no URI first
			} else { //if the other resource has no URI
				return hashCode() - resource.hashCode(); //compare hash codes TODO improve
			}
		}
	}

	/**
	 * Returns a string representation of the resource. This version returns the URI, if there is one, between double angle quotation marks; otherwise the default
	 * string representation of the object is returned.
	 * @return A string representation of the resource.
	 */
	public String toString() {
		final URI uri = getURI(); //get the URI, if any
		return uri != null ? new StringBuilder().append(LEFT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR).append(uri.toString())
				.append(RIGHT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR).toString() : super.toString(); //return the URI, if available
	}
}