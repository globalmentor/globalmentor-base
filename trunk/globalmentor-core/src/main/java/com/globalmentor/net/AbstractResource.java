/*
 * Copyright © 2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.net;

import java.net.URI;

import static com.globalmentor.java.Characters.*;

/**
 * Represents an abstract implementation of a resource.
 * @author Garret Wilson
 */
public abstract class AbstractResource implements Resource
{

	/**
	 * {@inheritDoc} This implementation compares the resource URIs. If neither object has a reference URI, the default identity comparison is performed.
	 * @see #getURI()
	 */
	public boolean equals(final Object object)
	{
		if(object instanceof Resource) //if we're being compared with another resource
		{
			final URI uri = getURI(); //get the reference URI
			if(uri != null) //if this resource has a reference URI
			{
				return uri.equals(((Resource) object).getURI()); //compare reference URIs
			}
			else
			//if this resource has no reference URI
			{
				return super.equals(object); //compare normally
			}
		}
		else
		//if the object is not a resource
		{
			return false; //we can't compare this object to a non-resource object
		}
	}

	/**
	 * {@inheritDoc} This implementation returns a hashcode value composed from the reference URI, if available.
	 */
	public int hashCode()
	{
		//return the hash code of the reference URI unless there is no reference ID;
		//  in that case, return the default hash code
		return getURI() != null ? getURI().hashCode() : super.hashCode();
	}

	/**
	 * Returns a reference string representation of the given resource. This version returns the URI, if there is one, between double angle quotation marks;
	 * otherwise, an ID using the resource's hash code is used.
	 * @param resource The resource for which a reference string should be returned.
	 * @return A reference string representation of the resource.
	 */
	public static String toString(final Resource resource)
	{
		final URI uri = resource.getURI(); //get the URI, if any
		return uri != null ? new StringBuilder().append(LEFT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR).append(uri)
				.append(RIGHT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR).toString() : "resource" + Integer.toHexString(resource.hashCode()); //return the URI, if available
	}

	/**
	 * Returns a string representation of the resource. This version returns the URI, if there is one, between double angle quotation marks; otherwise the default
	 * string representation of the object is returned.
	 * @return A string representation of the resource.
	 */
	public String toString()
	{
		final URI uri = getURI(); //get the URI, if any
		return uri != null ? new StringBuilder().append(LEFT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR).append(uri)
				.append(RIGHT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR).toString() : super.toString(); //return the URI, if available
	}

}