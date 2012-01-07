/*
 * Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.urf;

import java.io.IOException;
import java.net.URI;
import java.util.concurrent.locks.*;

import com.globalmentor.java.Objects;
import com.globalmentor.net.ContentType;

import static com.globalmentor.java.Objects.*;
import static com.globalmentor.urf.URF.*;

/**Default implementation of an encapsulation of a parent scope, property URI, value, and the associated property-value scope.
<p>Properties calculate {@link #equals(Object)} by whether they have equivalent values for {@link #getPropertyURI()} and {@link #getValue()}.</p>
@author Garret Wilson
*/
public class DefaultURFProperty extends DefaultURFValueContext implements URFProperty
{

	/**The scope to which the property belongs, or <code>null</code> if this is a property definition not attached to any scope.*/
	private final URFScope subjectScope;

		/**@return The scope to which the property belongs, or <code>null</code> if this is a property definition not attached to any scope.*/
		public URFScope getSubjectScope() {return subjectScope;}

	/**The URI of the property.*/
	private final URI propertyURI;

		/**@return The URI of the property.*/
		public URI getPropertyURI() {return propertyURI;}

	/**Property URI and value constructor with no subject scope, a default lock, and a default scope
	@param propertyURI The property URI.
	@param value The property value.
	@exception NullPointerException if the given property URI and/or value is <code>null</code>.
	*/
	public DefaultURFProperty(final URI propertyURI, final URFResource value)
	{
		this(new ReentrantReadWriteLock(), propertyURI, value);	//construct the class with a default lock
	}
	
	/**Property URI and media type value constructor with no subject scope, a default lock, and a default scope
	@param propertyURI The property URI.
	@param value The property value.
	@exception NullPointerException if the given property URI and/or value is <code>null</code>.
	*/
	public DefaultURFProperty(final URI propertyURI, final ContentType value)
	{
		this(propertyURI, DEFAULT_URF_RESOURCE_FACTORY.createMediaTypeResource(value));	//construct the class with a resource created from the value
	}

	/**Property URI and string value constructor with no subject scope, a default lock, and a default scope
	@param propertyURI The property URI.
	@param value The property value.
	@exception NullPointerException if the given property URI and/or value is <code>null</code>.
	*/
	public DefaultURFProperty(final URI propertyURI, final URFDate value)
	{
		this(propertyURI, DEFAULT_URF_RESOURCE_FACTORY.createDateResource(value));	//construct the class with a resource created from the value
	}

	/**Property URI and string value constructor with no subject scope, a default lock, and a default scope
	@param propertyURI The property URI.
	@param value The property value.
	@exception NullPointerException if the given property URI and/or value is <code>null</code>.
	*/
	public DefaultURFProperty(final URI propertyURI, final URFDateTime value)
	{
		this(propertyURI, DEFAULT_URF_RESOURCE_FACTORY.createDateTimeResource(value));	//construct the class with a resource created from the value
	}

	/**Property URI and string value constructor with no subject scope, a default lock, and a default scope
	@param propertyURI The property URI.
	@param value The property value.
	@exception NullPointerException if the given property UR, and/or value is <code>null</code>.
	*/
	public DefaultURFProperty(final URI propertyURI, final String value)
	{
		this(propertyURI, DEFAULT_URF_RESOURCE_FACTORY.createStringResource(value));	//construct the class with a resource created from the value
	}

	/**Property URI and long value constructor with no subject scope, a default lock, and a default scope
	@param propertyURI The property URI.
	@param value The property value.
	@exception NullPointerException if the given property UR, and/or value is <code>null</code>.
	*/
	public DefaultURFProperty(final URI propertyURI, final long value)
	{
		this(propertyURI, DEFAULT_URF_RESOURCE_FACTORY.createIntegerResource(value));	//construct the class with a resource created from the value
	}

	/**Property URI and URI value constructor with no subject scope, a default lock, and a default scope
	@param propertyURI The property URI.
	@param value The property value.
	@exception NullPointerException if the given property URI and/or value is <code>null</code>.
	*/
	public DefaultURFProperty(final URI propertyURI, final URI value)
	{
		this(propertyURI, DEFAULT_URF_RESOURCE_FACTORY.createURIResource(value));	//construct the class with a resource created from the value
	}

	/**Read write lock, property URI and value constructor with no subject scope and a default scope.
	@param readWriteLock The lock for controlling access to the properties.
	@param propertyURI The property URI.
	@param value The property value.
	@exception NullPointerException if the given lock, property URI, and/or value is <code>null</code>.
	*/
	public DefaultURFProperty(final ReadWriteLock readWriteLock, final URI propertyURI, final URFResource value)
	{
		this(readWriteLock, propertyURI, value, new DefaultURFScope(readWriteLock));	//construct the class with no subject scope
	}

	/**Property URI, value, and scope constructor with no subject scope and a default lock.
	@param propertyURI The property URI.
	@param value The property value.
	@param scope The property-value scope
	@exception NullPointerException if the given property URI, value, and/or scope is <code>null</code>.
	*/
	public DefaultURFProperty(final URI propertyURI, final URFResource value, final URFScope scope)
	{
		this(new ReentrantReadWriteLock(), propertyURI, value, scope);	//construct the class with a default lock
	}

	/**Read write lock, property URI, value, and scope constructor with no subject scope.
	@param readWriteLock The lock for controlling access to the properties.
	@param propertyURI The property URI.
	@param value The property value.
	@param scope The property-value scope
	@exception NullPointerException if the given subject scope, property URI, value, and/or scope is <code>null</code>.
	*/
	public DefaultURFProperty(final ReadWriteLock readWriteLock, final URI propertyURI, final URFResource value, final URFScope scope)
	{
		this(readWriteLock, null, propertyURI, value, scope);	//construct the class with no subject scope
	}

	/**Subject scope, property URI, value, and scope constructor.
	The subject scope lock will be used for controlling access to the value.
	@param subjectScope The scope to which the property belongs.
	@param propertyURI The property URI.
	@param value The property value.
	@param scope The property-value scope
	@exception NullPointerException if the given subject scope, property URI, value, and/or scope is <code>null</code>.
	*/
	public DefaultURFProperty(final URFScope subjectScope, final URI propertyURI, final URFResource value, final URFScope scope)
	{
		this(subjectScope, checkInstance(subjectScope, "Subject scope cannot be null."), propertyURI, value, scope);	//construct the class, using the subject scope for locking
	}

	/**Read write lock, Subject scope, property URI, value, and scope constructor.
	@param readWriteLock The lock for controlling access to the value.
	@param subjectScope The scope to which the property belongs, or <code>null</code> if this is a property definition not attached to any scope.
	@param propertyURI The property URI.
	@param value The property value.
	@param scope The property-value scope
	@exception NullPointerException if the given lock, property URI, value, and/or scope is <code>null</code>.
	*/
	protected DefaultURFProperty(final ReadWriteLock readWriteLock, final URFScope subjectScope, final URI propertyURI, final URFResource value, final URFScope scope)
	{
		super(readWriteLock, value, scope);	//construct the parent class
		this.subjectScope=subjectScope;
		this.propertyURI=checkInstance(propertyURI, "Property URI cannot be null.");
	}

	/**Determines a hash code for the object.
	This implementation returns the hash code for the property URI and the value.
	@return A hash code value for the object.
	*/
	public int hashCode()
	{
		return Objects.getHashCode(getPropertyURI(), getValue());	//calculate a hash code from the property URI and value
	}

	/**Determines if the given object is an equivalent URF property.
	Another URF property is considered equivalent to this one if it has equivalent values for {@link #getPropertyURI()} and {@link #getValue()}.
	@param object The object with which to compare this URF property.
	@return <code>true<code> if this URF property equals that specified in <code>object</code>.
	*/
	public boolean equals(final Object object)
	{
		if(object instanceof URFProperty)	//if we're being compared with another URF Property
		{
			final URFProperty urfProperty=(URFProperty)object;	//get the object as an URF property
			return getPropertyURI().equals(urfProperty.getPropertyURI())	//compare property URI and value
					&& getValue().equals(urfProperty.getValue());
		}
		else	//if we're being compared with anything else
		{
			return false;	//the objects aren't equal
		}
	}

	/**@return A string representation of the property.*/
	public String toString()
	{
		try
		{
			final StringBuilder stringBuilder=new StringBuilder();
			stringBuilder.append('(');
			URFTURFGenerator.appendReference(stringBuilder, getPropertyURI());
			stringBuilder.append(',').append(getValue());
			return stringBuilder.append(')').toString();
		}
		catch(final IOException ioException)
		{
			throw new AssertionError(ioException);
		}
	}
}
