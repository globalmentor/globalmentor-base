package com.garretwilson.urf;

import java.net.URI;
import java.util.concurrent.locks.*;

import com.garretwilson.lang.Objects;
import static com.garretwilson.lang.Objects.*;
import static com.garretwilson.urf.URF.DEFAULT_URF_RESOURCE_FACTORY;

/**Default implementation of an encapsulation of a parent scope, property URI, value, and the associated property-value scope.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
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
	@exception NullPointerException if the given property URI, and/or value is <code>null</code>.
	*/
	public DefaultURFProperty(final URI propertyURI, final URFResource value)
	{
		this(new ReentrantReadWriteLock(), propertyURI, value);	//construct the class with a default lock
	}

	/**Property URI and string value constructor with no subject scope, a default lock, and a default scope
	@param propertyURI The property URI.
	@param value The property value.
	@exception NullPointerException if the given property URI, and/or value is <code>null</code>.
	*/
	public DefaultURFProperty(final URI propertyURI, final String value)
	{
		this(propertyURI, DEFAULT_URF_RESOURCE_FACTORY.createStringResource(value));	//construct the class with a resource created from the value
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

	/**@return A hash code value for the object.*/
	public int hashCode()
	{
		return Objects.hashCode(getSubjectScope(), getPropertyURI(), getValue(), getScope());	//calculate a hash code from the resource, property URI, value, and scope
	}

	/**Determines if the given object is URF property with the same property URI, value, and scope.
	@param object The object with which to compare this URF property.
	@return <code>true<code> if this URF property equals that specified in <code>object</code>.
	*/
	public boolean equals(final Object object)
	{
		if(object instanceof URFProperty)	//if we're being compared with another URF Property
		{
			final URFProperty urfProperty=(URFProperty)object;	//get the object as an URF property
			return getSubjectScope()==urfProperty.getSubjectScope()	//compare subject scopes by identity
					&& getPropertyURI().equals(urfProperty.getPropertyURI())	//compare property URIs by equivalence
					&& getValue().equals(urfProperty.getValue())	//compare values by equivalence
					&& getScope()==urfProperty.getScope();	//compare property-value scopes by identity
		}
		else	//if we're being compared with anything else
		{
			return false;	//the objects aren't equal
		}
	}
}
