package com.garretwilson.urf;

import java.net.URI;
import java.util.concurrent.locks.ReadWriteLock;

import com.garretwilson.lang.ObjectUtilities;
import static com.garretwilson.lang.ObjectUtilities.*;

/**Default implementation of an encapsulation of a parent scope, property URI, value, and the associated property-value scope.
@author Garret Wilson
*/
public class DefaultURFProperty extends DefaultURFValueContext implements URFProperty
{

	/**The scope to which the property belongs.*/
	private final URFScope subjectScope;

		/**@return The scope to which the property belongs.*/
		public URFScope getSubjectScope() {return subjectScope;}

	/**The URI of the property.*/
	private final URI propertyURI;

		/**@return The URI of the property.*/
		public URI getPropertyURI() {return propertyURI;}

	/**Subject scope, property URI, value, and scope constructor.
	@param readWriteLock The lock for controlling access to the properties.
	@param subjectScope The scope to which the property belongs.
	@param propertyURI The property URI.
	@param value The property value.
	@param scope The property-value scope
	@exception NullPointerException if the given subject scope, property URI, value, and/or scope is <code>null</code>.
	*/
	public DefaultURFProperty(final ReadWriteLock readWriteLock, final URFScope subjectScope, final URI propertyURI, final URFResource value, final URFScope scope)
	{
		super(readWriteLock, value, scope);	//construct the parent class
		this.subjectScope=checkInstance(subjectScope, "Subject scope cannot be null.");
		this.propertyURI=checkInstance(propertyURI, "Property URI cannot be null.");
	}

	/**@return A hash code value for the object.*/
	public int hashCode()
	{
		return ObjectUtilities.hashCode(getSubjectScope(), getPropertyURI(), getValue(), getScope());	//calculate a hash code from the resource, property URI, value, and scope
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
