package com.garretwilson.urf;

import java.net.URI;
import java.util.concurrent.locks.ReadWriteLock;

/**An URF value with its scope.
@author Garret Wilson
*/
public interface URFValueContext extends ReadWriteLock
{

	/**@return The value resource.*/
	public URFResource getValue();

	/**The scope of the value in the context of some property.*/ 
	public URFScope getScope();

	/**Determines whether there exists a property with the given property URI in this context,
	either as a property of the context value or as a property of the context scope.
	@param propertyURI The URI of the property to check.
	@return <code>true</code> if a property exists with the given property URI.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public boolean hasProperty(final URI propertyURI);

	/**Retrieves the first value context of the property with the given URI in this context,
	either as a property of the context value or as a property of the context scope.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order. 
	@param propertyURI The URI of the property for which a value context should be returned.
	@return The first value context of the property with the given URI, or <code>null</code> if there is no such property.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFValueContext getPropertyValueContext(final URI propertyURI);

	/**Retrieves the first value of the property with the given URI in this context,
	either as a property of the context value or as a property of the context scope.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order. 
	@param propertyURI The URI of the property for which a value should be returned.
	@return The first value of the property with the given URI, or <code>null</code> if there is no such property.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource getPropertyValue(final URI propertyURI);

}
