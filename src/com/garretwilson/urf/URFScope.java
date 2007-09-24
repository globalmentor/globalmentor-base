package com.garretwilson.urf;

import java.net.URI;
import java.util.concurrent.locks.ReadWriteLock;

/**A scope of URF properties.
@author Garret Wilson
*/
public interface URFScope extends ReadWriteLock
{

	/**@return The order in which this scope was created; useful as a unique ID when sorting.*/
	public long getCreationOrder();

	/**@return The parent scope of this scope, or <code>null</code> if this scope has no parent scope.*/
	public URFScope getParentScope();

	/**Retrieves the scope of a given property and value of this scope.
	@param propertyURI The URI of the property the scope of which to retrieve.
	@param propertyValue The value of the property the scope of which to retrieve.
	@return The scope of the given property and value, or <code>null</code> if no such property and value exists.
	*/
	public URFScope getScope(final URI propertyURI, final URFResource propertyValue);

	/**@return Whether this scope has properties.*/
	public boolean hasProperties();

	/**@return The number of properties this scope has.*/
	public long getPropertyCount();

	/**Returns an iterable to the properties of this scope.
	@return An iterable to all available properties.
	*/
	public Iterable<URFProperty> getProperties();

	/**Returns an iterable to the properties of this scope within a particular namespace.
	@param namespaceURI The URI of the namespace of the properties to be returned.
	@return An iterable to all available properties.
	@exception NullPointerException if the given namespace URI is <code>null</code>.
	*/
	public Iterable<URFProperty> getNamespaceProperties(final URI namespaceURI);

	/**Retrieves an iterable to all property URIs.
	Any deletions made to the returned iterable will result in all corresponding properties being removed.
	@return An iterable to all property URIs.
	*/
	public Iterable<URI> getPropertyURIs();

	/**Retrieves the first value of the property with the given URI.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order. 
	@param propertyURI The URI of the property for which a value should be returned.
	@return The first value of the property with the given URI, or <code>null</code> if there is no such property.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource getPropertyValue(final URI propertyURI);

	/**Retrieves an iterable to the values of the property with the given URI.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order. 
	@param propertyURI The URI of the property for which values should be returned.
	@return An iterable to all values of the property with the given URI.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public Iterable<URFResource> getPropertyValues(final URI propertyURI);

	/**Determines whether there exists a property with the given property URI.
	@param propertyURI The URI of the property to check.
	@return <code>true</code> if a property exists with the given property URI.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public boolean hasProperty(final URI propertyURI);

	/**Determines whether there exists a property with the given property URI and the given property value.
	@param propertyURI The URI of the property of the value to check.
	@param propertyValue The value to match for the given property.
	@return <code>true</code> if a property exists with the given property URI and property value.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public boolean hasPropertyValue(final URI propertyURI, final URFResource propertyValue);

	/**Determines whether there exists a property with the given property URI and the given property value URI.
	@param propertyURI The URI of the property of the value to check.
	@param propertyValueURI The value URI to match for the given property.
	@return <code>true</code> if a property exists with the given property URI and property value URI.
	@exception NullPointerException if the given property URI and/or property value URI is <code>null</code>.
	*/
	public boolean hasPropertyValueURI(final URI propertyURI, final URI propertyValueURI);

	/**Adds a property value for the property with the given URI.
	If the given property and value already exists, no action occurs.
	@param propertyURI The URI of the property of the value to add.
	@param propertyValue The value to add for the given property.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public void addPropertyValue(final URI propertyURI, final URFResource propertyValue);

	/**Sets a property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property, or <code>null</code> if there should be no such property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final URFResource propertyValue);

	/**Sets a string property value for the property with the given URI.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final String propertyValue);

	/**Removes all properties of this scope.
	@return The number of properties removed.
	*/
	public long removeProperties();

	/**Removes all properties of this scope within a particular namespace.
	@param namespaceURI The URI of the namespace of the properties to be removed.
	@return The number of properties removed.
	@exception NullPointerException if the given namespace URI is <code>null</code>.
	*/
	public long removeNamespaceProperties(final URI namespaceURI);

	/**Retrieves the order of this scope.
	The order is considered to be the number value of the property with URI {@value URF#ORDER_PROPERTY_URI}.
	@return The order of this scope, or <code>null</code> if no order is indicated or the order is not a number.
	*/
	public Number getOrder();
}
