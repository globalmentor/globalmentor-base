package com.garretwilson.urf;

import java.net.URI;

import com.garretwilson.net.Resource;

/**An URF resource.
@author Garret Wilson
*/
public interface URFResource extends Resource
{

	/**Retrieves the first value of the property with the given URI.
	All contextually ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order. 
	@param propertyURI The URI of the property for which a value should be returned.
	@return The first value of the property with the given URI, or <code>null</code> if there is no such property.
	*/
	public URFResource getPropertyValue(final URI propertyURI);

	/**Retrieves an iterable to the values of the property with the given URI.
	All contextually ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order. 
	@param propertyURI The URI of the property for which values should be returned.
	@return An iterable to all values of the property with the given URI.
	*/
	public Iterable<URFResource> getPropertyValues(final URI propertyURI);

	/**Adds a property value for the property with the given URI.
	If the given property and value already exists, no action occurs.
	@param propertyURI The URI of the property of the value to add.
	@param propertyValue The value to add for the given property.
	*/
	public void addPropertyValue(final URI propertyURI, final URFResource propertyValue);

	/**Sets a property value for the property with the given URI.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property.
	*/
	public void setPropertyValue(final URI propertyURI, final URFResource propertyValue);

	/**Adds a contextual property value for the contextual property with the given URI, in the context of a given property and value.
	If the given context property and value do not exists, no action occurs.
	If the given contextual property and value already exists, no action occurs.
	@param contextPropertyURI The URI of the context property of the value to add.
	@param contextPropertyValue The context value of the property value to add.
	@param contextualPropertyURI The URI of the property of the contextual value to add.
	@param contextualPropertyValue The value to add for the given contextual property.
	*/
	public void addContextPropertyValue(final URI contextPropertyURI, final URFResource contextPropertyValue, final URI contextualPropertyURI, final URFResource contextualPropertyValue);

	/**Sets a contextual property value for the contextual property with the given URI, in the context of a given property and value.
	If the given context property and value do not exists, no action occurs.
	@param contextPropertyURI The URI of the context property of the value to set.
	@param contextPropertyValue The context value of the property value to set.
	@param contextualPropertyURI The URI of the property of the contextual value to set.
	@param contextualPropertyValue The value to set for the given contextual property.
	*/
	public void setContextPropertyValue(final URI contextPropertyURI, final URFResource contextPropertyValue, final URI contextualPropertyURI, final URFResource contextualPropertyValue);

}
