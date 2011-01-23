package com.globalmentor.urf;

import java.net.URI;
import java.util.concurrent.locks.ReadWriteLock;

/**A scope of URF properties.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
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

	/**Determines the number of distinct properties that have at least one value.
	@return The number of propertis this scope has.
	*/
	public long getPropertyCount();

	/**@return The number of property values this scope has.*/
	public long getPropertyValueCount();

	/**Determines whether there exists a property with the given property URI.
	@param propertyURI The URI of the property to check.
	@return <code>true</code> if a property exists with the given property URI.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public boolean hasProperty(final URI propertyURI);

	/**Determines whether there exists a property within the given namespace.
	@param namespaceURI The URI of the namespace of the property to check.
	@return <code>true</code> if a property exists in the given namespace.
	@exception NullPointerException if the given namespace URI is <code>null</code>.
	*/
	public boolean hasNamespaceProperty(final URI namespaceURI);

	/**Determines the number of values there exists for a property with the given property URI.
	@param propertyURI The URI of the property to count.
	@return The number of values of the property with the given property URI.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public long getPropertyValueCount(final URI propertyURI);

	/**Determines whether there exists a property with the given property and value.
	This is a convenient method that is equivalent to {@link #hasPropertyValue(URI, URFResource)}.
	@param property The property and value to check.
	@return <code>true</code> if a property exists with the given property and value.
	@exception NullPointerException if the given property is <code>null</code>.
	*/
	public boolean hasProperty(final URFProperty property);

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

	/**Returns an iterable to the properties of this scope.
	@return An iterable to all available properties.
	*/
	public Iterable<URFProperty> getProperties();

	/**Returns an iterable to the properties of this scope with the given property URI.
	@param propertyURI The URI of the properties to be returned.
	@return An iterable to all available properties with the given property URI.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public Iterable<URFProperty> getProperties(final URI propertyURI);

	/**Returns an iterable to the properties of this scope within a particular namespace.
	@param namespaceURI The URI of the namespace of the properties to be returned.
	@return An iterable to all available properties within the given namespace.
	@exception NullPointerException if the given namespace URI is <code>null</code>.
	*/
	public Iterable<URFProperty> getNamespaceProperties(final URI namespaceURI);

	/**Retrieves an iterable to all property URIs.
	Any deletions made to the returned iterable will result in all corresponding properties being removed.
	@return An iterable to all property URIs.
	*/
	public Iterable<URI> getPropertyURIs();

	/**Retrieves the value context of the property with the given preoprty URI and the given property value.
	@param propertyURI The URI of the property for which a value context should be returned.
	@param propertyValue The value of the property for which a value context should be returned.
	@return The value context of the property with the given URI and value, or <code>null</code> if there is no such property with the given value.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public URFValueContext getPropertyValueContext(final URI propertyURI, final URFResource propertyValue);

	/**Retrieves the first value context of the property with the given URI.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order.
	@param propertyURI The URI of the property for which a value context should be returned.
	@return The first value context of the property with the given URI, or <code>null</code> if there is no such property.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFValueContext getPropertyValueContext(final URI propertyURI);

	/**Retrieves an iterable to the value contexts of the property with the given URI.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order.
	@param propertyURI The URI of the property for which value contexts should be returned.
	@return An iterable to all value contexts of the property with the given URI.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public Iterable<URFValueContext> getPropertyValueContexts(final URI propertyURI);

	/**Retrieves the first value of the property with the given URI.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order.
	@param propertyURI The URI of the property for which a value should be returned.
	@return The first value of the property with the given URI, or <code>null</code> if there is no such property.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource getPropertyValue(final URI propertyURI);

	/**Retrieves the URI of the first value of the property with the given URI.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order.
	@param propertyURI The URI of the property for which the URI of a value should be returned.
	@return The URI of the first value of the property with the given URI, or <code>null</code> if there is no such property or the first property value has no URI.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URI getPropertyValueURI(final URI propertyURI);

	/**Retrieves an iterable to the values of the property with the given URI.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order.
	@param propertyURI The URI of the property for which values should be returned.
	@return A live iterable to all values of the property with the given URI.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public Iterable<URFResource> getPropertyValues(final URI propertyURI);

	/**Retrieves an iterable to the values of a given type of the property with the given URI.
	All values of the property with the given URI that are not of the specified type will be ignored.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order.
	@param <V> The type of values to be returned.
	@param propertyURI The URI of the property for which values should be returned.
	@param valueClass The class indicating the type of values to be returned in the iterator.
	@return A live iterable to all values of the given type of the property with the given URI.
	@exception NullPointerException if the given property URI and/or value class is <code>null</code>.
	*/
	public <V extends URFResource> Iterable<V> getPropertyValues(final URI propertyURI, final Class<V> valueClass);

	/**Adds a property and its scoped properties recursively.
	If the given property and value already exists, the scoped properties, if any, will still be added recursively if they don't exist.
	@param scope The scope containing the properties to add.
	@return <code>true</code> if one or more properties was added, else <code>false</code> if all given property URI and value pairs already existed.
	@exception NullPointerException if the given scope is <code>null</code>.
	*/
	public boolean addAllProperties(final URFScope scope);

	/**Adds a property and its scoped properties recursively.
	If the given property and value already exists, the scoped properties, if any, will still be added recursively if they don't exist.
	@param property The property to add.
	@return <code>true</code> if the property was added, else <code>false</code> if the property URI and value already existed.
	@exception NullPointerException if the given property is <code>null</code>.
	*/
	public boolean addProperty(final URFProperty property);

	/**Adds a property value for the property with the given URI.
	If the given property and value already exists, no action occurs.
	@param propertyURI The URI of the property of the value to add.
	@param propertyValue The value to add for the given property.
	@return <code>true</code> if the value was added for the indicated property, or <code>false</code> if the property and value already existed.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public boolean addPropertyValue(final URI propertyURI, final URFResource propertyValue);

	/**Adds a date time property value for the property with the given URI.
	If the given property and value already exists, no action occurs.
	@param propertyURI The URI of the property of the value to add.
	@param propertyValue The value to add for the given property.
	@return <code>true</code> if the value was added for the indicated property, else <code>false</code> if the property and value already existed.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public boolean addPropertyValue(final URI propertyURI, final URFDateTime propertyValue);

	/**Adds a number property value for the property with the given URI.
	If the given property and value already exists, no action occurs.
	@param propertyURI The URI of the property of the value to add.
	@param propertyValue The value to add for the given property.
	@return <code>true</code> if the value was added for the indicated property, else <code>false</code> if the property and value already existed.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public boolean addPropertyValue(final URI propertyURI, final Number propertyValue);

	/**Adds an integer property value for the property with the given URI.
	If the given property and value already exists, no action occurs.
	@param propertyURI The URI of the property of the value to add.
	@param propertyValue The value to add for the given property.
	@return <code>true</code> if the value was added for the indicated property, else <code>false</code> if the property and value already existed.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public boolean addPropertyValue(final URI propertyURI, final long propertyValue);

	/**Adds a rational property value for the property with the given URI.
	If the given property and value already exists, no action occurs.
	@param propertyURI The URI of the property of the value to add.
	@param propertyValue The value to add for the given property.
	@return <code>true</code> if the value was added for the indicated property, else <code>false</code> if the property and value already existed.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public boolean addPropertyValue(final URI propertyURI, final double propertyValue);

	/**Adds a string property value for the property with the given URI.
	If the given property and value already exists, no action occurs.
	@param propertyURI The URI of the property of the value to add.
	@param propertyValue The value to add for the given property.
	@return <code>true</code> if the value was added for the indicated property, else <code>false</code> if the property and value already existed.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public boolean addPropertyValue(final URI propertyURI, final String propertyValue);

	/**Adds a URI property value for the property with the given URI.
	If the given property and value already exists, no action occurs.
	@param propertyURI The URI of the property of the value to add.
	@param propertyValue The value to add for the given property.
	@return <code>true</code> if the value was added for the indicated property, else <code>false</code> if the property and value already existed.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public boolean addPropertyValue(final URI propertyURI, final URI propertyValue);

	/**Sets a value and its scoped properties recursively by removing all properties with the URI of the given property and adding the given property value and scoped properties recursively.
	@param property The property to set.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property is <code>null</code>.
	*/
	public URFResource setProperty(final URFProperty property);

	/**Sets a date property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property, or <code>null</code> if there should be no such property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final URFDate propertyValue);

	/**Sets a date time property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property, or <code>null</code> if there should be no such property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final URFDateTime propertyValue);

	/**Sets a number property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property, or <code>null</code> if there should be no such property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final Number propertyValue);

	/**Sets an boolean property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final boolean propertyValue);

	/**Sets an integer property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final long propertyValue);

	/**Sets a rational property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final double propertyValue);

	/**Sets values for the property with the given URI by removing all properties with the given URI and adding the given property values.
	Duplicate property values are ignored.
	@param propertyURI The URI of the property of the value to set.
	@param ordered Whether each added property value should be given a contextual order.
	@param propertyValues The values to set for the given property.
	@return The old property values.
	@exception NullPointerException if the given property URI and/or property values is <code>null</code>.
	*/
	public URFResource[] setPropertyValues(final URI propertyURI, final URFResource... propertyValues);

	/**Sets integer values for the property with the given URI by removing all properties with the given URI and adding the given property values.
	Duplicate property values are ignored.
	@param propertyURI The URI of the property of the value to set.
	@param ordered Whether each added property value should be given a contextual order.
	@param propertyValues The values to set for the given property.
	@return The old property values.
	@exception NullPointerException if the given property URI and/or property values is <code>null</code>.
	*/
	public URFResource[] setPropertyValues(final URI propertyURI, final long... propertyValues);

	/**Sets rational values for the property with the given URI by removing all properties with the given URI and adding the given property values.
	Duplicate property values are ignored.
	@param propertyURI The URI of the property of the value to set.
	@param ordered Whether each added property value should be given a contextual order.
	@param propertyValues The values to set for the given property.
	@return The old property values.
	@exception NullPointerException if the given property URI and/or property values is <code>null</code>.
	*/
	public URFResource[] setPropertyValues(final URI propertyURI, final double... propertyValues);

	/**Sets string values for the property with the given URI by removing all properties with the given URI and adding the given property values.
	Duplicate property values are ignored.
	@param propertyURI The URI of the property of the value to set.
	@param ordered Whether each added property value should be given a contextual order.
	@param propertyValues The values to set for the given property.
	@return The old property values.
	@exception NullPointerException if the given property URI and/or property values is <code>null</code>.
	*/
	public URFResource[] setPropertyValues(final URI propertyURI, final String... propertyValues);

	/**Sets URI values for the property with the given URI by removing all properties with the given URI and adding the given property values.
	Duplicate property values are ignored.
	@param propertyURI The URI of the property of the value to set.
	@param ordered Whether each added property value should be given a contextual order.
	@param propertyValues The values to set for the given property.
	@return The old property values.
	@exception NullPointerException if the given property URI and/or property values is <code>null</code>.
	*/
	public URFResource[] setPropertyValues(final URI propertyURI, final URI... propertyValues);

	/**Sets ordered values for the property with the given URI by removing all properties with the given URI and adding the given property values.
	Duplicate property values are ignored.
	@param propertyURI The URI of the property of the value to set.
	@param ordered Whether each added property value should be given a contextual order.
	@param propertyValues The values to set for the given property.
	@return The old property values.
	@exception NullPointerException if the given property URI and/or property values is <code>null</code>.
	*/
	public URFResource[] setOrderedPropertyValues(final URI propertyURI, final URFResource... propertyValues);

	/**Sets ordered integer values for the property with the given URI by removing all properties with the given URI and adding the given property values.
	Duplicate property values are ignored.
	@param propertyURI The URI of the property of the value to set.
	@param ordered Whether each added property value should be given a contextual order.
	@param propertyValues The values to set for the given property.
	@return The old property values.
	@exception NullPointerException if the given property URI and/or property values is <code>null</code>.
	*/
	public URFResource[] setOrderedPropertyValues(final URI propertyURI, final long... propertyValues);

	/**Sets ordered string values for the property with the given URI by removing all properties with the given URI and adding the given property values.
	Duplicate property values are ignored.
	@param propertyURI The URI of the property of the value to set.
	@param ordered Whether each added property value should be given a contextual order.
	@param propertyValues The values to set for the given property.
	@return The old property values.
	@exception NullPointerException if the given property URI and/or property values is <code>null</code>.
	*/
	public URFResource[] setOrderedPropertyValues(final URI propertyURI, final String... propertyValues);

	/**Sets ordered URI values for the property with the given URI by removing all properties with the given URI and adding the given property values.
	Duplicate property values are ignored.
	@param propertyURI The URI of the property of the value to set.
	@param ordered Whether each added property value should be given a contextual order.
	@param propertyValues The values to set for the given property.
	@return The old property values.
	@exception NullPointerException if the given property URI and/or property values is <code>null</code>.
	*/
	public URFResource[] setOrderedPropertyValues(final URI propertyURI, final URI... propertyValues);

	/**Sets a property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property, or <code>null</code> if there should be no such property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final URFResource propertyValue);

	/**Sets a URI property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property, or <code>null</code> if there should be no such property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final URI propertyValue);

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

	/**Removes all property values for a given property URI.
	@param propertyURI The URI of the property the values of which to remove.
	@return The number of property values removed.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public long removePropertyValues(final URI propertyURI);

	/**Removes all properties of this scope within a particular namespace.
	@param namespaceURI The URI of the namespace of the properties to be removed.
	@return The number of property values removed.
	@exception NullPointerException if the given namespace URI is <code>null</code>.
	*/
	public long removeNamespacePropertyValues(final URI namespaceURI);

	/**Removes a given property value for the indicated property and value.
	If the given property and value do not exist, no action occurs.
	This is a convenient method that is equivalent to {@link #removePropertyValue(URI, URFResource)}.
	@param property The property and value to remove.
	@return <code>true</code> if the value was removed from the indicated property, else <code>false</code> if the property and value did not exist.
	@exception NullPointerException if the given property is <code>null</code>.
	*/
	public boolean removeProperty(final URFProperty property);

	/**Removes a given property value for the property with the given URI.
	If the given property and value do not exist, no action occurs.
	@param propertyURI The URI of the property of the value to remove.
	@param propertyValue The value to remove for the given property.
	@return <code>true</code> if the value was removed from the indicated property, else <code>false</code> if the property and value did not exist.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public boolean removePropertyValue(final URI propertyURI, final URFResource propertyValue);

	/**Retrieves the order of this scope.
	The order is considered to be the number value of the property with URI {@value URF#ORDER_PROPERTY_URI}.
	@return The order of this scope, or <code>null</code> if no order is indicated or the order is not a number.
	*/
	public Number getOrder();
}
