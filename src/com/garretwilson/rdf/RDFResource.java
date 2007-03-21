package com.garretwilson.rdf;

import java.net.URI;
import java.util.*;

import com.garretwilson.net.Resource;

/**Represents the an RDF resource connected in an RDF graph.
@author Garret Wilson
@see DefaultRDFResource
*/
public interface RDFResource extends RDFObject, Resource, Cloneable	//TODO fix, Comparable<RDFResource>
{

	/**@return The resource identifier URI, or <code>null</code> if the identifier is not known.*/
//G***del; this is already in Resource	public URI getReferenceURI();

	/**Sets the reference URI of the resource.
	@param uri The new reference URI, or <code>null</code> if the identifier is not known.
	*/
	public void setReferenceURI(final URI uri);

	/**@return The RDF data model with which this resource is associated, or
		<code>null</code> if this resource is not associated with a data model.
	*/
	public RDF getRDF();

	/**Associates this resource with an RDF data model.
	@param rdf The RDF data model with which to associate this resource, or
		<code>null</code> if this resource should not be associated with any
		RDF data model.
	*/
	public void setRDF(final RDF rdf);

	/**@return The number of properties this resource has.*/
	public int getPropertyCount();

	/**@return Iterable access to all properties, each of which is a {@link RDFPropertyValuePair}, with the name being the property predicate and the value being the property value.*/
	public Iterable<RDFPropertyValuePair> getProperties();
	
	/**@return An iterator that allows traversal of all properties, each of which
		is a <code>RDFPropertyValuePair</code>, with the name being the property predicate
		and the value being the property value.
	*/
	public ListIterator<RDFPropertyValuePair> getPropertyIterator();

	/**Searches and returns the first occurring property value that appears as
		an RDF statement object with a predicate of <code>propertyResource</code>.
	@param propertyResource The property resource.
	@return The value of the property, either a <code>RDFResource</code> or a
		<code>RDFLiteral</code>, or <code>null</code> if this resource has no such
		property.
	*/
	public RDFObject getPropertyValue(final RDFResource propertyResource);

	/**Searches and returns the first occurring property value that appears as
		an RDF statement object with a predicate of <code>propertyURI</code>.
	@param propertyURI The reference URI of the property resource.
	@return The value of the property, either a <code>RDFResource</code> or a
		<code>RDFLiteral</code>, or <code>null</code> if this resource has no such
		property.
	*/
	public RDFObject getPropertyValue(final URI propertyURI);

	/**Searches and returns the first occurring property value that appears as
		an RDF statement object with a predicate of a property URI formed by the
		given namespace URI and local name.
	@param namespaceURI The XML namespace URI used in the serialization.
	@param localName The XML local name used in the serialization.
	@return The value of the property, either a <code>RDFResource</code> or a
		<code>RDFLiteral</code>, or <code>null</code> if this resource has no such
		property.
	*/
	public RDFObject getPropertyValue(final URI namespaceURI, final String localName);

	/**Searches and returns an iterable of all property values that appear as RDF
		statement objects with a predicate of <var>propertyURI</var>.
	@param propertyURI The reference URI of the property resources.
	@return A read-only iterable of values of properties.
	*/
	public Iterable<RDFObject> getPropertyValues(final URI propertyURI);

	/**Searches and returns an iterable of all property values of the requested type that appear as RDF
		statement objects with a predicate of <var>propertyURI</var>.
	@param propertyURI The reference URI of the property resources.
	@param valueType The type of values to include
	@return A read-only iterable of values of properties.
	*/
	public <T> Iterable<T> getPropertyValues(final URI propertyURI, final Class<T> valueType);

	/**Searches and returns an iterable of all property values that appear as
		RDF statement objects with a predicate of a property URI formed by the
		given namespace URI and local name.
	@param namespaceURI The XML namespace URI that represents part of the reference URI.
	@param localName The XML local name that represents part of the reference URI.
	@return A read-only iterable of values of properties.
	*/
	public Iterable<RDFObject> getPropertyValues(final URI namespaceURI, final String localName);

	/**Searches and returns an iterable of all property values of the requested type that appear as
		RDF statement objects with a predicate of a property URI formed by the
		given namespace URI and local name.
	@param namespaceURI The XML namespace URI that represents part of the reference URI.
	@param localName The XML local name that represents part of the reference URI.
	@param valueType The type of values to include
	@return A read-only iterable of values of properties.
	*/
	public <T> Iterable<T> getPropertyValues(final URI namespaceURI, final String localName, final Class<T> valueType);

	/**Determines if the resource has the given property with the resource
		identified by the given URI.
	@param propertyURI The reference URI of the property resource.
	@param propertyValueURI The URI of the resource to which the property should
		be compared.
	@return <code>true</code> if the specified property is set to the specified
		value.
	*/
	public boolean hasPropertyResourceValue(final URI propertyURI, final URI propertyValueURI);

	/**Determines if the resource has the given property with the resource
		identified by the given URI.
	@param namespaceURI The XML namespace URI that represents part of the reference URI.
	@param localName The XML local name that represents part of the reference URI.
	@param propertyValueURI The URI of the resource to which the property should
		be compared.
	@return <code>true</code> if the specified property is set to the specified
		value.
	*/
	public boolean hasPropertyResourceValue(final URI namespaceURI, final String localName, final URI propertyValueURI);

	/**Adds a property by creating a {@link RDFPropertyValuePair} from the given property and value.
	If an equivalent property already exists, no action is taken.
	@param property A property resource; the predicate of an RDF statement.
	@param value A property value; the object of an RDF statement.
	@return The added property value.
	*/
	public <T extends RDFObject> T addProperty(final RDFResource property, final T value);

	/**Adds a property by creating a {@link RDFPropertyValuePair} from the given property URI and value.
	If an equivalent property already exists, no action is taken.
	@param propertyURI The reference URI of a property resource; the predicate of an RDF statement.
	@param value A property value; the object of an RDF statement.
	@return The added property value.
	*/
	public <T extends RDFObject> T addProperty(final URI propertyURI, final T value);

	/**Adds a property by creating a {@link RDFPropertyValuePair} from the given property URI and value.
	If an equivalent property already exists, no action is taken.
	@param propertyNamespaceURI The namespace URI of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The local name of the property resource that is the predicate of an RDF statement.
	@param value A property value; the object of an RDF statement.
	@return The added property value.
	*/
	public <T extends RDFObject> T addProperty(final URI propertyNamespaceURI, final String propertyLocalName, final T value);

	/**Adds a resource property value indicated by its reference URI.
	If an equivalent property already exists, no action is taken.
	@param propertyURI The reference URI of a property resource; the predicate of an RDF statement.
	@param valueURI The reference URI of the resource value; the object of an RDF statement.
	@return The added property value.
	*/
	public RDFResource addProperty(final URI propertyURI, final URI valueURI);

	/**Adds a resource property value indicated by its reference URI.
	If an equivalent property already exists, no action is taken.
	@param propertyNamespaceURI The namespace URI of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The local name of the property resource that is the predicate of an RDF statement.
	@param valueURI The reference URI of the resource value; the object of an RDF statement.
	@return The added property value.
	*/
	public RDFResource addProperty(final URI propertyNamespaceURI, final String propertyLocalName, final URI valueURI);

	/**Adds a plain literal property from a string by creating a {@link RDFPropertyValuePair} from the given property URI and value.
	No language is specified.
	If an equivalent property already exists, no action is taken.
	@param propertyURI The reference URI of a property resource; the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a {@link RDFPlainLiteral}; the object of an RDF statement.
	@return The added property value.
	*/
	public RDFPlainLiteral addProperty(final URI propertyURI, final String literalValue);

	/**Adds a plain literal property from a string by creating a {@link RDFPropertyValuePair} from the given property URI and value.
	No language is specified.
	If an equivalent property already exists, no action is taken.
	@param propertyNamespaceURI The namespace URI of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The local name of the property resource that is the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a {@link RDFPlainLiteral}; the object of an RDF statement.
	@return The added property value.
	*/
	public RDFPlainLiteral addProperty(final URI propertyNamespaceURI, final String propertyLocalName, final String literalValue);

	/**Adds a plain literal property from a string by creating a {@link RDFPropertyValuePair} from the given property URI and value with specified language.
	If an equivalent property already exists, no action is taken.
	@param propertyURI The reference URI of a property resource; the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a {@link RDFPlainLiteral}; the object of an RDF statement.
	@param language The language of the plain literal, or <code>null</code> if no language should be specified.
	@return The added property value.
	*/
	public RDFPlainLiteral addProperty(final URI propertyURI, final String literalValue, final Locale language);

	/**Adds a plain literal property from a string by creating a {@link RDFPropertyValuePair} from the given property URI and value.
	If an equivalent property already exists, no action is taken.
	@param propertyNamespaceURI The namespace URI of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The local name of the property resource that is the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a {@link RDFPlainLiteral}; the object of an RDF statement.
	@param language The language of the plain literal, or <code>null</code> if no language should be specified.
	@return The added property value.
	*/
	public RDFPlainLiteral addProperty(final URI propertyNamespaceURI, final String propertyLocalName, final String literalValue, final Locale language);

	/**Removes the property with the given property URI and property value.
	@param propertyURI The reference URI of the property resource of the property to be removed.
	@param propertyvalue The value of the property to be removed.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public void removeProperty(final URI propertyURI, final RDFObject propertyValue);
	
	/**Removes the property with the given property URI and property value.
	@param namespaceURI The XML namespace URI that represents part of the reference URI of the property to be removed.
	@param localName The XML local name that represents part of the reference URI of the property to be removed.
	@param propertyValue The value of the property to be removed.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public void removeProperty(final URI namespaceURI, final String localName, final RDFObject propertyValue);

	/**Removes all properties with the given URI.
	@param propertyURI The reference URI of the property resource of the
		properties to be removed.
	@return The number of properties removed.
	*/
	public int removeProperties(final URI propertyURI);
	
	/**Removes all properties with the given URI.
	@param namespaceURI The XML namespace URI that represents part of the
		reference URI of the properties to be removed.
	@param localName The XML local name that represents part of the reference URI
		of the properties to be removed.
	@return The number of properties removed.
	*/
	public int removeProperties(final URI namespaceURI, final String localName);
	
	/**Sets a property by removing all property values for the given property and creating a new {@link RDFPropertyValuePair} from the given property URI and value.
	If no value is given, all such properties are removed.
	@param propertyURI The reference URI of a property resource; the predicate of an RDF statement.
	@param value A property value&mdash;the object of an RDF statement&mdash;or <code>null</code> if all such properties should be removed with nothing to replace them.
	@return The added property value, or <code>null</code> if no property was added.
	@see #removeProperties(URI)
	@see #addProperty(URI, RDFObject)
	*/
	public <T extends RDFObject> T setProperty(final URI propertyURI, final T value);

	/**Sets a property by first removing all such properties and then adding a new property.
	If no value is given, all such properties are removed.
	@param propertyNamespaceURI The namespace URI of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The local name of the property resource that is the predicate of an RDF statement.
	@param value A property value&mdash;the object of an RDF statement&mdash;or <code>null</code> if all such properties should be removed with nothing to replace them.
	@return The added property value.
	*/
	public <T extends RDFObject> T setProperty(final URI propertyNamespaceURI, final String propertyLocalName, final T value);

	/**Sets a resource property value indicated by its reference URI by first removing all such properties and then adding a new property..
	If no value is given, all such properties are removed.
	@param propertyURI The reference URI of a property resource; the predicate of an RDF statement.
	@param valueURI The reference URI of the resource value; the object of an RDF statement.
	@return The added property value.
	*/
	public RDFResource setProperty(final URI propertyURI, final URI valueURI);

	/**Sets a resource property value indicated by its reference URI by first removing all such properties and then adding a new property..
	If no value is given, all such properties are removed.
	@param propertyNamespaceURI The namespace URI of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The local name of the property resource that is the predicate of an RDF statement.
	@param valueURI The reference URI of the resource value; the object of an RDF statement.
	@return The added property value.
	*/
	public RDFResource setProperty(final URI propertyNamespaceURI, final String propertyLocalName, final URI valueURI);

	/**Sets a plain literal property from a string by removing all property values for the given property and creating a new {@link RDFPropertyValuePair} from the given property URI and value.
	No language is specified.
	If no value is given, all such properties are removed.
	@param propertyURI The reference URI of a property resource; the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a {@link RDFPlainLiteral}; the object of an RDF statement.
	@return The added property value.
	@see #removeProperties(URI)
	@see #addProperty(URI, String)
	*/
	public RDFPlainLiteral setProperty(final URI propertyURI, final String literalValue);

	/**Sets a plain literal property from a string by first removing all such properties and then adding a new property.
	No language is specified.
	If no value is given, all such properties are removed.
	@param propertyNamespaceURI The namespace URI of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The local name of the property resource that is the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a {@link RDFPlainLiteral}; the object of an RDF statement.
	@return The added property value.
	*/
	public RDFPlainLiteral setProperty(final URI propertyNamespaceURI, final String propertyLocalName, final String literalValue);

	/**Sets a plain literal property from a string by removing all property values for the given property and creating a new {@link RDFPropertyValuePair} from the given property URI and value.
	If no value is given, all such properties are removed.
	@param propertyURI The reference URI of a property resource; the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a {@link RDFPlainLiteral}; the object of an RDF statement.
	@param language The language of the plain literal, or <code>null</code> if no language should be specified.
	@return The added property value.
	@see #removeProperties(URI)
	@see #addProperty(URI, String)
	*/
	public RDFPlainLiteral setProperty(final URI propertyURI, final String literalValue, final Locale language);

	/**Sets a plain literal property from a string by first removing all such properties and then adding a new property.
	If no value is given, all such properties are removed.
	@param propertyNamespaceURI The namespace URI of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The local name of the property resource that is the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a {@link RDFPlainLiteral}; the object of an RDF statement.
	@param language The language of the plain literal, or <code>null</code> if no language should be specified.
	@return The added property value.
	*/
	public RDFPlainLiteral setProperty(final URI propertyNamespaceURI, final String propertyLocalName, final String literalValue, final Locale language);

	/**@return A copy of this resource with the same URI and identical properties.*/
	public Object clone();

}