package com.garretwilson.rdf;

import java.net.URI;
import java.util.*;

import com.garretwilson.model.*;

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

	/**@return The XML namespace URI used in serialization, or <code>null</code>
		if no namespace URI was used or there was no namespace.*/
	public URI getNamespaceURI();

	/**@return The XML local name used in serialization, or <code>null</code>
		if no namespace URI and local name was used.*/
	public String getLocalName();

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
		given namespace URI and local name. This is a convenience function that
		creates a property URI from an XML qualified name automatically for searching.
	@param namespaceURI The XML namespace URI used in the serialization.
	@param localName The XML local name used in the serialization.
	@return The value of the property, either a <code>RDFResource</code> or a
		<code>RDFLiteral</code>, or <code>null</code> if this resource has no such
		property.
	*/
	public RDFObject getPropertyValue(final URI namespaceURI, final String localName);

	/**Searches and returns an iterator of all property values that appear as RDF
		statement objects with a predicate of <var>propertyURI</var>.
	@param propertyURI The reference URI of the property resources.
	@return An iterator to a read-only list of values of properties.
	*/
	public Iterator<RDFObject> getPropertyValueIterator(final URI propertyURI);

	/**Searches and returns an iterator of all property values of the requested type that appear as RDF
		statement objects with a predicate of <var>propertyURI</var>.
	@param propertyURI The reference URI of the property resources.
	@param valueType The type of values to include
	@return An iterator to a read-only list of values of properties.
	*/
	public <T> Iterator<T> getPropertyValueIterator(final URI propertyURI, final Class<T> valueType);

	/**Searches and returns an iterator of all property values that appear as
		RDF statement objects with a predicate of a property URI formed by the
		given namespace URI and local name. This is a convenience function that
		creates a property URI automatically for searching.
	@param namespaceURI The XML namespace URI that represents part of the reference URI.
	@param localName The XML local name that represents part of the reference URI.
	@return An iterator to a read-only list of values of properties.
	*/
	public Iterator<RDFObject> getPropertyValueIterator(final URI namespaceURI, final String localName);

	/**Searches and returns an iterator of all property values of the requested type that appear as
		RDF statement objects with a predicate of a property URI formed by the
		given namespace URI and local name. This is a convenience function that
		creates a property URI automatically for searching.
	@param namespaceURI The XML namespace URI that represents part of the reference URI.
	@param localName The XML local name that represents part of the reference URI.
	@param valueType The type of values to include
	@return An iterator to a read-only list of values of properties.
	*/
	public <T> Iterator<T> getPropertyValueIterator(final URI namespaceURI, final String localName, final Class<T> valueType);

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
	This is a convenience function that creates a property URI from an XML
		qualified name automatically for searching.
	@param namespaceURI The XML namespace URI that represents part of the reference URI.
	@param localName The XML local name that represents part of the reference URI.
	@param propertyValueURI The URI of the resource to which the property should
		be compared.
	@return <code>true</code> if the specified property is set to the specified
		value.
	*/
	public boolean hasPropertyResourceValue(final URI namespaceURI, final String localName, final URI propertyValueURI);

	/**Adds a property by creating a <code>RDFPropertyValuePair</code> from the given
		property and value. For each property, this resource serves as the subject
		of an RDF statement with the property as the predicate and the value as
		the object.
		<p>Note that the property is not simply a property URI &mdash; it is a
		resource that is identified by the property URI.</p>
		<p>If an equivalent property already exists, no action is taken.</p>
	@param property A property resource; the predicate of an RDF statement.
	@param value A property value; the object of an RDF statement.
	@return The added property value.
	*/
	public RDFObject addProperty(final RDFResource property, final RDFObject value);

	/**Adds a property by creating a <code>RDFPropertyValuePair</code> from the given
		property and value. This is a convenience method that allows specification
		of the property resource through a namespace URI and a local name.
		For each property, this resource serves as the subject
		of an RDF statement with the property as the predicate and the value as
		the object.
	<p>Note that the property is not simply a property URI &mdash; it is a
		resource that is identified by the property URI.</p>
	<p>If an equivalent property already exists, no action is taken.</p>
	@param propertyNamespaceURI The XML namespace URI used in the serialization
		of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The XML local name used in the serialization of the
		property resource that is the predicate of an RDF statement.
	@param value A property value; the object of an RDF statement.
	@return The added property value.
	*/
	public RDFObject addProperty(final URI propertyNamespaceURI, final String propertyLocalName, final RDFObject value);

	/**Adds a plain literal property from a string by creating a <code>RDFPropertyValuePair</code>
		from the given property and value. For each property, this resource serves
		as the subject of an RDF statement with the property as the predicate and
		the value, stored as a literal, as the object. No language is specified.
		<p>Note that the property is not simply a property URI &mdash; it is a
		resource that is identified by the property URI.</p>
		<p>If an equivalent property already exists, no action is taken.</p>
	@param property A property resource; the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a
		<code>RDFLiteral</code>; the object of an RDF statement.
	@return The added property value.
	*/
	public RDFLiteral addProperty(final RDFResource property, final String literalValue);

	/**Adds a plain literal property from a string by creating a <code>RDFPropertyValuePair</code>
		from the given property and value. This is a convenience method that allows specification
		of the property resource through a namespace URI and a local name.
		For each property, this resource serves as the subject of an RDF statement
		with the property as the predicate and the value, stored as a literal, as
		the object. No language is specified
		<p>Note that the property is not simply a property URI &mdash; it is a
		resource that is identified by the property URI.</p>
		<p>If an equivalent property already exists, no action is taken.</p>
	@param propertyNamespaceURI The XML namespace URI used in the serialization
		of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The XML local name used in the serialization of the
		property resource that is the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a
		<code>RDFLiteral</code>; the object of an RDF statement.
	@return The added property value.
	*/
	public RDFLiteral addProperty(final URI propertyNamespaceURI, final String propertyLocalName, final String literalValue);

	/**Adds a plain literal property from a string by creating a <code>RDFPropertyValuePair</code>
		from the given property and value. For each property, this resource serves
		as the subject of an RDF statement with the property as the predicate and
		the value, stored as a literal, as the object.
		<p>Note that the property is not simply a property URI &mdash; it is a
		resource that is identified by the property URI.</p>
		<p>If an equivalent property already exists, no action is taken.</p>
	@param property A property resource; the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a
		<code>RDFLiteral</code>; the object of an RDF statement.
	@param language The language of the plain literal, or <code>null</code> if
		no language should be specified.
	@return The added property value.
	*/
	public RDFLiteral addProperty(final RDFResource property, final String literalValue, final Locale language);

	/**Adds a plain literal property from a string by creating a <code>RDFPropertyValuePair</code>
		from the given property and value. This is a convenience method that allows specification
		of the property resource through a namespace URI and a local name.
		For each property, this resource serves as the subject of an RDF statement
		with the property as the predicate and the value, stored as a literal, as
		the object.
		<p>Note that the property is not simply a property URI &mdash; it is a
		resource that is identified by the property URI.</p>
		<p>If an equivalent property already exists, no action is taken.</p>
	@param propertyNamespaceURI The XML namespace URI used in the serialization
		of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The XML local name used in the serialization of the
		property resource that is the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a
		<code>RDFLiteral</code>; the object of an RDF statement.
	@param language The language of the plain literal, or <code>null</code> if
		no language should be specified.
	@return The added property value.
	*/
	public RDFLiteral addProperty(final URI propertyNamespaceURI, final String propertyLocalName, final String literalValue, final Locale language);
	
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
	
	/**Sets a property by removing all property values for the given property and
		creating a new <code>RDFPropertyValuePair</code> from the given
		property and value. If no value is given, all such properties are removed.
	@param property A property resource&mdash;the predicate of an RDF statement.
	@param value A property value&mdash;the object of an RDF statement&mdash;or
		<code>null</code> if all such properties should be removed with nothing
		to replace them.
	@return The added property value.
	@see #removeProperties(URI)
	@see #addProperty(RDFResource, RDFObject)
	*/
	public RDFObject setProperty(final RDFResource property, final RDFObject value);

	/**Sets a property by first removing all such properties and then adding
		a new property. If no value is given, all such properties are removed.
	@param propertyNamespaceURI The XML namespace URI used in the serialization
		of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The XML local name used in the serialization of the
		property resource that is the predicate of an RDF statement.
	@param value A property value&mdash;the object of an RDF statement&mdash;or
		<code>null</code> if all such properties should be removed with nothing
		to replace them.
	@return The added property value.
	*/
	public RDFObject setProperty(final URI propertyNamespaceURI, final String propertyLocalName, final RDFObject value);

	/**Sets a plain literal property from a string by removing all property values
		for the given property and creating a new <code>RDFPropertyValuePair</code>
		from the given property and value. No language is specified.
	If no value is given, all such properties are removed.
	@param property A property resource; the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a
		<code>RDFLiteral</code>; the object of an RDF statement.
	@return The added property value.
	@see #removeProperties(URI)
	@see #addProperty(RDFResource, String)
	*/
	public RDFLiteral setProperty(final RDFResource property, final String literalValue);

	/**Sets a plain literal property from a string by first removing all such
		properties and then adding a new property. No language is specified.
	If no value is given, all such properties are removed.
	@param propertyNamespaceURI The XML namespace URI used in the serialization
		of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The XML local name used in the serialization of the
		property resource that is the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a
		<code>RDFLiteral</code>; the object of an RDF statement.
	@return The added property value.
	*/
	public RDFLiteral setProperty(final URI propertyNamespaceURI, final String propertyLocalName, final String literalValue);

	/**Sets a plain literal property from a string by removing all property values
		for the given property and creating a new <code>RDFPropertyValuePair</code>
		from the given property and value.
	If no value is given, all such properties are removed.
	@param property A property resource; the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a
		<code>RDFLiteral</code>; the object of an RDF statement.
	@param language The language of the plain literal, or <code>null</code> if
		no language should be specified.
	@return The added property value.
	@see #removeProperties(URI)
	@see #addProperty(RDFResource, String)
	*/
	public RDFLiteral setProperty(final RDFResource property, final String literalValue, final Locale language);

	/**Sets a plain literal property from a string by first removing all such
		properties and then adding a new property.
	If no value is given, all such properties are removed.
	@param propertyNamespaceURI The XML namespace URI used in the serialization
		of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The XML local name used in the serialization of the
		property resource that is the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a
		<code>RDFLiteral</code>; the object of an RDF statement.
	@param language The language of the plain literal, or <code>null</code> if
		no language should be specified.
	@return The added property value.
	*/
	public RDFLiteral setProperty(final URI propertyNamespaceURI, final String propertyLocalName, final String literalValue, final Locale language);

	/**@return A copy of this resource with the same URI and identical properties.*/
	public Object clone();

}