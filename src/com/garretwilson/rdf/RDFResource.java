package com.garretwilson.rdf;

import java.net.URI;
import java.util.*;
import com.garretwilson.util.*;

/**Represents the an RDF resource connected in an RDF graph.
@author Garret Wilson
@see DefaultRDFResource
*/
public interface RDFResource extends RDFObject, Resource, RDFConstants, Comparable
{

	/**@return The resource identifier URI.*/
	public URI getReferenceURI();

	/**@return The XML namespace URI used in serialization, or <code>null</code>
		if no namespace URI was used or there was no namespace.*/
	public URI getNamespaceURI();

	/**@return The XML local name used in serialization, or <code>null</code>
		if no namespace URI and local name was used.*/
	public String getLocalName();

	/**@return The number of properties this resource has.*/
	public int getPropertyCount();

	/**@return An iterator that allows traversal of all properties, each of which
		is a <code>NameValuePair</code>, with the name being the property predicate
		and the value being the property value.
	*/
	public ListIterator getPropertyIterator();

	/**Searches and returns the first occurring property value that appears as
		an RDF statement object with a predicate of <code>propertyResource</code>.
	@param propertyResource The property resource.
	@return The value of the property, either a <code>RDFResource</code> or a
		<code>Literal</code>, or <code>null</code> if this resource has no such
		property.
	*/
	public RDFObject getPropertyValue(final RDFResource propertyResource);

	/**Searches and returns the first occurring property value that appears as
		an RDF statement object with a predicate of <code>propertyURI</code>.
	@param propertyURI The reference URI of the property resource.
	@return The value of the property, either a <code>RDFResource</code> or a
		<code>Literal</code>, or <code>null</code> if this resource has no such
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
		<code>Literal</code>, or <code>null</code> if this resource has no such
		property.
	*/
	public RDFObject getPropertyValue(final URI namespaceURI, final String localName);

	/**Searches and returns an iterator of all property values that appear as RDF
		statement objects with a predicate of <code>propertyURI</code>.
	@param propertyURI The reference URI of the property resources.
	@return An iterator to a read-only list of values of properties, each either a
		<code>RDFResource</code> or a <code>Literal</code>.
	*/
	public Iterator getPropertyValueIterator(final URI propertyURI);  //G***maybe fix to make the iterator dynamic to the RDF data model

	/**Searches and returns an iterator of all property values that appear as
		RDF statement objects with a predicate of a property URI formed by the
		given namespace URI and local name. This is a convenience function that
		creates a property URI automatically for searching.
	@param namespaceURI The XML namespace URI that represents part of the reference URI.
	@param localName The XML local name that represents part of the reference URI.
	@return An iterator to a read-only list of values of properties, each either a
		<code>RDFResource</code> or a <code>Literal</code>.
	*/
	public Iterator getPropertyValueIterator(final URI namespaceURI, final String localName);

	/**Determines if the resource has the given property with the given value.
		Each matching property is compared to the property value using the
		property's <code>equal()</code> method.
	@param propertyURI The reference URI of the property resource.
	@param propertyValue The object to which the property should be compared,
		such a resource reference URI, a resource, or a literal.
	@return <code>true</code> if the specified property is set to the specified
		value.
	*/
	public boolean hasPropertyValue(final URI propertyURI, final Object propertyValue);

	/**Determines if the resource has the given property with the given value.
		Each matching property is compared to the property value using the
		property's <code>equal()</code> method. This is a convenience function that
		creates a property URI from an XML qualified name automatically for searching.
	@param namespaceURI The XML namespace URI that represents part of the reference URI.
	@param localName The XML local name that represents part of the reference URI.
	@param propertyValue The object to which the property should be compared,
		such a resource reference URI, a resource, or a literal.
	@return <code>true</code> if the specified property is set to the specified
		value.
	*/
	public boolean hasPropertyValue(final URI namespaceURI, final String localName, final Object propertyValue);

	/**Adds a property by creating a <code>NameValuePair</code> from the given
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

	/**Adds a literal property from a string by creating a <code>NameValuePair</code>
		from the given property and value. For each property, this resource serves
		as the subject of an RDF statement with the property as the predicate and
		the value, stored as a literal, as the object.
		<p>Note that the property is not simply a property URI &mdash; it is a
		resource that is identified by the property URI.</p>
		<p>If an equivalent property already exists, no action is taken.</p>
	@param property A property resource; the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a
		<code>Literal</code>; the object of an RDF statement.
	@return The added property value.
	*/
	public Literal addProperty(final RDFResource property, final String literalValue);

	/**@return <code>true</code> if this resource is an anonymous resource;
		currently anonymous resources are those that either have no reference URI
		or that have a reference URI that begins with "anonymous:".
	*/
	public boolean isAnonymous();

}