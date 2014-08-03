/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.rdf;

import java.io.*;
import java.net.*;
import java.text.*;
import java.util.*;

import static com.globalmentor.java.Objects.*;
import com.globalmentor.net.URIs;
import static com.globalmentor.rdf.RDF.*;
import com.globalmentor.rdf.rdfs.RDFS;
import com.globalmentor.rdf.xmlschema.*;
import com.globalmentor.text.W3CDateFormat;
import com.globalmentor.text.xml.XML;

import org.w3c.dom.*;

/**
 * Various supporting methods for processing RDF resources and literals.
 * @author Garret Wilson
 */
public class RDFResources {

	/** The start of a reference URI from the rdf:li element qualified name (i.e. "rdfNamespaceURI#li_"), which we'll use to check for items. */
	protected final static String RDF_LI_REFERENCE_PREFIX = createReferenceURI(RDF_NAMESPACE_URI, CONTAINER_MEMBER_PREFIX).toString();

	/**
	 * Adds a <code>rdf:type</code> property to the resource.
	 * <p>
	 * If an equivalent property already exists, no action is taken.
	 * </p>
	 * @param resource The resource to which the type should be added.
	 * @param typeURI The URI of the type to add.
	 * @return The added property value.
	 */
	public static RDFResource addType(final RDFResource resource, final URI typeURI) {
		return resource.addProperty(RDF_NAMESPACE_URI, TYPE_PROPERTY_NAME, locateResource(resource, typeURI)); //get a resource from the type URI and add it as a type
	}

	/**
	 * Adds an <code>rdf:type</code> property to the resource. If an equivalent property already exists, no action is taken.
	 * @param resource The resource to which the type should be added.
	 * @param typeNamespaceURI The namespace URI used of the property value resource that is the object of an RDF statement.
	 * @param typeLocalName The local name of the property value resource that is the object of an RDF statement.
	 * @return The added property value.
	 */
	public static RDFResource addType(final RDFResource resource, final URI typeNamespaceURI, final String typeLocalName) {
		return addType(resource, createReferenceURI(typeNamespaceURI, typeLocalName)); //create a type URI and add that type
	}

	/**
	 * Determines if the RDF object is a list and, if so, casts the object to a list and returns it.
	 * @param rdfObject The RDF object in question.
	 * @return The RDF object as a list, or <code>null</code> if the object is not a list or the object is <code>null</code>.
	 */
	public static RDFListResource<?> asListResource(final RDFObject rdfObject) {
		return asInstance(rdfObject, RDFListResource.class); //cast the object to a list if we can
	}

	/**
	 * Determines if the RDF object is a literal and, if so, casts the object to a literal and returns it.
	 * @param rdfObject The RDF object in question.
	 * @return The RDF object as a literal, or <code>null</code> if the object is not a literal or the object is <code>null</code>.
	 */
	public static RDFLiteral asLiteral(final RDFObject rdfObject) {
		return asInstance(rdfObject, RDFLiteral.class); //cast the object to a literal if we can
	}

	/**
	 * Determines if the RDF object is a resource and, if so, casts the object to a resource and returns it.
	 * @param rdfObject The RDF object in question.
	 * @return The RDF object as a resource, or <code>null</code> if the object is not a resource or the object is <code>null</code>.
	 */
	public static RDFResource asResource(final RDFObject rdfObject) {
		return asInstance(rdfObject, RDFResource.class); //cast the object to a resource if we can
	}

	/**
	 * Determines if the RDF object is a plain literal or a String typed literal and, if so, returns the string lexical form.
	 * @param rdfObject The RDF object in question.
	 * @return The RDF object as a string, or <code>null</code> if the object is not a plain literal or a string typed literal, or the object is <code>null</code>
	 *         .
	 */
	public static String asString(final RDFObject rdfObject) { //TODO should we check the object's rdf:value?
		return rdfObject instanceof RDFPlainLiteral || rdfObject instanceof StringLiteral //if this object is a plain literal or a string literal
		? ((RDFLiteral)rdfObject).getLexicalForm() //return the lexical form of the literal (if the object is a plain literal or a string litera, it's an RDFLiteral)
				: null; //indicate that the object shouldn't be interpreted as a string
	}

	/**
	 * Determines if the RDF object is a plain literal or a URI typed literal and, if so, creates a URI from the literal's lexical form if possible.
	 * @param rdfObject The RDF object in question.
	 * @return The RDF object as a URI, or <code>null</code> if the object is not a plain literal or a URI typed literal, the lexical form does not contain a
	 *         valid URI, or the object is <code>null</code>.
	 */
	public static URI asURI(final RDFObject rdfObject) { //TODO should we allow a resource as well and return its reference URI?
		if(rdfObject instanceof RDFLiteral) { //if this object is a literal
			final RDFLiteral rdfLiteral = (RDFLiteral)rdfObject; //cast the object to a literal
			if(rdfLiteral instanceof URILiteral) { //if this is a URI literal
				return ((URILiteral)rdfLiteral).getValue(); //return the URI literals' value
			} else if(rdfLiteral instanceof RDFPlainLiteral) { //if this is a plain literal
				try {
					return new URI(rdfLiteral.getLexicalForm()); //create a URI from the literal's lexical form
				} catch(final URISyntaxException uriSyntaxException) { //ignore URI format errors and return null
				}
			}
		}
		return null; //indicate that the object didn't contain a valid URI
	}

	/**
	 * Creates a resource reference URI from an RDF namespace URI and an RDF local name.
	 * @param namespaceURI The RDF namespace URI used in the serialization.
	 * @param localName The RDF local name used in the serialization.
	 * @return An RDF reference URI constructed from the given namespace and local name.
	 * @throws NullPointerException if the given namespace URI and/or local name is <code>null</code>.
	 */
	public static URI createReferenceURI(final URI namespaceURI, final String localName) { //TODO del if not needed throws URISyntaxException	//TODO del if not needed with QualifiedName.createReferenceURI
		//TODO check for local names that aren't valid URI characters---see QualifiedName.createReferenceURI
		final StringBuilder stringBuilder = new StringBuilder(); //create a string builder to hold the resource URI
		stringBuilder.append(checkInstance(namespaceURI, "Namespace URI cannot be null.")); //append the namespace URI
		stringBuilder.append(checkInstance(localName, "Local name cannot be null.")); //append the local name
		return URI.create(stringBuilder.toString()); //return a URI from the the string we constructed; if somehow concatenating the strings does not create a valid URI, a runtime exception will be thrown
	}

	/**
	 * Determines if the given property reference URI is an RDF container member reference&mdash;i.e. it begins with <code>rdf:_</code>.
	 * @param propertyReferenceURI The reference URI of the predicate of an RDF statement.
	 * @return <code>true</code> if the property reference URI represents a container member.
	 */
	public static boolean isContainerMemberPropertyReference(final URI propertyReferenceURI) {
		return propertyReferenceURI.toString().startsWith(RDF_LI_REFERENCE_PREFIX); //return whether this property name begins with rdf:_
	}

	/**
	 * Locates and returns an existing resource or creates a new resource from the data model associated with the given resource. If no data model is associated
	 * with the given resource, a new resource will be created and returned.
	 * @param contextResource A resource which may be associated with an RDF data model
	 * @param namespaceURI The XML namespace URI used in the serialization.
	 * @param localName The XML local name used in the serialization.
	 * @return A resource with a reference URI corresponding to the given namespace URI and local name.
	 */
	public static RDFResource locateResource(final RDFResource contextResource, final URI namespaceURI, final String localName) {
		return locateResource(contextResource, createReferenceURI(namespaceURI, localName)); //locate a resource, constructing a reference URI from the given namespace URI and local name
	}

	/**
	 * Locates and returns an existing resource or creates a new resource from the data model associated with the given resource. If no data model is associated
	 * with the given resource, a new resource will be created and returned.
	 * @param contextResource A resource which may be associated with an RDF data model
	 * @param referenceURI The reference URI of the resource to retrieve.
	 * @return A resource with the given reference URI.
	 */
	public static RDFResource locateResource(final RDFResource contextResource, final URI referenceURI) {
		return locateTypedResource(contextResource, referenceURI, null); //locate a resource without knowing its type
	}

	/**
	 * Locates and returns an existing resource or creates a new resource from the data model associated with the given resource. If no data model is associated
	 * with the given resource, a new resource will be created and returned.
	 * @param contextResource A resource which may be associated with an RDF data model
	 * @param referenceURI The reference URI of the new resource.
	 * @param typeURI The URI of the type, or <code>null</code> if the type is not known.
	 * @return A new resource, optionally with the given type, created from the given resource's RDF data model if possible.
	 */
	public static RDFResource locateTypedResource(final RDFResource contextResource, final URI referenceURI, final URI typeURI) {
		return locateTypedResource(contextResource, referenceURI, typeURI != null ? getNamespaceURI(typeURI) : null, typeURI != null ? getLocalName(typeURI) : null); //locate a typed resource after extracting the type namespace URI and local name
	}

	/**
	 * Locates and returns an existing resource or creates a new resource from the data model associated with the given resource. If no data model is associated
	 * with the given resource, a new resource will be created and returned.
	 * @param contextResource A resource which may be associated with an RDF data model
	 * @param referenceURI The reference URI of the new resource.
	 * @param typeNamespaceURI The XML namespace used in the serialization of the type URI, or <code>null</code> if the type is not known.
	 * @param typeLocalName The XML local name used in the serialization of the type URI, or <code>null</code> if the type is not known.
	 * @return A new resource, optionally with the given type, created from the given resource's RDF data model if possible.
	 */
	public static RDFResource locateTypedResource(final RDFResource contextResource, final URI referenceURI, final URI typeNamespaceURI,
			final String typeLocalName) {
		final RDF rdf = contextResource.getRDF(); //get the RDF data model of the given resource
		if(rdf != null) { //if we know which RDF data model to use
			//locate the resource within the RDF data model 
			return rdf.locateTypedResource(referenceURI, typeNamespaceURI, typeLocalName);
		} else { //if there is no known data model
			final RDFResource resource = new DefaultRDFResource(referenceURI); //create a new resource from the given reference URI
			if(typeNamespaceURI != null && typeLocalName != null) { //if we were given a type
				addType(resource, typeNamespaceURI, typeLocalName); //add the type property
			}
			return resource; //return the resource we created			
		}
	}

	/**
	 * Gets an <code>rdf:type</code> property for a given resource. This ensures that an existing type property will be used, if the resource is associated with a
	 * data model and the <code>rdf:type</code> property already exists.
	 * @param contextResource A resource which may be associated with an RDF data model
	 * @return The <code>rdf:type</code> property resource.
	 */
	public static RDFResource locateTypeProperty(final RDFResource contextResource) {
		//TODO pass along the rdf:Property type, maybe, and create a separate method for creating properties
		return locateResource(contextResource, RDF_NAMESPACE_URI, TYPE_PROPERTY_NAME); //get an rdf:type resource
	}

	/**
	 * Returns the value of the first indicated property parsed as a date, using the default W3C full date style.
	 * @param resource The resource the property of which should be located.
	 * @param typeNamespaceURI The XML namespace URI that represents part of the reference URI.
	 * @param typeLocalName The XML local name that represents part of the reference URI.
	 * @param style The style of the date formatting.
	 * @return The value of the first indicated property, or <code>null</code> if no such property exists or it does not contain a date.
	 * @see W3CDateFormat.Style#DATE_TIME
	 */
	public static Date getDate(final RDFResource resource, final URI typeNamespaceURI, final String typeLocalName) {
		return getDate(resource, typeNamespaceURI, typeLocalName, W3CDateFormat.Style.DATE_TIME); //get the date using the default formatter
	}

	/**
	 * Returns the value of the first indicated property parsed as a date.
	 * @param resource The resource the property of which should be located.
	 * @param typeNamespaceURI The XML namespace URI that represents part of the reference URI.
	 * @param typeLocalName The XML local name that represents part of the reference URI.
	 * @param style The style of the date formatting.
	 * @return The value of the first indicated property, or <code>null</code> if no such property exists or it does not contain a date.
	 */
	public static Date getDate(final RDFResource resource, final URI typeNamespaceURI, final String typeLocalName, final W3CDateFormat.Style style) {
		return getDate(resource, typeNamespaceURI, typeLocalName, new W3CDateFormat(style)); //create a date format and get the date
	}

	/**
	 * Returns the value of the first indicated property parsed as a date.
	 * @param resource The resource the property of which should be located.
	 * @param typeNamespaceURI The XML namespace URI that represents part of the reference URI.
	 * @param typeLocalName The XML local name that represents part of the reference URI.
	 * @param dateFormat The object to parse the date.
	 * @return The value of the first property, or <code>null</code> if no such property exists or it does not contain a date.
	 */
	public static Date getDate(final RDFResource resource, final URI typeNamespaceURI, final String typeLocalName, final DateFormat dateFormat) {
		final RDFObject dateObject = resource.getPropertyValue(typeNamespaceURI, typeLocalName); //get the date object
		if(dateObject instanceof RDFLiteral) { //if this is a literal
			final String dateString = ((RDFLiteral)dateObject).getLexicalForm(); //get the string version of the date
			try {
				return dateFormat.parse(dateString); //parse the date from the string and return it
			} catch(final ParseException parseException) { //if there is a problem with the format, ignore it and return null
			}
		}
		return null; //show that for some reason we couldn't parse the date
	}

	/**
	 * Sets the indicated property with the given date value to the resource, using the default W3C full date style.
	 * @param resource The resource to which the property should be set.
	 * @param typeNamespaceURI The XML namespace URI that represents part of the reference URI.
	 * @param typeLocalName The XML local name that represents part of the reference URI.
	 * @param date The property value to set.
	 * @return The added literal property value.
	 * @see W3CDateFormat.Style#DATE_TIME
	 */
	public static RDFLiteral setDate(final RDFResource resource, final URI typeNamespaceURI, final String typeLocalName, final Date date) {
		return setDate(resource, typeNamespaceURI, typeLocalName, date, W3CDateFormat.Style.DATE_TIME); //set the date using the default formatter
	}

	/**
	 * Sets the indicated property with the given date value to the resource, using the given style.
	 * @param resource The resource to which the property should be set.
	 * @param typeNamespaceURI The XML namespace URI that represents part of the reference URI.
	 * @param typeLocalName The XML local name that represents part of the reference URI.
	 * @param date The property value to set.
	 * @param style The style of the date formatting.
	 * @return The added literal property value.
	 */
	public static RDFLiteral setDate(final RDFResource resource, final URI typeNamespaceURI, final String typeLocalName, final Date date,
			final W3CDateFormat.Style style) {
		return setDate(resource, typeNamespaceURI, typeLocalName, date, new W3CDateFormat(style)); //create a date formatter and set the date
	}

	/**
	 * Sets the indicated property with the given date value to the resource, using the given formatter.
	 * @param resource The resource to which the property should be set.
	 * @param typeNamespaceURI The XML namespace URI that represents part of the reference URI.
	 * @param typeLocalName The XML local name that represents part of the reference URI.
	 * @param date The property value to set.
	 * @param dateFormat The object to format the date.
	 * @return The added literal property value.
	 */
	public static RDFLiteral setDate(final RDFResource resource, final URI typeNamespaceURI, final String typeLocalName, final Date date,
			final DateFormat dateFormat) {
		return resource.setProperty(typeNamespaceURI, typeLocalName, dateFormat.format(date));
	}

	/**
	 * Retrieves a resource from an RDF data model that is of the requested type. If there are more than one resource with the requested type, it is undefined
	 * which one will be returned.
	 * @param rdf The RDF data model.
	 * @param typeNamespaceURI The XML namespace URI that represents part of the reference URI.
	 * @param typeLocalName The XML local name that represents part of the reference URI.
	 * @return A resource of the requested type, or <code>null</code> if there are no resourcees with the specified type.
	 */
	public static RDFResource getResourceByType(final RDF rdf, final URI typeNamespaceURI, final String typeLocalName) { //TODO should we move these to the RDF data model?
		final Iterator<RDFResource> resourceIterator = getResourcesByType(rdf, typeNamespaceURI, typeLocalName).iterator(); //get an iterator to matching resources
		return resourceIterator.hasNext() ? (RDFResource)resourceIterator.next() : null; //return the first resource, if there are any at all
	}

	/**
	 * Retrieves the resources in an RDF data model that are of the requested type.
	 * @param rdf The RDF data model.
	 * @param typeNamespaceURI The XML namespace URI that represents part of the reference URI.
	 * @param typeLocalName The XML local name that represents part of the reference URI.
	 * @return A read-only iterable of resources that are of the requested type.
	 */
	public static Iterable<RDFResource> getResourcesByType(final RDF rdf, final URI typeNamespaceURI, final String typeLocalName) {
		return getResourcesByType(rdf, createReferenceURI(typeNamespaceURI, typeLocalName)); //gather the resources with a type property of the URI from the given namespace and local name
	}

	/**
	 * Retrieves a resource from an RDF data model that is of the requested type. If there are more than one resource with the requested type, it is undefined
	 * which one will be returned.
	 * @param rdf The RDF data model.
	 * @param typeURI The reference URI of the type resource.
	 * @return A resource of the requested type, or <code>null</code> if there are no resourcees with the specified type.
	 */
	public static RDFResource getResourceByType(final RDF rdf, final URI typeURI) {
		final Iterator<RDFResource> resourceIterator = getResourcesByType(rdf, typeURI).iterator(); //get an iterator to matching resources
		return resourceIterator.hasNext() ? (RDFResource)resourceIterator.next() : null; //return the first resource, if there are any at all
	}

	/**
	 * Retrieves the resources in an RDF data model that are of the requested type.
	 * @param rdf The RDF data model.
	 * @param typeURI The reference URI of the type resource.
	 * @return A read-only iterable of resources that are of the requested type.
	 */
	public static Iterable<RDFResource> getResourcesByType(final RDF rdf, final URI typeURI) {
		final List<RDFResource> resourceList = new ArrayList<RDFResource>(); //create a list in which to store the resources
		for(final RDFResource resource : rdf.getResources()) { //for each resource in this data model
			if(isType(resource, typeURI)) //if this resource is of the given type
				resourceList.add(resource); //add this resource to our list
		}
		return Collections.unmodifiableList(resourceList); //make the list read-only and return it
	}

	/**
	 * Retrieves the type of the resource. If this resource has more than one property of <code>rdf:type</code>, it is undefined which of those property values
	 * will be returned.
	 * @param resource The resource the type of which will be returned.
	 * @return The type value of the resource, or <code>null</code> if the resource has no type specified.
	 * @throws ClassCastException Thrown if the resource type property value is not a <code>RDFResource</code> (such as a <code>RDFLiteral</code>), which would
	 *           indicate that an incorrect value has been stored for the type.
	 */
	public static RDFResource getType(final RDFResource resource) throws ClassCastException {
		return (RDFResource)resource.getPropertyValue(RDF_NAMESPACE_URI, TYPE_PROPERTY_NAME); //return the type property
	}

	/**
	 * Returns an iterable to each property of <code>rdf:type</code>.
	 * @param resource The resource the types of which will be returned.
	 * @return A read-only iterable the values of <code>rdf:type</code> properties, each item of which is expected to be an <code>RDFResource</code> of the type.
	 */
	public static Iterable<RDFObject> getTypes(final RDFResource resource) {
		return resource.getPropertyValues(RDF_NAMESPACE_URI, TYPE_PROPERTY_NAME); //return an iterable to the type properties		
	}

	/**
	 * Retrieves the value of the resource. If this resource has more than one property of <code>rdf:value</code>, it is undefined which of these property values
	 * will be returned.
	 * @param resource The resource the value of which will be returned.
	 * @return The value of the resource, or <code>null</code> if there is no value.
	 */
	public static RDFObject getValue(final RDFResource resource) {
		return resource.getPropertyValue(RDF_NAMESPACE_URI, VALUE_PROPERTY_NAME); //get the value of the value property
	}

	/**
	 * Replaces all <code>rdf:value</code> properties of the resource with a new property with the given value. No language is specified.
	 * @param resource The resource for which the property should be set.
	 * @param value A string value.
	 */
	public static void setValue(final RDFResource resource, final String value) {
		setValue(resource, value, (Locale)null); //set the value without specifying a language
	}

	/**
	 * Replaces all <code>rdf:value</code> properties of the resource with a new property with the given value.
	 * @param resource The resource for which the property should be set.
	 * @param value A string value.
	 * @param language The language of the plain literal, or <code>null</code> if no language should be specified.
	 */
	public static void setValue(final RDFResource resource, final String value, final Locale language) {
		resource.setProperty(RDF_NAMESPACE_URI, VALUE_PROPERTY_NAME, value, language); //replace all value properties with a literal value
	}

	/**
	 * Replaces all <code>rdf:value</code> properties of the resource with a new property with the given literal.
	 * @param resource The resource for which the property should be set.
	 * @param literal A literal value.
	 */
	public static void setValue(final RDFResource resource, final RDFLiteral literal) {
		resource.setProperty(RDF_NAMESPACE_URI, VALUE_PROPERTY_NAME, literal); //replace all value properties with a literal value
	}

	/**
	 * Determines if the given resource is the empty list.
	 * @param rdf The RDF data model in which to locate the resource.
	 * @return A list resource with the reference URI &amp;rdf;nil.
	 */
	public static boolean isNil(final RDFResource listResource) {
		return listResource != null && NIL_RESOURCE_URI.equals(listResource.getURI()); //see if the resource has the nil URI 
	}

	/**
	 * Determines whether a given resource is of a particular type. Every type property of the resource is checked.
	 * @param resource The resource the type type of which to check.
	 * @param typeURI The reference URI of the type resource.
	 * @return <code>true</code> if the resource has the indicated type property.
	 */
	public static boolean isType(final RDFResource resource, final URI typeURI) {
		return resource.hasPropertyResourceValue(RDF_NAMESPACE_URI, TYPE_PROPERTY_NAME, typeURI); //determine if the resource has a type property of the given URI
	}

	/**
	 * Determines whether a given resource is of a particular type. Every type property of the resource is checked.
	 * @param resource The resource the type type of which to check.
	 * @param typeNamespaceURI The XML namespace URI that represents part of the reference URI.
	 * @param typeLocalName The XML local name that represents part of the reference URI.
	 * @return <code>true</code> if the resource has the indicated type property.
	 */
	public static boolean isType(final RDFResource resource, final URI typeNamespaceURI, final String typeLocalName) {
		return resource.hasPropertyResourceValue(RDF_NAMESPACE_URI, TYPE_PROPERTY_NAME, createReferenceURI(typeNamespaceURI, typeLocalName)); //determine if the resource has a type property of the URI from the given namespace and local name
	}

	/**
	 * Determines a string value for resource appropriate for representation. The label is determined in the following sequence:
	 * <ol>
	 * <li>The lexical form of any literal <code>rdfs:label</code>.</li>
	 * <li>The reference URI.</li>
	 * <li>The lexical form of any literal <code>rdfs:value</code>.</li>
	 * <li>The Java string representation of the resource as given by its <code>toString()</code> method.</li>
	 * </ol>
	 * @param resource The resource the label of which will be returned.
	 * @return The label of the resource, or <code>null</code> if there is no label or the label is not a literal.
	 */
	public static String getDisplayLabel(final RDFResource resource) {
		final RDFLiteral labelLiteral = RDFS.getLabel(resource); //see if there is an rdfs:label property
		if(labelLiteral != null) { //if a rdfs:label property was provided
			return labelLiteral.getLexicalForm(); //return the rdfs:label property's lexical form
		}
		final URI referenceURI = resource.getURI(); //get the resource's reference URI
		if(referenceURI != null) { //if the resource has a reference URI
			return referenceURI.toString(); //return the reference URI
		}
		final RDFObject valueLiteral = getValue(resource); //get the rdfs:value property, if any
		if(valueLiteral instanceof RDFLiteral) { //if a literal rdfs:value property was provided
			return ((RDFLiteral)valueLiteral).getLexicalForm(); //return the rdfs:value property's lexical form
		}
		return resource.toString(); //there's nothing else to do but return the resource's string value
	}

	/**
	 * Determines the RDF namespace of the given reference URI. For most reference URIs, this is the URI formed by all the the reference URI characters up to and
	 * including the last character that is not a valid XML name character. If all characters are valid XML name characters, the last non-alphanumeric character
	 * is used as a delimiter.
	 * @param referenceURI The reference URI for which a namespace URI should be determined.
	 * @return The namespace URI of the reference URI, or <code>null</code> if the namespace URI could not be determined.
	 */
	public static URI getNamespaceURI(final URI referenceURI) { //TODO del in favor of RDFName.createRDFName()
		//TODO do something special for certain namespaces such as for XLink that do not follow the rules
		final String referenceURIString = referenceURI.toString(); //get a string version of the reference URI
		for(int i = referenceURIString.length() - 1; i >= 0; --i) { //look at each character in the reference URI, starting at the end
			final char character = referenceURIString.charAt(i); //get this character
			if(!XML.isNameChar(character) && character != URIs.ESCAPE_CHAR) { //if this is not a name character (but it isn't the URI escape character, either)
				return URI.create(referenceURIString.substring(0, i + 1)); //create a URI using everything up to and including the last non-XML name character
			}
		}
		//if we still don't know the namespace URI, look for the last non-alphanumeric character
		for(int i = referenceURIString.length() - 1; i >= 0; --i) { //look at each character in the reference URI, starting at the end
			if(!Character.isLetter(referenceURIString.charAt(i)) && !Character.isDigit(referenceURIString.charAt(i))) { //if this is not a letter or a number
				//TODO correctly check for a URI syntax error here
				return URI.create(referenceURIString.substring(0, i + 1)); //create a URI using everything up to and including the last non-XML name character
			}
		}
		return null; //show that we couldn't determine a namespace URI from the given reference URI
	}

	/**
	 * Determines the local name to be used for the given reference URI, suitable for XML serialization. For most reference URIs, this is the name formed by all
	 * the the reference URI characters after but not including the last character that is not a valid XML name character. If all characters are valid XML name
	 * characters, the last non-alphanumeric character is used as a delimiter.
	 * @param referenceURI The reference URI for which a local name should be determined.
	 * @return The local name of the reference URI, or <code>null</code> if a local name could not be determined.
	 */
	public static String getLocalName(final URI referenceURI) { //TODO del in favor of RDFName.createRDFName()
		//TODO do something special for certain namespaces such as for XLink that do not follow the rules
		final String referenceURIString = referenceURI.toString(); //get a string version of the reference URI
		for(int i = referenceURIString.length() - 1; i >= 0; --i) { //look at each character in the reference URI, starting at the end
			final char character = referenceURIString.charAt(i); //get this character
			if(!XML.isNameChar(character) && character != URIs.ESCAPE_CHAR) { //if this is not a name character (but it isn't the URI escape character, either)
				return referenceURIString.substring(i + 1); //create a local name using everything after the last non-XML name character
			}
		}
		//if we still don't know the local name, look for the last non-alphanumeric character
		for(int i = referenceURIString.length() - 1; i >= 0; --i) { //look at each character in the reference URI, starting at the end
			if(!Character.isLetter(referenceURIString.charAt(i)) && !Character.isDigit(referenceURIString.charAt(i))) { //if this is not a letter or a number
				return referenceURIString.substring(i + 1); //create a local name using everything after the last non-XML name character
			}
		}
		return null; //show that we couldn't determine a local name from the given reference URI
	}

	/**
	 * Converts an RDF data model to an XML string. If an error occurs converting the XML document document to a string, the normal XML object string will be
	 * returned.
	 * @param rdf The RDF data model to convert.
	 * @return A string representation of an XML serialization of the RDF data model.
	 */
	public static String toString(final RDF rdf) {
		final ByteArrayOutputStream outputStream = new ByteArrayOutputStream(); //create an output stream of bytes
		final RDFXMLGenerator rdfXMLifier = new RDFXMLGenerator(); //create an object to turn the RDF into XML
		final Document document = rdfXMLifier.createDocument(rdf, XML.createDocumentBuilder(true).getDOMImplementation()); //create an XML document from the RDF
		return XML.toString(document); //convert the XML document to a string and return it
	}

	/**
	 * Converts an RDF resource to an XML string. If an error occurs converting the XML document document to a string, the normal XML object string will be
	 * returned.
	 * @param resource The RDF resource to convert.
	 * @return A string representation of an XML serialization of the RDF resource.
	 */
	public static String toString(final RDFResource resource) {
		final ByteArrayOutputStream outputStream = new ByteArrayOutputStream(); //create an output stream of bytes
		final RDFXMLGenerator rdfXMLifier = new RDFXMLGenerator(); //create an object to turn the RDF into XML
		final Document document = rdfXMLifier.createDocument(resource, XML.createDocumentBuilder(true).getDOMImplementation()); //create an XML document from the RDF
		return XML.toString(document); //convert the XML document to a string and return it
	}

}