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

import java.net.URI;
import java.util.*;

import static com.globalmentor.collections.Collections.*;
import static com.globalmentor.java.Objects.*;

import com.globalmentor.collections.IdentityHashSet;
import com.globalmentor.model.Locales;
import com.globalmentor.text.xml.XMLNamespacePrefixManager;
import com.globalmentor.text.xml.XML;
import static com.globalmentor.rdf.RDF.*;
import static com.globalmentor.rdf.RDFResources.*;
import static com.globalmentor.rdf.RDFXML.*;
import static com.globalmentor.text.xml.XML.*;
import com.globalmentor.util.*;

import org.w3c.dom.*;

/**
 * Class that creates an XML representation of RDF through DOM.
 * <p>
 * This implementation resets the node ID generator between each XML generation, which means that use of the same instance of this class is not appropriate for
 * multiple generations to the same RDF instance XML document or fragment.
 * </p>
 * <p>
 * This class is not thread safe.
 * </p>
 * @author Garret Wilson
 */
public class RDFXMLGenerator //TODO fix bug that doesn't serialize property value resources with no properties
{

	/** The base URI of the RDF data model, or <code>null</code> if the base URI is unknown. */
	private final URI baseURI;

	/** @return The base URI of the RDF data model, or <code>null</code> if the base URI is unknown. */
	public URI getBaseURI() {
		return baseURI;
	}

	/** Create a new XML namespace prefix manager. */
	private final XMLNamespacePrefixManager xmlNamespacePrefixManager;

	/**
	 * Registers the given XML serialization prefix to be used with the given namespace URI. If a prefix is already registered with the given namespace, it is
	 * replaced with this prefix.
	 * @param namespaceURI The namespace URI to register.
	 * @param prefix The XML serialization prefix to use with the given namespace.
	 */
	public void registerNamespacePrefix(final URI namespaceURI, final String prefix) {
		xmlNamespacePrefixManager.registerNamespacePrefix(namespaceURI.toString(), prefix); //store the prefix in the map, keyed to the URI
	}

	/**
	 * Unregisters the XML serialization prefix for the given namespace URI. If no prefix is registered for the given namespace, no action occurs.
	 * @param namespaceURI The namespace URI to unregister.
	 */
	public void unregisterNamespacePrefix(final URI namespaceURI) {
		xmlNamespacePrefixManager.unregisterNamespacePrefix(namespaceURI.toString()); //remove whatever prefix is registered with this namespace, if any
	}

	/**
	 * The set of resources that have been serialized, using identity rather than equality for equivalence, as required be serialization.
	 */
	private final Set<RDFResource> serializedResourceSet;

	/**
	 * Determines if the given resource has been serialized.
	 * @param resource The resource to check.
	 * @return <code>true</code> if the resource has already been serialized, else <code>false</code>.
	 */
	protected final boolean isSerialized(final RDFResource resource) {
		return serializedResourceSet.contains(resource); //see if the resource is in our set
	}

	/**
	 * Sets the serialized status of a given resource.
	 * @param resource The resource for which the serialization status should be set.
	 * @param serialized <code>true</code> if the resource has been serialized, or <code>false</code> if not.
	 */
	protected final void setSerialized(final RDFResource resource, final boolean serialized) {
		if(serialized) { //if the resource has been serialized
			serializedResourceSet.add(resource); //add the resource to the set of serialized resources
		} else { //if the resource has not been serialized
			serializedResourceSet.remove(resource); //remove the resource from the set of serialized resources
		}
	}

	/**
	 * A map that associates, for each resource, a set of all resources that reference the that resource, using identity rather than equality for equivalence.
	 */
	private final Map<RDFResource, Set<RDFResource>> resourceReferenceMap;

	/**
	 * Returns the set of resources that reference this resource as already calculated.
	 * @param resource The resource for which references should be returned.
	 * @return A set of all references to the resource that have been gathered at an earlier time, or <code>null</code> if no references have been gathered for
	 *         the given resource.
	 */
	protected Set<RDFResource> getReferenceSet(final RDFResource resource) {
		return resourceReferenceMap.get(resource); //get the set of references, if any, associated with the resource
	}

	/**
	 * A map of node ID strings keyed to the resource they represent, using identity rather than equality for equivalence for comparing resources.
	 */
	private final Map<RDFResource, String> nodeIDMap;

	/**
	 * Retrieves a node ID appropriate for the given resource. If the resource has already been assigned a node ID, it will be returned; otherwise, a new node ID
	 * will be generated.
	 * @param resource The resource for which a node ID should be returned
	 * @return
	 */
	protected String getNodeID(final RDFResource resource) {
		String nodeID = nodeIDMap.get(resource); //get a node ID for the given resource
		if(nodeID == null) { //if there is no node ID for this resource
			nodeID = "node" + serializedResourceSet.size() + 1; //generate a node ID for the resource, based upon the number of resources already serialized
			nodeIDMap.put(resource, nodeID); //associate the node ID with the resource
		}
		return nodeID; //return the retrieved or generated node ID
	}

	/**
	 * Whether we're in the mode that allows us to put off serialization of some resources until the very end.
	 */
	//G***fix	private boolean isSerializationDeferable=true;

	/** Indicates literals should be serialized as attributes by default. */
	//G***del	public final static boolean LITERAL_ATTRIBUTE_SERIALIZATION=1;
	/** Indicates literals should be serialized as elements by default. */
	//G***del	public final static boolean LITERAL_ELEMENT_SERIALIZATION=2;

	/** Whether literals are serialized as attributes, if possible. */
	private boolean literalAttributeSerialization = true;

	/**
	 * @return <code>true</code> if literals are serialized as attributes, if possible.
	 */
	public boolean isLiteralAttributeSerialization() {
		return literalAttributeSerialization;
	}

	/**
	 * Sets whether literal property values are serialized as attributes.
	 * @param newLiteralAttributeSerialization <code>true</code> if literals should be serialized as attributes if possible.
	 */
	public void setLiteralAttributeSerialization(final boolean newLiteralAttributeSerialization) {
		literalAttributeSerialization = newLiteralAttributeSerialization;
	}

	/**
	 * A set of namespace URIs the properties in which should have literal values serialized as attributes rather than elements.
	 */
	private final Set<URI> literalAttributeSerializationNamespaceSet = new HashSet<URI>();

	/**
	 * Adds a namespace URI the properties in which namespace should be categorically serialized as attributes if the property value is a literal. For attributes
	 * added by this method, the value of <code>isLiteralAttributeSerialization()</code> is ignored.
	 * @param namespaceURI The URI of the namespace that should have its literal property values serialized as attributes.
	 */
	public void addLiteralAttributeSerializationNamespaceURI(final URI namespaceURI) {
		literalAttributeSerializationNamespaceSet.add(namespaceURI); //add the namespace to the set
	}

	/**
	 * Determines whether literal property values from a given namespace should be serialized as attributes.
	 * @param namespaceURI The URI of the namespace in question
	 * @return <code>true</code> if the literal property values from the given namespace should be serialized as attributes.
	 * @see #isLiteralAttributeSerialization
	 */
	public boolean isLiteralAttributeSerializationNamespaceURI(final URI namespaceURI) {
		return literalAttributeSerializationNamespaceSet.contains(namespaceURI); //if this namespace is in the set, return true
	}

	/** Whether lists are serialized in compact form, if possible. */
	private boolean compactRDFListSerialization = true;

	/** @return <code>true</code> if lists are serialized in compact form, if possible. */
	public boolean isCompactRDFListSerialization() {
		return compactRDFListSerialization;
	}

	/**
	 * Sets whether lists are serialized in compact form.
	 * @param compactRDFListSerialization <code>true</code> if lists are serialized in compact form, if possible.
	 */
	public void setCompactRDFListSerialization(final boolean compactRDFListSerialization) {
		this.compactRDFListSerialization = compactRDFListSerialization;
	}

	/** Whether URI references are relativized to the base URI if possible. */
	private boolean uriReferencesRelativized = true;

	/** @return <code>true</code> if URI references are relativized to the base URI if possible.. */
	public boolean isURIReferencesRelativized() {
		return uriReferencesRelativized;
	}

	/**
	 * Sets whether URI references are relativized to the base URI if possible..
	 * @param uriReferencesRelativized <code>true</code> if URI references should be relativized to the base URI if possible.
	 */
	public void setURIReferencesRelativized(final boolean uriReferencesRelativized) {
		this.uriReferencesRelativized = uriReferencesRelativized;
	}

	/** Default constructor that creates a compact XMLifier. */
	public RDFXMLGenerator() {
		this(true); //construct a compact XMLifier
	}

	/**
	 * Compact constructor.
	 * @param compact Whether the XML representation should be as compact as possible; if <code>false</code>, literals will by default be represented as child
	 *          elements instead of attributes.
	 * @see #setLiteralAttributeSerialization(boolean)
	 */
	public RDFXMLGenerator(final boolean compact) {
		this(new XMLNamespacePrefixManager()); //construct the class with a default XML namespace prefix manager
		setLiteralAttributeSerialization(compact); //if we should be compact, show literals as attributes
	}

	/**
	 * XML namespace prefix manager constructor with no base URI and compact serialization.
	 * @param xmlNamespacePrefixManager The object managing XML namespaces and prefixes.
	 * @excepion NullPointerException if the given XML namespace prefix manager is <code>null</code>.
	 */
	public RDFXMLGenerator(final XMLNamespacePrefixManager xmlNamespacePrefixManager) {
		this(null, xmlNamespacePrefixManager); //create the class with no base URI
	}

	/**
	 * Base URI constructor with compact serialization.
	 * @param baseURI The base URI of the RDF data model, or <code>null</code> if the base URI is unknown.
	 */
	public RDFXMLGenerator(final URI baseURI) {
		this(baseURI, new XMLNamespacePrefixManager()); //construct the class with a default XML namespace prefix manager
	}

	/**
	 * Base URI and XML namespace prefix manager constructor with compact serialization.
	 * @param baseURI The base URI of the RDF data model, or <code>null</code> if the base URI is unknown.
	 * @param xmlNamespacePrefixManager The object managing XML namespaces and prefixes.
	 * @excepion NullPointerException if the given XML namespace prefix manager is <code>null</code>.
	 */
	public RDFXMLGenerator(final URI baseURI, final XMLNamespacePrefixManager xmlNamespacePrefixManager) {
		this.baseURI = baseURI; //save the base URI
		this.xmlNamespacePrefixManager = checkInstance(xmlNamespacePrefixManager, "XML namespace prefix manager cannot be null.");
		serializedResourceSet = new IdentityHashSet<RDFResource>(); //create a map that will determine whether resources have been serialized, based upon the identity of resources
		resourceReferenceMap = new IdentityHashMap<RDFResource, Set<RDFResource>>(); //create a map of sets of referring resources for each referant resource, using identity rather than equality for equivalence
		nodeIDMap = new IdentityHashMap<RDFResource, String>(); //create a map of node IDs keyed to resources, using identity rather than equality to determine associated resource
	}

	/**
	 * Initializes the generator by resetting values and initializing the namespace prefixes.
	 * @see #reset()
	 */
	protected void initialize() {
		reset(); //reset the generator
	}

	/** Releases memory by clearing all internal maps and sets of resources. */
	public void reset() {
		serializedResourceSet.clear(); //show that we've not serialized any resources
		resourceReferenceMap.clear(); //clear all our references to resources
		nodeIDMap.clear(); //clear our map of node IDs
	}

	/**
	 * Creates a default RDF XML document.
	 * @param domImplementation The DOM implementation to use.
	 * @return A newly created default RDF XML document with an RDF section.
	 */
	public static Document createDefaultRDFDocument(final DOMImplementation domImplementation) {
		//create an RDF document
		final Document document = domImplementation.createDocument(RDF_NAMESPACE_URI.toString(), createQName(RDF_NAMESPACE_PREFIX, ELEMENT_RDF), null);
		//add the RDF namespace declaration prefix, xmlns:rdf
		document.getDocumentElement().setAttributeNS(XML.XMLNS_NAMESPACE_URI.toString(), createQName(XML.XMLNS_NAMESPACE_PREFIX, RDF_NAMESPACE_PREFIX),
				RDF_NAMESPACE_URI.toString());
		return document; //return the document we created
	}

	/**
	 * Creates an XML document with <code>&lt;rdf:RDF&gt;</code> as the document element, and creates elements representing the given RDF data model as children
	 * of the that element.
	 * @param rdf The RDF data model to XMLify.
	 * @param domImplementation The DOM implementation to use.
	 * @return The XML document tree that represents the given RDF data model.
	 */
	public Document createDocument(final RDF rdf, final DOMImplementation domImplementation) {
		initialize(); //make sure we don't have information left over from other calls to this method
		rdf.getReferences(resourceReferenceMap); //get all the references to resources in the RDF data model
		try {
			final Document document = createDefaultRDFDocument(domImplementation); //create a default RDF document
			final Element rootElement = document.getDocumentElement(); //get the document element
			createElements(rdf, rootElement); //XMLify the resource as elements under the document element
			return document; //return the document we created
		} finally {
			reset(); //clean up by releasing all local references to resources
		}
	}

	/**
	 * Creates an XML document with <code>&lt;rdf:RDF&gt;</code> as the document element, and creates an element representing the given resource.
	 * @param resource The resource to store as an XML element, or <code>null</code> if all non-anonymous resources should be stored.
	 * @param domImplementation The DOM implementation to use.
	 * @return The XML document tree that represents the given RDF resource.
	 */
	public Document createDocument(final RDFResource resource, final DOMImplementation domImplementation) {
		initialize(); //make sure we don't have information left over from other calls to this method
		getReferences(resource, resourceReferenceMap); //get all the references to the given resource
		try {
			final Document document = createDefaultRDFDocument(domImplementation); //create a default RDF document
			final Element rootElement = document.getDocumentElement(); //get the document element
			final Element resourceElement = createResourceElement(document, resource); //create an element from this resource
			rootElement.appendChild(resourceElement); //add the resource element to the root element
			return document; //return the document we created
		} finally {
			reset(); //clean up by releasing all local references to resources
		}
	}

	/**
	 * Creates the <rdf:RDF> element needed in an XML serialization of RDF.
	 * @param document The document to use as an element factory for the element.
	 * @return An <rdf:RDF> element for the given document.
	 */
	public static Element createRDFElement(final Document document) {
		return document.createElementNS(RDF_NAMESPACE_URI.toString(), createQName(RDF_NAMESPACE_PREFIX, ELEMENT_RDF)); //create an <rdf:RDF> element for the given document
	}

	/**
	 * Creates an <code>&lt;rdf:RDF&gt;</code> XML element and creates elements representing the given RDF data model as children of the that element.
	 * @param rdf The RDF data model to XMLify.
	 * @param document The XML document to use as an element factory.
	 * @return The RDF element created.
	 */
	public Element createElement(final RDF rdf, final Document document) {
		initialize(); //make sure we don't have information left over from other calls to this method
		rdf.getReferences(resourceReferenceMap); //get all the references to resources in the RDF data model
		try {
			//create an <rdf:RDF> element
			final Element rdfElement = document.createElementNS(RDF_NAMESPACE_URI.toString(), createQName(RDF_NAMESPACE_PREFIX, ELEMENT_RDF));
			createElements(rdf, rdfElement); //XMLify the resourcs as elements under the RDF element
			return rdfElement; //return the element we constructed with RDF resources as child elements
		} finally {
			reset(); //clean up by releasing all local references to resources
		}
	}

	/**
	 * Creates elements representing the given RDF data model as children of the given parent element.
	 * @param rdf The RDF data model to XMLify.
	 * @param parentElement The parent XML element of the elements to be added.
	 */
	protected void createElements(final RDF rdf, final Element parentElement) {
		for(final RDFResource resource : rdf.getRootResources()) { //for each root RDF resource
			final Element resourceElement = createResourceElement(parentElement.getOwnerDocument(), resource); //create an element from this resource
			parentElement.appendChild(resourceElement); //add the resource element to our parent element
		}
	}

	/**
	 * Creates an XML element to represent the given resource.
	 * @param document The document to be used as an element factory.
	 * @param resource The resource for which an element should be created.
	 * @return The XML element that represents the given RDF resource.
	 */
	public Element createResourceElement(final Document document, final RDFResource resource) {
		final Element resourceElement; //we'll store here the element that we create
		final RDFResource resourceType = getType(resource); //get the type of the resource
		if(resourceType != null) { //if this resource has a type that we can use for the element name
			final URI resourceTypeURI = resourceType.getURI(); //get the URI of the type
			final URI namespaceURI = getNamespaceURI(resourceTypeURI); //get the type's namespace URI
			assert namespaceURI != null : "Unable to find namespace of type " + resourceTypeURI; //TODO add real error handling here
			final String prefix = xmlNamespacePrefixManager.getNamespacePrefix(namespaceURI.toString()); //get the prefix for use with this namespace
			final String qualifiedName = createQName(prefix, getLocalName(resourceTypeURI)); //create a qualified name for the element
			//TODO check for null on local name
			resourceElement = document.createElementNS(namespaceURI.toString(), qualifiedName); //create an element from the resource type
		} else { //if we can't create an element from its type
			final String qualifiedName = createQName(RDF_NAMESPACE_PREFIX, ELEMENT_DESCRIPTION); //create a qualified name for the element
			resourceElement = document.createElementNS(RDF_NAMESPACE_URI.toString(), qualifiedName); //create an rdf:Description element
		}
		final URI referenceURI = resource.getURI(); //get the reference URI of the resource
		if(referenceURI != null) { //if this resource has a reference URI (i.e. it isn't a blank node)
			final URI serializationReferenceURI; //we'll relativize the URI if possible
			if(isURIReferencesRelativized()) { //if we should relativize URI references
				final URI baseURI = getBaseURI(); //get the base URI, if any
				serializationReferenceURI = baseURI != null ? baseURI.relativize(referenceURI) : referenceURI; //if there is a base URI, relativize this resource's URI against the base URI
			} else { //if we shouldn't relativize URI references
				serializationReferenceURI = referenceURI; //use the full reference URI as the serialization URI
			}
			//set the rdf:about attribute to the reference URI
			final String aboutAttributeQualifiedName = createQName(RDF_NAMESPACE_PREFIX, ATTRIBUTE_ABOUT); //create a qualified name for reference URI
			//add the rdf:about attribute with the value set to the reference URI of the resource
			resourceElement.setAttributeNS(RDF_NAMESPACE_URI.toString(), aboutAttributeQualifiedName, serializationReferenceURI.toString());
		} else { //if this resource has no reference URI, we'll have to give it a node ID if any other resources reference it
			final Set<RDFResource> referenceSet = getReferenceSet(resource); //find out which resources reference this resource
			if(referenceSet != null && referenceSet.size() > 1) { //if more than one resource references this resource (if there's only one reference to it, we'll just serialize it inline)
				final String nodeID = getNodeID(resource); //get the node ID for this resource
				//set the rdf:nodeID attribute to the node ID
				final String nodeIDAttributeQualifiedName = createQName(RDF_NAMESPACE_PREFIX, ATTRIBUTE_NODE_ID); //create a qualified name for node ID
				//add the rdf:nodeID attribute with the value set to the node ID of the resource
				resourceElement.setAttributeNS(RDF_NAMESPACE_URI.toString(), nodeIDAttributeQualifiedName, nodeID);
			}
		}
		if(!isSerialized(resource)) { //if we haven't serialized this resource, yet
			setSerialized(resource, true); //show that we've serialized this resource (do this before adding properties, just in case there are circular references)
			addProperties(document, resourceElement, resource, resourceType); //add all the properties of the resource to the resource element
		}
		return resourceElement; //return the resource element we created
	}

	/**
	 * Sets the appropriate attributes of the given element so that the element becomes a reference to the indicated resource. This will either be
	 * <code>rdf:resource</code> if the resource has a reference URI, or <code>rdf:nodeID</code> if the resource has no reference URI.
	 * @param element The XML element which should indicate a reference to the resource.
	 * @param resource The resource this element should reference.
	 */
	protected void setReference(final Element element, final RDFResource resource) {
		final URI referenceURI = resource.getURI(); //get the reference URI of the resource
		if(referenceURI != null) { //if the resource has a reference URI, reference the reference URI using rdf:resource
			final String resourceAttributeQualifiedName = createQName(RDF_NAMESPACE_PREFIX, ATTRIBUTE_RESOURCE); //create a qualified name for the link attribute
			//add the rdf:resource attribute with the value set to the reference URI of the property value resource
			element.setAttributeNS(RDF_NAMESPACE_URI.toString(), resourceAttributeQualifiedName, referenceURI.toString());
		} else { //if the object resource has no reference URI, we'll reference it using rdf:nodeID
			final String nodeID = getNodeID(resource); //get the node ID for this resource
			//set the rdf:nodeID attribute to the node ID
			final String nodeIDAttributeQualifiedName = createQName(RDF_NAMESPACE_PREFIX, ATTRIBUTE_NODE_ID); //create a qualified name for node ID
			//add the rdf:nodeID attribute with the value set to the node ID of the resource
			element.setAttributeNS(RDF_NAMESPACE_URI.toString(), nodeIDAttributeQualifiedName, nodeID);
		}
	}

	/**
	 * Adds all properties of the resource to the given element.
	 * @param document The document to be used as an element factory.
	 * @param element The XML element to which the properties should be added
	 * @param resource The resource the properties of which should be added to the DOM tree, either as elements or attributes.
	 * @param resourceType The main resource type which was used when serializing the element, or <code>null</code> if no main resource type was used.
	 */
	public void addProperties(final Document document, final Element element, final RDFResource resource, final RDFResource resourceType) {
		for(final RDFPropertyValuePair propertyValuePair : resource.getProperties()) { //for each resource property
			//if this property is not a type property we already used for creating the element name
			if(!(TYPE_PROPERTY_REFERENCE_URI.equals(propertyValuePair.getProperty().getURI()) && propertyValuePair.getPropertyValue().equals(resourceType))) {
				addProperty(document, element, propertyValuePair); //create a representation for this property
			}
		}
	}

	/**
	 * Adds all properties of the resource to the given element.
	 * @param document The document to be used as an element factory.
	 * @param element The XML element to which the properties should be added
	 * @param propertyValuePair The property to be added to the DOM tree, either as an element or as an attributes.
	 * @return The XML child element used to represent the given property, or <code>null</code> if an XML attribute was used to represent the given property.
	 */
	public Element addProperty(final Document document, final Element element, final RDFPropertyValuePair propertyValuePair) {
		final RDFResource propertyResource = propertyValuePair.getProperty(); //get the property predicate
		final RDFObject propertyValue = propertyValuePair.getPropertyValue(); //get the property value
		final URI propertyResourceURI = propertyResource.getURI(); //get the property resource URI
		final URI propertyNamespaceURI = getNamespaceURI(propertyResourceURI); //get the namespace URI of the property
		assert propertyNamespaceURI != null : "Unable to find namespace of property " + propertyResourceURI; //TODO add real error handling here
		if(propertyValue instanceof RDFPlainLiteral) { //if this is a plain literal, see if we can serialize it as an attribute	TODO add support for typed literals
			//if we should serialize all literal property values as attributes, or if we should serialize literal property values from this namespace 
			if(isLiteralAttributeSerialization() || isLiteralAttributeSerializationNamespaceURI(propertyNamespaceURI)) {
				final RDFPlainLiteral plainLiteral = (RDFPlainLiteral)propertyValue; //cast the value to a plain literal
				if(plainLiteral.getLanguage() == null) { //if there is no language definition
					final String prefix = xmlNamespacePrefixManager.getNamespacePrefix(propertyNamespaceURI.toString()); //get the prefix for use with this namespace
					final String localName = getLocalName(propertyResourceURI); //get the local name of the property
					//TODO check for null on local name
					if(!element.hasAttributeNS(propertyNamespaceURI.toString(), localName)) { //if we don't already have this attribute (otherwise, we'll have to use a child element after all
						final String qualifiedName = createQName(prefix, localName); //create a qualified name for the attribute
						//store the property as an attribute
						element.setAttributeNS(propertyNamespaceURI.toString(), qualifiedName, ((RDFLiteral)propertyValue).getLexicalForm());
						return null; //indicate that we didn't use an element to represent the property
					}
				}
			}
		}
		final Element propertyElement = createPropertyElement(document, propertyResource, propertyValue); //create a property element
		element.appendChild(propertyElement); //append the property element we created
		return propertyElement; //return the element we used to represent the property
	}

	/**
	 * Creates an XML element to represent the given property. All anonymous property value resources will be created as child elements. If an anonymous property
	 * value's properties are all literals, that property resource value's properties will be created as attributes of the property.
	 * @param document The document to be used as an element factory.
	 * @param propertyResource The property predicate for which an element should be created.
	 * @param propertyValue The property value for which an element should be created.
	 * @return The XML element that represents the given RDF property.
	 */
	//G***del rdf if we don't need
	protected Element createPropertyElement(final Document document, final RDFResource propertyResource, final RDFObject propertyValue) {
		//G***del		final URI namespaceURI; //we'll store here the namespace URI of the property element
		final String qualifiedName; //we'll store here the qualified name of the property element
		//if we know the namespace URI and local name of the property
		//TODO replace all this code with the new utility method getLabel(RDFResource)
		final URI propertyResourceURI = propertyResource.getURI(); //get the URI of the property
		final URI namespaceURI = getNamespaceURI(propertyResourceURI); //get the namespace URI of the property
		assert namespaceURI != null : "Unable to find namespace of property " + propertyResourceURI; //TODO add real error handling here
		final String prefix = xmlNamespacePrefixManager.getNamespacePrefix(namespaceURI.toString()); //get the prefix for use with this namespace
		String localName = getLocalName(propertyResourceURI); //get the local name of the property
		//TODO check for null on local name
		if(RDF_NAMESPACE_URI.equals(namespaceURI)) { //if this is the RDF namespace
			if(localName.startsWith(CONTAINER_MEMBER_PREFIX)) //if this is one of the rdf:li_XXX members
				localName = LI_PROPERTY_NAME; //just use the normal rdf:li property name---the order is implicit in the serialization
		}
		qualifiedName = createQName(prefix, localName); //create a qualified name for the element
		final Element propertyElement = document.createElementNS(namespaceURI.toString(), qualifiedName); //create an element from the property resource
		if(propertyValue instanceof RDFResource) { //if the property value is a resource
			if(propertyValue instanceof RDFListResource && isCompactRDFListSerialization()) { //if this is a list and we should serialize lists in compact form TODO maybe make sure it's a normal list, first, before creating the compact form
				//set the rdf:parseType attribute to "Collection" TODO eventually get our prefix from a prefix rather than hard-coding RDF, maybe
				propertyElement.setAttributeNS(RDF_NAMESPACE_URI.toString(), createQName(RDF_NAMESPACE_PREFIX.toString(), ATTRIBUTE_PARSE_TYPE), COLLECTION_PARSE_TYPE);
				final RDFListResource propertyListResource = (RDFListResource)propertyValue; //cast the resource to a list
				final Iterator<RDFObject> iterator = propertyListResource.iterator(); //get an iterator to look at the list elements
				while(iterator.hasNext()) { //while there are more elements

					//TODO important: fix; individual elements can be literals

					final RDFResource elementResource = (RDFResource)iterator.next(); //get the next element of the list
					final Element elementResourceElement = createResourceElement(document, elementResource); //create an element from this list element resource
					propertyElement.appendChild(elementResourceElement); //add the list element resource element resource to the property element
				}
			} else { //if this is a normal property resource value
				final RDFResource valueResource = (RDFResource)propertyValue; //cast the value to a resource
				final Set<RDFResource> referenceSet = getReferenceSet(valueResource); //find out which resources reference this property value
				//G***if we're only serializing a single resource tree, the commented-out code on this line will result in missing nodes; maybe allow a setFlatSerialization option later, but that may be too flat---maybe have a setSortOfFlat()...				if(valueResource.getReferenceURI()==null) //if this is a blank node
				/*G***fix or delete
							if(!isSerialized(valueResource)) {	//if we haven't serialized the property value resource, yet
									//if more than one resource references this resource, don't try to
									//	choose which one gets an inline reference---defer serialization
									//	until later
								if(referenceSet!=null && referenceSet.size()>1 && isSerializationDeferable()) {	//if more than one resource references this resource (i.e. more than one property has this resource for a value) and we can defer serialization
									deferSerialization(valueResource);	//defer serialization until later
								}
				*/
				//see if we should serialize inline; we'll do so if all the following are true
				//	1. the resource is not yet serialized
				//	2. the resource is not referenced by a list elsewhere (always defer to the list for inline serialization)
				//	3. the resource has at least one property
				//if all the conditions are not met, we'll only reference the resource
				if(!isSerialized(valueResource) //if we haven't serialized the property value resource, yet
						&& (referenceSet == null || !containsInstance(referenceSet, RDFListResource.class)) //if this resource is not referenced by a list
						&& valueResource.getPropertyCount() > 0) { //if this resource has at least one property 
					boolean serializeSubPropertyLiteralAttributes = true; //we'll see if all the subproperties are plain literals without language indications; if so, we'll just add them as attributes
					//TODO del Log.trace("ready to look at all property value properties");
					final Iterator<RDFPropertyValuePair> propertyIterator = valueResource.getPropertyIterator(); //get an iterator to all the element properties
					while(propertyIterator.hasNext()) { //while there are more properties
						final RDFPropertyValuePair subPropertyValuePair = propertyIterator.next(); //get the next property name/value pair
						final RDFResource subPropertyResource = subPropertyValuePair.getProperty(); //get the property resource
						final RDFObject subPropertyValue = subPropertyValuePair.getPropertyValue(); //get the property value
						final URI subPropertyResourceURI = subPropertyResource.getURI(); //get the URI of the subproperty
						final URI subPropertyNamespaceURI = getNamespaceURI(subPropertyResourceURI); //get the namespace URI of the property
						assert subPropertyNamespaceURI != null : "Unable to find namespace of subproperty " + subPropertyResourceURI; //TODO add real error handling here
						//if this value is not a plain literal, or the plain literal has a language indication, we can't store it in an attribute
						if(!(subPropertyValue instanceof RDFPlainLiteral) || ((RDFPlainLiteral)subPropertyValue).getLanguage() != null) {
							serializeSubPropertyLiteralAttributes = false; //show that all subproperties are not literals
							break; //stop looking for a non-literal
						} else if(!isLiteralAttributeSerialization()) { //if we shouldn't serialize all literal property values as attributes
							if(subPropertyNamespaceURI != null && getLocalName(subPropertyResourceURI) != null) { //TODO remove this check because of new method for determining namespace URI and local name
								if(!isLiteralAttributeSerializationNamespaceURI(subPropertyNamespaceURI)) { //if we should serialize literal property values from this namespace
									serializeSubPropertyLiteralAttributes = false; //show that all subproperties are not literals, because we shouldn't serialize this literal as an attribute
								}
							} else
								//we couldn't check to see if we should serialize literals as attributes from this namespace; assume we can't
								serializeSubPropertyLiteralAttributes = false; //show that all subproperties are not literals, because we shouldn't serialize this one as an attribute
						}
					}
					if(serializeSubPropertyLiteralAttributes) { //if all subproperties are literals, then just add them as attributes
						setSerialized(valueResource, true); //show that we've serialized this resource (do this before adding properties, just in case there are circular references)
						addProperties(document, propertyElement, valueResource, getType(valueResource)); //add all the properties of the value resource to the element representing the property TODO check getType(); is it correct? do we even need it logically in this context?
					} else { //if not all subproperties of the anonymous element are literals, add the value resource as a subelement of the property element
						final Element anonymousResourceElement = createResourceElement(document, valueResource); //create an element from this resource, which will mark the resource as serialized
						propertyElement.appendChild(anonymousResourceElement); //add the anonymous resource element to the property element
					}
				} else { //if we've already serialized the property value resource or the resource is referenced by a list
					setReference(propertyElement, valueResource); //make the property XML element reference the resource rather than including the resource inline
				}
			}
		} else if(propertyValue instanceof RDFLiteral) { //if the value is a literal
			final RDFLiteral valueLiteral = (RDFLiteral)propertyValue; //cast the value to a literal
			//store the value
			if(propertyValue instanceof RDFXMLLiteral) { //special-case XML literals because they have a special XML serialization syntax---that of XML itself
				final RDFXMLLiteral valueXMLLiteral = (RDFXMLLiteral)valueLiteral; //cast the literal to an XML literal
				//add an rdf:parseType="Literal" attribute
				propertyElement.setAttributeNS(RDF_NAMESPACE_URI.toString(), createQName(RDF_NAMESPACE_PREFIX, ATTRIBUTE_PARSE_TYPE), LITERAL_PARSE_TYPE);
				//import the document fragment to our document
				final Node importedDocumentFragment = document.importNode(valueXMLLiteral.getValue(), true);
				propertyElement.appendChild(importedDocumentFragment); //append the children of the document fragment to this node
				//make sure all namespaces are properly declared for all child elements, declaring the namespaces on the property element itself if possible
				XML.ensureChildNamespaceDeclarations(propertyElement);
			} else { //for all other XML literals, just store their lexical form as a text child to the property element
				appendText(propertyElement, valueLiteral.getLexicalForm()); //append the literal value as content of the property element
				if(valueLiteral instanceof RDFPlainLiteral) { //if this is a plain literal
					final RDFPlainLiteral valuePlainLiteral = (RDFPlainLiteral)valueLiteral; //cast the literal to a plain literal
					if(valuePlainLiteral.getLanguage() != null) { //if there is a language indication
						final String languageTag = Locales.getLanguageTag(valuePlainLiteral.getLanguage()); //create a language tag from the locale
						//store the language tag in the xml:lang attribute
						propertyElement.setAttributeNS(XML.XML_NAMESPACE_URI.toString(), createQName(XML.XML_NAMESPACE_PREFIX, XML.ATTRIBUTE_LANG), languageTag);
					}
				} else if(valueLiteral instanceof RDFTypedLiteral) { //if this is a typed literal
					final RDFTypedLiteral<?> valueTypedLiteral = (RDFTypedLiteral<?>)valueLiteral; //cast the literal to a typed literal
					//store the datatype reference URI in the rdf:datatype attribute
					propertyElement.setAttributeNS(RDF_NAMESPACE_URI.toString(), createQName(RDF_NAMESPACE_PREFIX, ATTRIBUTE_DATATYPE), valueTypedLiteral
							.getDatatypeURI().toString());
				}
			}
		}
		return propertyElement; //return the property element we constructed
	}

	/**
	 * Retrieves the prefix to use for the given namespace. If a namespace is unrecognized, a new one will be created and stored for future use.
	 * @param namespaceURI The namespace URI for which a prefix should be returned
	 * @return A prefix for use with the given namespace.
	 */
	/*G***del when works
		protected String getPrefix(final String namespaceURI)
		{
			String prefix=(String)namespacePrefixMap.get(namespaceURI);  //get the prefix keyed by the namespace
			if(prefix==null) {	//if there is no prefix for this namespace

			}

			//G***fix to detect unknown namespaces and assign new prefixes
		  return (String)namespacePrefixMap.get(namespaceURI);  //return a prefix keyed by the namespace
		}
	*/

	/**
	 * Retrieves a label appropriate for the reference URI of the resource. If the resource has namespace URI and local name, the XML qualified name will be
	 * returned in <em>namespaceURI</em>:<em>localName</em> format; otherwise, reference URI itself will be returned.
	 * @param resource The resource the label of which to return.
	 * @return A label representing the reference URI of the resource.
	 */
	/*G**del if not needed
		public String getLabel(final RDFResource resource)
		{
			return getLabel(resource, true);	//get 
		}
	*/

	/**
	 * Creates an RDF name for the given reference URI, taking into account registered namespaces namespaces.
	 * @param referenceURI The reference URI for which an RDF name should be determined.
	 * @return The RDF name for the reference URI, or <code>null</code> if the namespace URI and local name could not be determined.
	 * @throws NullPointerException if the given reference URI is <code>null</code>.
	 */
	public RDFName getRDFName(final URI referenceURI) {
		RDFName rdfName = RDFName.getRDFName(referenceURI); //get an RDF name from the reference URI		  
		if(rdfName == null) { //if there is no namespace URI known, see if a namespace URI is registered that matches the beginning of this reference URI
			//TODO right now we iterate through the namespaces each time; there might be a better way to do this
			final String referenceURIString = referenceURI.toString(); //get a string version of the reference URI to use in comparisons
			for(final String registeredNamespace : xmlNamespacePrefixManager.getRegisteredNamespaces()) { //for all the registered namespaces
				if(referenceURIString.startsWith(registeredNamespace)) { //if this reference URI is in this namespace
					rdfName = new RDFName(URI.create(registeredNamespace), referenceURIString.substring(registeredNamespace.length())); //user the registered namespace URI, and remove the namespace URI from the front of the reference URI to get the local name
					break; //stop looking for a matching namespace
				}
			}
		}
		return rdfName; //return the RDF name we determined, if any
	}

	/**
	 * Retrieves a label appropriate for the given reference URI. If the reference URI has namespace URI and local name, the XML qualified name will be returned
	 * in <var>namespaceURI</var>:<var>localName</var> format; otherwise, the reference URI itself will be returned.
	 * @param referenceURI The reference URI for which a label should be determined.
	 * @return A label representing the given reference URI.
	 * @throws NullPointerException if the given reference URI is <code>null</code>.
	 */
	public String getLabel(final URI referenceURI) {
		final RDFName rdfName = getRDFName(referenceURI); //get an RDF name from the reference URI if we can, taking into account the registered namespaces
		if(rdfName != null) { //if we've determined a namespace URI and a local name
			final URI namespaceURI = rdfName.getNamespaceURI(); //get the namespace URI we determined
			//get the prefix from the to be used for this namespace, but don't generate a new prefix if we don't recognize it
			final String prefix = xmlNamespacePrefixManager.getNamespacePrefix(namespaceURI.toString(), false);
			if(prefix != null) { //if we know a prefix for this namespace URI
				String localName = rdfName.getLocalName(); //get the local name we determined
				if(RDF_NAMESPACE_URI.equals(namespaceURI)) { //if this is the RDF namespace
					if(localName.startsWith(CONTAINER_MEMBER_PREFIX)) //if this is one of the rdf:li_XXX members
						localName = LI_PROPERTY_NAME; //just use the normal rdf:li property name---the order is implicit in the serialization
				}
				return createQName(prefix, localName); //create an XML qualified name for this namespace prefix and local name
			}
		}
		return referenceURI.toString(); //just use the reference URI as the label, if we can't find anything else TODO we might want to check for rdf:li_ here as well; maybe not
	}

}