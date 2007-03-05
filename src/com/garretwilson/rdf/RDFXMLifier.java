package com.garretwilson.rdf;

import java.net.URI;
import java.util.*;

import static com.garretwilson.rdf.RDFConstants.*;
import static com.garretwilson.rdf.RDFUtilities.*;
import static com.garretwilson.rdf.RDFXMLConstants.*;
import com.garretwilson.text.xml.XMLConstants;
import com.garretwilson.text.xml.XMLNamespaceProcessor;
import com.garretwilson.text.xml.XMLUtilities;
import com.garretwilson.text.xml.XMLSerializer;
import com.garretwilson.util.CollectionUtilities;
import com.garretwilson.util.Debug;
import com.garretwilson.util.IdentityHashSet;
import com.garretwilson.util.LocaleUtilities;
import org.w3c.dom.*;

/**Class that creates an XML representation of RDF through DOM.
<p>This implementation resets the node ID generator between each XML generation,
	which means that use of the same instance of this class is not appropriate
	for multiple generations to the same RDF instance XML document or
	fragment.</p>
TODO fix bug that doesn't serialize property value resources with no properties
@author Garret Wilson
*/
public class RDFXMLifier	//TODO why don't we keep the DOM implementation around at the class level?
{

//TODO why aren't we using real URIs here?
	
	/**The map of prefixes, keyed by namespace URIs.*/
	private final Map<String, String> namespacePrefixMap;

		/**@return The map of prefixes, keyed by namespace URI.*/
		protected Map<String, String> getNamespacePrefixMap() {return namespacePrefixMap;}
		
		/**Registers the given prefix to be used with the given namespace URI.
			If a prefix is already registered with the given namespace, it is
			replaced with this prefix.
		@param namespaceURI The XML namespace.
		@param prefix The serialization prefix to use with the given namespace.
		*/
		public void registerNamespacePrefix(final String namespaceURI, final String prefix)
		{
			namespacePrefixMap.put(namespaceURI, prefix);	//store the prefix in the map, keyed to the URI
		}

		/**Unregisters the prefix for the given namespace URI.
			If no prefix is registered for the given namespace, no action occurs.
		@param namespaceURI The XML namespace.
		*/
		public void unregisterNamespacePrefix(final String namespaceURI, final String prefix)
		{
			namespacePrefixMap.remove(namespaceURI);	//remove whatever prefix is registered with this namespace, if any
		}

	/**The set of resources that have been serialized, using identity rather 
		than equality for equivalence, as required be serialization.
	*/
	private final Set<RDFResource> serializedResourceSet;

		/**Determines if the given resource has been serialized.
		@param resource The resource to check.
		@return <code>true</code> if the resource has already been serialized, else
			<code>false</code>.
		*/
		protected final boolean isSerialized(final RDFResource resource)
		{
			return serializedResourceSet.contains(resource);	//see if the resource is in our set
		}

		/**Sets the serialized status of a given resource.
		@param resource The resource for which the serialization status should be
			set.
		@param serialized <code>true</code> if the resource has been serialized,
			or <code>false</code> if not.
		*/
		protected final void setSerialized(final RDFResource resource, final boolean serialized)
		{
			if(serialized)	//if the resource has been serialized
			{
				serializedResourceSet.add(resource);	//add the resource to the set of serialized resources
			}
			else	//if the resource has not been serialized
			{
				serializedResourceSet.remove(resource);	//remove the resource from the set of serialized resources
			} 
		}

	/**A map that associates, for each resource, a set of all resources that
		reference the that resource, using identity rather than equality for
		equivalence.*/
	private final Map<RDFResource, Set<RDFResource>> resourceReferenceMap;

		/**Returns the set of resources that reference this resource as already
			calculated.
		@param resource The resource for which references should be returned.
		@return A set of all references to the resource that have been gathered
			at an earlier time, or <code>null</code> if no references have been
			gathered for the given resource.
		*/
		protected Set<RDFResource> getReferenceSet(final RDFResource resource)
		{
			return resourceReferenceMap.get(resource);	//get the set of references, if any, associated with the resource
		}

	/**A map of node ID strings keyed to the resource they represent, using
		identity rather than equality for equivalence for comparing resources.
	*/
	private final Map<RDFResource, String> nodeIDMap;

		/**Retrieves a node ID appropriate for the given resource. If the resource
			has already been assigned a node ID, it will be returned; otherwise, a
			new node ID will be generated.
		@param resource The resource for which a node ID should be returned
		@return
		*/
		protected String getNodeID(final RDFResource resource)
		{
			String nodeID=nodeIDMap.get(resource);	//get a node ID for the given resource
			if(nodeID==null)	//if there is no node ID for this resource
			{
				nodeID="node"+serializedResourceSet.size()+1;	//generate a node ID for the resource, based upon the number of resources already serialized
				nodeIDMap.put(resource, nodeID);	//associate the node ID with the resource
			}
			return nodeID;	//return the retrieved or generated node ID
		} 

	/**Whether we're in the mode that allows us to put off serialization of some
		resources until the very end.
	*/
//G***fix	private boolean isSerializationDeferable=true;

	/**Indicates literals should be serialized as attributes by default.*/
//G***del	public final static boolean LITERAL_ATTRIBUTE_SERIALIZATION=1;
	/**Indicates literals should be serialized as elements by default.*/
//G***del	public final static boolean LITERAL_ELEMENT_SERIALIZATION=2;

	/**Whether literals are serialized as attributes, if possible.*/
	private boolean literalAttributeSerialization=false;

		/**@return <code>true</code> if literals are serialized as attributes, if
		  possible.
		*/
		public boolean isLiteralAttributeSerialization() {return literalAttributeSerialization;}

		/**Sets whether literal property values are serialized as attributes.
		@param newLiteralAttributeSerialization <code>true</code> if literals should
			be serialized as attributes if possible.
		*/
		public void setLiteralAttributeSerialization(final boolean newLiteralAttributeSerialization) {literalAttributeSerialization=newLiteralAttributeSerialization;}

	/**A set of namespace URIs the properties
		in which should have literal values serialized as attributes rather than
		elements.
	*/
	private final Set<URI> literalAttributeSerializationNamespaceSet=new HashSet<URI>();

	/**Adds a namespace URI the properties in which namespace should be
		categorically serialized as attributes if the property value is a literal.
		For attributes added by this method, the value of
		<code>isLiteralAttributeSerialization()</code> is ignored.
	@param namespaceURI The URI of the namespace that should have its literal
		property values serialized as attributes.
	*/
	public void addLiteralAttributeSerializationNamespaceURI(final URI namespaceURI)
	{
		literalAttributeSerializationNamespaceSet.add(namespaceURI);  //add the namespace to the set
	}

	/**Determines whether literal property values from a given namespace should
		be serialized as attributes.
	@param namespaceURI The URI of the namespace in question
	@return <code>true</code> if the literal property values from the given
		namespace should be serialized as attributes.
	@see #isLiteralAttributeSerialization
	*/
	public boolean isLiteralAttributeSerializationNamespaceURI(final URI namespaceURI)
	{
//G***del		return isLiteralAttributeSerialization()  //if all literal property values should be serialized, return true
		return literalAttributeSerializationNamespaceSet.contains(namespaceURI); //if this namespace is in the set, return true
	}

/*G***fix
	public void isLiteralAttributeSerializationNamespaceURI(final String namespaceURI, final String localName)
	{

	}
*/

	/**Default constructor that creates a compact XMLifier.*/
	public RDFXMLifier()
	{
		this(true); //construct a compact XMLifier
	}

	/**Compact constructor.
	@param compact Whether the XML representation should be as compact as possible;
		if <code>false</code>, literals will by default be represented as child
		elements instead of attributes.
	@see #setLiteralAttributeSerialization
	*/
	public RDFXMLifier(final boolean compact)
	{
		namespacePrefixMap=XMLSerializer.createNamespacePrefixMap();  //create a map of default XML namespace prefixes
		serializedResourceSet=new IdentityHashSet<RDFResource>();	//create a map that will determine whether resources have been serialized, based upon the identity of resources
		resourceReferenceMap=new IdentityHashMap<RDFResource, Set<RDFResource>>();	//create a map of sets of referring resources for each referant resource, using identity rather than equality for equivalence
		nodeIDMap=new IdentityHashMap<RDFResource, String>();	//create a map of node IDs keyed to resources, using identity rather than equality to determine associated resource
		setLiteralAttributeSerialization(compact);  //if we should be compact, show literals as attributes
	}

	/**Releases memory by clearing all internal maps and sets of resources.*/ 
	protected void reset()
	{
		serializedResourceSet.clear();	//show that we've not serialized any resources
		resourceReferenceMap.clear();	//clear all our references to resources
		nodeIDMap.clear();	//clear our map of node IDs
	}

	/**Creates a default RDF XML document.
	@param domImplementation The DOM implementation to use.
	@return A newly created default RDF XML document with an RDF section.
	*/
	public static Document createDefaultRDFDocument(final DOMImplementation domImplementation)
	{
		  //create an RDF document
		final Document document=domImplementation.createDocument(RDF_NAMESPACE_URI.toString(), XMLUtilities.createQualifiedName(RDF_NAMESPACE_PREFIX, ELEMENT_RDF), null);
		  //add the RDF namespace declaration prefix, xmlns:rdf
		document.getDocumentElement().setAttributeNS(XMLConstants.XMLNS_NAMESPACE_URI.toString(), XMLUtilities.createQualifiedName(XMLConstants.XMLNS_NAMESPACE_PREFIX, RDFConstants.RDF_NAMESPACE_PREFIX), RDFConstants.RDF_NAMESPACE_URI.toString());
		return document;  //return the document we created
	}

	/**Creates an XML document with <code>&lt;rdf:RDF&gt;</code> as the document
		element, and creates elements representing the given RDF data model as
		children of the that element.
	@param rdf The RDF data model to XMLify.
	@param domImplementation The DOM implementation to use.
	@return The XML document tree that represents the given RDF data model.
	*/
	public Document createDocument(final RDF rdf, final DOMImplementation domImplementation)
	{
		reset();	//make sure we don't have information left over from other calls to this method
		rdf.getReferences(resourceReferenceMap);	//get all the references to resources in the RDF data model
		try
		{
			final Document document=createDefaultRDFDocument(domImplementation);  //create a default RDF document
			final Element rootElement=document.getDocumentElement();  //get the document element
			createElements(rdf, rootElement); //XMLify the resource as elements under the document element
			return document;  //return the document we created
		}
		finally
		{
			reset();	//clean up by releasing all local references to resources
		}
	}

	/**Creates an XML document with <code>&lt;rdf:RDF&gt;</code> as the document
		element, and creates an element representing the given resource.
	@param resource The resource to store as an XML element, or <code>null</code>
		if all non-anonymous resources should be stored.
	@param domImplementation The DOM implementation to use.
	@return The XML document tree that represents the given RDF resource.
	*/
	public Document createDocument(final RDFResource resource, final DOMImplementation domImplementation)
	{
		reset();	//make sure we don't have information left over from other calls to this method
		RDF.getReferences(resource, resourceReferenceMap);	//get all the references to the given resource
		try
		{
			final Document document=createDefaultRDFDocument(domImplementation);  //create a default RDF document
			final Element rootElement=document.getDocumentElement();  //get the document element
			final Element resourceElement=createResourceElement(document, resource); //create an element from this resource
			rootElement.appendChild(resourceElement); //add the resource element to the root element
			return document;  //return the document we created
		}
		finally
		{
			reset();	//clean up by releasing all local references to resources
		}
	}

	/**Creates the <rdf:RDF> element needed in an XML serialization of RDF.
	@param document The document to use as an element factory for the element.
	@return An <rdf:RDF> element for the given document.
	*/
	public static Element createRDFElement(final Document document)
	{
		return document.createElementNS(RDF_NAMESPACE_URI.toString(), XMLUtilities.createQualifiedName(RDF_NAMESPACE_PREFIX, ELEMENT_RDF));	//create an <rdf:RDF> element for the given document
	}

	/**Creates an <code>&lt;rdf:RDF&gt;</code> XML element and creates elements
		representing the given RDF data model as children of the that element.
	@param rdf The RDF data model to XMLify.
	@param document The XML document to use as an element factory.
	@return The RDF element created.
	*/
	public Element createElement(final RDF rdf, final Document document)
	{
		reset();	//make sure we don't have information left over from other calls to this method
		rdf.getReferences(resourceReferenceMap);	//get all the references to resources in the RDF data model
		try
		{
				//create an <rdf:RDF> element
			final Element rdfElement=document.createElementNS(RDF_NAMESPACE_URI.toString(), XMLUtilities.createQualifiedName(RDF_NAMESPACE_PREFIX, ELEMENT_RDF));
			createElements(rdf, rdfElement); //XMLify the resourcs as elements under the RDF element
			return rdfElement;  //return the element we constructed with RDF resources as child elements
		}
		finally
		{
			reset();	//clean up by releasing all local references to resources
		}
	}

	/**Creates elements representing the given RDF data model as children of the
		given parent element.
	@param rdf The RDF data model to XMLify.
	@param parentElement The parent XML element of the elements to be added.
	*/
	protected void createElements(final RDF rdf, final Element parentElement)
	{
		for(final RDFResource resource:rdf.getRootResources())  //for each root RDF resource
		{
			final Element resourceElement=createResourceElement(parentElement.getOwnerDocument(), resource); //create an element from this resource
			parentElement.appendChild(resourceElement); //add the resource element to our parent element
		}
	}

	/**Creates an XML element to represent the given resource.
	@param document The document to be used as an element factory.
	@param resource The resource for which an element should be created.
	@return The XML element that represents the given RDF resource.
	*/
	protected Element createResourceElement(final Document document, final RDFResource resource)
	{
		final URI referenceURI=resource.getReferenceURI(); //get the reference URI of the resource
		final Element resourceElement;  //we'll store here the element that we create
		RDFResource resourceType=getType(resource); //get the type of the resource
		if(resourceType!=null)   //if this resource has a type that we can use for the element name
		{
			final URI resourceTypeURI=resourceType.getReferenceURI();	//get the URI of the type
			final URI namespaceURI=getNamespaceURI(resourceTypeURI);	//get the type's namespace URI
//TODO important: check for namespace URI null
			final String prefix=XMLSerializer.getNamespacePrefix(getNamespacePrefixMap(), namespaceURI.toString());  //get the prefix for use with this namespace
			final String qualifiedName=XMLUtilities.createQualifiedName(prefix, getLocalName(resourceTypeURI));  //create a qualified name for the element
				//TODO check for null on local name
			resourceElement=document.createElementNS(namespaceURI.toString(), qualifiedName);  //create an element from the resource type
		}
		else  //if we can't create an element from its type
		{
			final String qualifiedName=XMLUtilities.createQualifiedName(RDF_NAMESPACE_PREFIX, ELEMENT_DESCRIPTION);  //create a qualified name for the element
		  resourceElement=document.createElementNS(RDF_NAMESPACE_URI.toString(), qualifiedName); //create an rdf:Description element
		}
		if(referenceURI!=null)  //if this resource has a reference URI (i.e. it isn't a blank node)
		{
				//set the rdf:about attribute to the reference URI
			final String aboutAttributeQualifiedName=XMLUtilities.createQualifiedName(RDF_NAMESPACE_PREFIX, ATTRIBUTE_ABOUT);  //create a qualified name for reference URI
				//add the rdf:about attribute with the value set to the reference URI of the resource
			resourceElement.setAttributeNS(RDF_NAMESPACE_URI.toString(), aboutAttributeQualifiedName, referenceURI.toString());
		}
		else	//if this resource has no reference URI, we'll have to give it a node ID if any other resources reference it
		{
			final Set referenceSet=getReferenceSet(resource);	//find out which resources reference this resource
			if(referenceSet!=null && referenceSet.size()>1)	//if more than one resource references this resource (if there's only one reference to it, we'll just serialize it inline)
			{
				final String nodeID=getNodeID(resource);	//get the node ID for this resource
					//set the rdf:nodeID attribute to the node ID
				final String nodeIDAttributeQualifiedName=XMLUtilities.createQualifiedName(RDF_NAMESPACE_PREFIX, ATTRIBUTE_NODE_ID);  //create a qualified name for node ID
					//add the rdf:nodeID attribute with the value set to the node ID of the resource
				resourceElement.setAttributeNS(RDF_NAMESPACE_URI.toString(), nodeIDAttributeQualifiedName, nodeID);
			}
		}
		if(!isSerialized(resource))	//if we haven't serialized this resource, yet
		{
			setSerialized(resource, true);	//show that we've serialized this resource (do this before adding properties, just in case there are circular references)
			addProperties(document, resourceElement, resource);  //add all the properties of the resource to the resource element
		}
		return resourceElement; //return the resource element we created
	}

	/**Sets the appropriate attributes of the given element so that the element
		becomes a reference to the indicated resource. This will either be
		<code>rdf:resource</code> if the resource has a reference URI, or
		<code>rdf:nodeID</code> if the resource has no reference URI.
	@param element The XML element which should indicate a reference to the
		resource.
	@param resource The resource this element should reference.
	*/
	protected void setReference(final Element element, final RDFResource resource)
	{
		final URI referenceURI=resource.getReferenceURI(); //get the reference URI of the resource
		if(referenceURI!=null)	//if the resource has a reference URI, reference the reference URI using rdf:resource
		{
			final String resourceAttributeQualifiedName=XMLUtilities.createQualifiedName(RDF_NAMESPACE_PREFIX, ATTRIBUTE_RESOURCE);  //create a qualified name for the link attribute
				//add the rdf:resource attribute with the value set to the reference URI of the property value resource
			element.setAttributeNS(RDF_NAMESPACE_URI.toString(), resourceAttributeQualifiedName, referenceURI.toString());
		}
		else	//if the object resource has no reference URI, we'll reference it using rdf:nodeID
		{
			final String nodeID=getNodeID(resource);	//get the node ID for this resource
				//set the rdf:nodeID attribute to the node ID
			final String nodeIDAttributeQualifiedName=XMLUtilities.createQualifiedName(RDF_NAMESPACE_PREFIX, ATTRIBUTE_NODE_ID);  //create a qualified name for node ID
				//add the rdf:nodeID attribute with the value set to the node ID of the resource
			element.setAttributeNS(RDF_NAMESPACE_URI.toString(), nodeIDAttributeQualifiedName, nodeID);
		}
	}


	/**Adds all properties of the resource to the given element.
	@param document The document to be used as an element factory.
	@param element The XML element to which the properties should be added
	@param resource The resource the properties of which should be added to the
		DOM tree, either as elements or attributes.
	*/
	protected void addProperties(final Document document, final Element element, final RDFResource resource)
	{
		RDFResource resourceType=getType(resource); //get the type of the resource G***do we want this in the general routine? we probably have to have it
		final Iterator propertyIterator=resource.getPropertyIterator(); //get an iterator to all the element properties
		while(propertyIterator.hasNext()) //while there are more properties
		{
			final RDFPropertyValuePair rdfPropertyValuePair=(RDFPropertyValuePair)propertyIterator.next(); //get the next property name/value pair
		  final RDFResource propertyResource=rdfPropertyValuePair.getProperty();  //get the property predicate
			final RDFObject propertyValue=rdfPropertyValuePair.getPropertyValue();  //get the property value
				//if this property is not a type property we already used for creating the element name
		  if(!(TYPE_PROPERTY_REFERENCE_URI.equals(propertyResource.getReferenceURI()) && propertyValue.equals(resourceType)))
			{
				final URI propertyNamespaceURI=getNamespaceURI(propertyResource.getReferenceURI()); //get the namespace URI of the property
				assert propertyNamespaceURI!=null : "Missing preperty namespace.";	//TODO add real error handling here
				boolean serializeLiteralAttribute=false; //start out by assuming we won't serialize as an attribute
				  //see if we should use an attribute to serialize a plain literal TODO add support for typed literals
				if(propertyValue instanceof RDFPlainLiteral)	//if this is a plain literal
				{
						//if we should serialize all literal property values as attributes, or if we should serialize literal property values from this namespace 
					if(isLiteralAttributeSerialization() || isLiteralAttributeSerializationNamespaceURI(propertyNamespaceURI))
					{
						final RDFPlainLiteral plainLiteral=(RDFPlainLiteral)propertyValue;	//cast the value to a plain literal
						if(plainLiteral.getLanguage()==null)	//if there is no language definition
							serializeLiteralAttribute=true;  //show that we should use an attribute for serialization
					}
				}
				if(serializeLiteralAttribute)  //if we should use an attribute for serializing this property value
				{
					final String prefix=XMLSerializer.getNamespacePrefix(getNamespacePrefixMap(), propertyNamespaceURI.toString());  //get the prefix for use with this namespace
					final String qualifiedName=XMLUtilities.createQualifiedName(prefix, getLocalName(propertyResource.getReferenceURI()));  //create a qualified name for the attribute
						//TODO check for null on local name
						//store the property as an attribute
				  element.setAttributeNS(propertyNamespaceURI.toString(), qualifiedName, ((RDFLiteral)propertyValue).getLexicalForm());
				}
				else  //if the value is a resource or we don't know the property's namespace URI and local name G***add more logic here later to automatically determine the namespace
				{
					final Element propertyElement=createPropertyElement(document, propertyResource, propertyValue); //create a property element
					element.appendChild(propertyElement); //append the property element we created
				}
			}
		}
	}

	/**Creates an XML element to represent the given property.
		All anonymous	property value resources will be created as child elements.
		If an anonymous property value's properties are all literals, that property
		resource value's properties will be created as attributes of the property.
	@param document The document to be used as an element factory.
	@param propertyResource The property predicate for which an element should be
		created.
	@param propertyValue The property value for which an element should be created.
	@return The XML element that represents the given RDF property.
	*/  //G***del rdf if we don't need
	protected Element createPropertyElement(final Document document, final RDFResource propertyResource, final RDFObject propertyValue)
	{
//G***del Debug.trace("creating property element for property: ", propertyResource);  //G***del
//G***del		final URI namespaceURI; //we'll store here the namespace URI of the property element
		final String qualifiedName; //we'll store here the qualified name of the property element
			//if we know the namespace URI and local name of the property
				  //TODO replace all this code with the new utility method getLabel(RDFResource)
		final URI propertyResourceURI=propertyResource.getReferenceURI();	//get the URI of the property
		final URI namespaceURI=getNamespaceURI(propertyResourceURI); //get the namespace URI of the property
//TODO important: check the namespace URI for null
		final String prefix=XMLSerializer.getNamespacePrefix(getNamespacePrefixMap(), namespaceURI.toString());  //get the prefix for use with this namespace
		String localName=getLocalName(propertyResourceURI); //get the local name of the property
			//TODO check for null on local name
		if(RDF_NAMESPACE_URI.equals(namespaceURI))  //if this is the RDF namespace
		{
			if(localName.startsWith(CONTAINER_MEMBER_PREFIX)) //if this is one of the rdf:li_XXX members
				localName=LI_PROPERTY_NAME; //just use the normal rdf:li property name---the order is implicit in the serialization
		}
		qualifiedName=XMLUtilities.createQualifiedName(prefix, localName);  //create a qualified name for the element
		final Element propertyElement=document.createElementNS(namespaceURI.toString(), qualifiedName);  //create an element from the property resource
		if(propertyValue instanceof RDFResource) //if the property value is a resource
		{
			if(propertyValue instanceof RDFListResource)	//if this is a list G***maybe make sure it's a normal list, first, before creating the compact form
			{
					//set the rdf:parseType attribute to "Collection" TODO eventually get our prefix from a prefix rather than hard-coding RDF, maybe
				propertyElement.setAttributeNS(RDF_NAMESPACE_URI.toString(), XMLUtilities.createQualifiedName(RDF_NAMESPACE_PREFIX.toString(), ATTRIBUTE_PARSE_TYPE), COLLECTION_PARSE_TYPE);
				final RDFListResource propertyListResource=(RDFListResource)propertyValue;	//cast the resource to a list
				final Iterator<RDFResource> iterator=propertyListResource.iterator();	//get an iterator to look at the list elements
				while(iterator.hasNext())	//while there are more elements
				{
					final RDFResource elementResource=(RDFResource)iterator.next();	//get the next element of the list
					final Element elementResourceElement=createResourceElement(document, elementResource); //create an element from this list element resource
					propertyElement.appendChild(elementResourceElement); //add the list element resource element resource to the property element
				}
			}
			else	//if this is a normal property resource value
			{
				final RDFResource valueResource=(RDFResource)propertyValue; //cast the value to a resource
				final Set<RDFResource> referenceSet=getReferenceSet(valueResource);	//find out which resources reference this property value
//G***if we're only serializing a single resource tree, the commented-out code on this line will result in missing nodes; maybe allow a setFlatSerialization option later, but that may be too flat---maybe have a setSortOfFlat()...				if(valueResource.getReferenceURI()==null) //if this is a blank node
	/*G***fix or delete
				if(!isSerialized(valueResource))	//if we haven't serialized the property value resource, yet
				{
						//if more than one resource references this resource, don't try to
						//	choose which one gets an inline reference---defer serialization
						//	until later
					if(referenceSet!=null && referenceSet.size()>1 && isSerializationDeferable())	//if more than one resource references this resource (i.e. more than one property has this resource for a value) and we can defer serialization
					{
						deferSerialization(valueResource);	//defer serialization until later
					}
*/
					//see if we should serialize inline; we'll do so if all the following are true
					//	1. the resource is not yet serialized
					//	2. the resource is not referenced by a list elsewhere (always defer to the list for inline serialization)
					//	3. the resource has at least one property
					//if all the conditions are not met, we'll only reference the resource
				if(!isSerialized(valueResource)	//if we haven't serialized the property value resource, yet
						&& (referenceSet==null || !CollectionUtilities.containsInstance(referenceSet, RDFListResource.class))	//if this resource is not referenced by a list
						&& valueResource.getPropertyCount()>0)	//if this resource has at least one property 
				{
					boolean serializeSubPropertyLiteralAttributes=true; //we'll see if all the subproperties are plain literals without language indications; if so, we'll just add them as attributes
					final Iterator propertyIterator=valueResource.getPropertyIterator(); //get an iterator to all the element properties
					while(propertyIterator.hasNext()) //while there are more properties
					{
						final RDFPropertyValuePair subPropertyValuePair=(RDFPropertyValuePair)propertyIterator.next(); //get the next property name/value pair
						final RDFResource subPropertyResource=subPropertyValuePair.getProperty();  //get the property resource
						final RDFObject subPropertyValue=subPropertyValuePair.getPropertyValue();  //get the property value
						final URI subPropertyResourceURI=subPropertyResource.getReferenceURI();	//get the URI of the subproperty
					  final URI subPropertyNamespaceURI=getNamespaceURI(subPropertyResourceURI); //get the namespace URI of the property
					  	//TODO make sure null is handled for the namespace URI
								//if this value is not a plain literal, or the plain literal has a language indication, we can't store it in an attribute
						if(!(subPropertyValue instanceof RDFPlainLiteral) || ((RDFPlainLiteral)subPropertyValue).getLanguage()!=null) 
						{
							serializeSubPropertyLiteralAttributes=false; //show that all subproperties are not literals
							break;  //stop looking for a non-literal
						}
						else if(!isLiteralAttributeSerialization()) //if we shouldn't serialize all literal property values as attributes
						{
							if(subPropertyNamespaceURI!=null && getLocalName(subPropertyResourceURI)!=null)	//TODO remove this check because of new method for determining namespace URI and local name
							{
								if(!isLiteralAttributeSerializationNamespaceURI(subPropertyNamespaceURI)) //if we should serialize literal property values from this namespace
								{
									serializeSubPropertyLiteralAttributes=false; //show that all subproperties are not literals, because we shouldn't serialize this literal as an attribute
								}
							}
							else  //we couldn't check to see if we should serialize literals as attributes from this namespace; assume we can't
								serializeSubPropertyLiteralAttributes=false; //show that all subproperties are not literals, because we shouldn't serialize this one as an attribute
						}
					}
					if(serializeSubPropertyLiteralAttributes)  //if all subproperties are literals, then just add them as attributes
					{
						setSerialized(valueResource, true);	//show that we've serialized this resource (do this before adding properties, just in case there are circular references)
					  addProperties(document, propertyElement, valueResource);  //add all the properties of the value resource to the element representing the property
					}
					else  //if not all subproperties of the anonymous element are literals, add the value resource as a subelement of the property element
				  {
						final Element anonymousResourceElement=createResourceElement(document, valueResource); //create an element from this resource, which will mark the resource as serialized
						propertyElement.appendChild(anonymousResourceElement); //add the anonymous resource element to the property element
				  }
				}
				else  //if we've already serialized the property value resource or the resource is referenced by a list
				{
					setReference(propertyElement, valueResource);	//make the property XML element reference the resource rather than including the resource inline
				}
			}
		}
		else if(propertyValue instanceof RDFLiteral)  //if the value is a literal
		{
			final RDFLiteral valueLiteral=(RDFLiteral)propertyValue;  //cast the value to a literal
				//store the value
			if(propertyValue instanceof RDFXMLLiteral)	//special-case XML literals because they have a special XML serialization syntax---that of XML itself
			{
				final RDFXMLLiteral valueXMLLiteral=(RDFXMLLiteral)valueLiteral;	//cast the literal to an XML literal
					//add an rdf:parseType="Literal" attribute
				propertyElement.setAttributeNS(RDF_NAMESPACE_URI.toString(), XMLUtilities.createQualifiedName(RDF_NAMESPACE_PREFIX, ATTRIBUTE_PARSE_TYPE), LITERAL_PARSE_TYPE);
					//import the document fragment to our document
				final Node importedDocumentFragment=document.importNode(valueXMLLiteral.getValue(), true);
				propertyElement.appendChild(importedDocumentFragment);	//append the children of the document fragment to this node
					//make sure all namespaces are properly declared for all child elements, declaring the namespaces on the property element itself if possible
				XMLNamespaceProcessor.ensureChildNamespaceDeclarations(propertyElement); 
			}
			else	//for all other XML literals, just store their lexical form as a text child to the property element
			{
				XMLUtilities.appendText(propertyElement, valueLiteral.getLexicalForm());  //append the literal value as content of the property element
				if(valueLiteral instanceof RDFPlainLiteral)	//if this is a plain literal
				{
					final RDFPlainLiteral valuePlainLiteral=(RDFPlainLiteral)valueLiteral;	//cast the literal to a plain literal
					if(valuePlainLiteral.getLanguage()!=null)	//if there is a language indication
					{
						final String languageTag=LocaleUtilities.getLanguageTag(valuePlainLiteral.getLanguage());	//create a language tag from the locale
							//store the language tag in the xml:lang attribute
						propertyElement.setAttributeNS(XMLConstants.XML_NAMESPACE_URI.toString(), XMLUtilities.createQualifiedName(XMLConstants.XML_NAMESPACE_PREFIX, XMLConstants.ATTRIBUTE_LANG), languageTag);
					}
				}
				else if(valueLiteral instanceof RDFTypedLiteral)	//if this is a typed literal
				{
					final RDFTypedLiteral valueTypedLiteral=(RDFTypedLiteral)valueLiteral;	//cast the literal to a typed literal
						//store the datatype reference URI in the rdf:datatype attribute
					propertyElement.setAttributeNS(RDF_NAMESPACE_URI.toString(), XMLUtilities.createQualifiedName(RDF_NAMESPACE_PREFIX, ATTRIBUTE_DATATYPE), valueTypedLiteral.getDatatypeURI().toString());
				}
			}
		}
		return propertyElement; //return the property element we constructed
	}

	/**Retrieves the prefix to use for the given namespace. If a namespace is
		unrecognized, a new one will be created and stored for future use.
	@param namespaceURI The namespace URI for which a prefix should be returned
	@return A prefix for use with the given namespace.
	*/
/*G***del when works
	protected String getPrefix(final String namespaceURI)
	{
		String prefix=(String)namespacePrefixMap.get(namespaceURI);  //get the prefix keyed by the namespace
		if(prefix==null)  //if there is no prefix for this namespace
		{

		}

		//G***fix to detect unknown namespaces and assign new prefixes
	  return (String)namespacePrefixMap.get(namespaceURI);  //return a prefix keyed by the namespace
	}
*/


	/**Retrieves a label appropriate for the reference URI of the resource. If the
		resource has namespace URI and local name, the XML qualified name will be
		returned in <em>namespaceURI</em>:<em>localName</em> format; otherwise,
		reference URI itself will be returned.
	@param resource The resource the label of which to return.
	@return A label representing the reference URI of the resource.
	*/
/*G**del if not needed
	public String getLabel(final RDFResource resource)
	{
		return getLabel(resource, true);	//get 
	}
*/

	//TODO maybe bring back the getNamespaceURI() and getLocalName() methods removed on 2007-03-04 so that the registered URIs are searched
	
	/**Retrieves a label appropriate for the reference URI of the resource.
	If the resource has namespace URI and local name, the XML qualified name will be
	returned in <var>namespaceURI</var>:<var>localName</var> format;
	otherwise, the reference URI itself will be returned.
	@param resourceURI The URI of the resource the label of which to return.
	@return A label representing the reference URI of the resource.
	*/
	public String getLabel(final URI resourceURI)
	{
		URI namespaceURI=getNamespaceURI(resourceURI); //get the resource namespace URI, if it has one		
		String localName=getLocalName(resourceURI); //get the resource's local name
		  //if there is no namespace URI known, see if a namespace URI is registered that matches the beginning of this reference URI
		if((namespaceURI==null || localName==null) && resourceURI!=null)  //if there is no namespace URI or local name, but there is a reference URI
		{
				//TODO right now we iterate through the namespaces each time; there might be a better way to do this
			final String resourceURIString=resourceURI.toString();	//get a string version of the resource URI to use in comparisons
			final Iterator<String> namespaceIterator=getNamespacePrefixMap().keySet().iterator(); //get an iterator to the namespace URIs
			while(namespaceIterator.hasNext())  //while there are more namespaces
			{
				final String registeredNamespaceURI=namespaceIterator.next();  //get this namespace URI TODO eventually use URIs in the map
				if(resourceURIString.startsWith(registeredNamespaceURI)) //if this reference URI is in this namespace
				{
				  namespaceURI=URI.create(registeredNamespaceURI);  //we'll use the registered namespace URI
				  localName=resourceURIString.substring(registeredNamespaceURI.length()); //remove the namespace URI from the front of the reference URI to get the local name
				}
			}
		}
		if(namespaceURI!=null && localName!=null) //if we've determined a namespace URI and a local name
		{
				//get the prefix from the to be used for this namespace, but don't generate a new prefix if we don't recognize it
			final String prefix=XMLSerializer.getNamespacePrefix(getNamespacePrefixMap(), namespaceURI.toString(), false);
			if(prefix!=null)	//if we know a prefix for this namespace URI
			{
				if(RDF_NAMESPACE_URI.equals(namespaceURI))  //if this is the RDF namespace
				{
					if(localName.startsWith(CONTAINER_MEMBER_PREFIX)) //if this is one of the rdf:li_XXX members
						localName=LI_PROPERTY_NAME; //just use the normal rdf:li property name---the order is implicit in the serialization
				}
				return XMLUtilities.createQualifiedName(prefix, localName);  //create an XML qualified name for this namespace prefix and local name
			}
		}
		return resourceURI!=null ? resourceURI.toString() : ""; //just use the reference URI as the label, if we can't find anything else TODO we might want to check for rdf:li_ here as well; maybe not
	}

}