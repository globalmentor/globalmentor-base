package com.garretwilson.rdf;

import java.net.URI;
import java.util.*;
import com.garretwilson.text.xml.XMLConstants;
import com.garretwilson.text.xml.XMLUtilities;
import com.garretwilson.text.xml.XMLSerializer;
import com.garretwilson.util.NameValuePair;
import com.garretwilson.util.Debug;
import org.w3c.dom.*;

/**Class that creates an XML representation of RDF through DOM.
@author Garret Wilson
*/
public class RDFXMLifier implements RDFConstants
{

//G***del	protected final static String RDF_LI
//G***del		RDFUtilities.createReferenceURI()

	/**The map of prefixes, keyed by namespace URIs.*/
	protected final Map namespacePrefixMap;

		/**@return The map of prefixes, keyed by namespace URI.*/
		protected Map getNamespacePrefixMap() {return namespacePrefixMap;}
		
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

	/**A general <code>rdf:type</code> property to use in comparisons.*/
	protected final static URI TYPE_PROPERTY;	//initialized below

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

	/**A map of namespace URIs (keyed to the same namespaces URIs) the properties
		in which should have literal values serialized as attributes rather than
		elements.
	*/
	private final Map literalAttributeSerializationNamespaceMap=new HashMap();

	/**Adds a namespace URI the properties in which namespace should be
		categorically serialized as attributes if the property value is a literal.
		For attributes added by this method, the value of
		<code>isLiteralAttributeSerialization()</code> is ignored.
	@param namespaceURI The URI of the namespace that should have its literal
		property values serialized as attributes.
	*/
	public void addLiteralAttributeSerializationNamespaceURI(final URI namespaceURI)
	{
		literalAttributeSerializationNamespaceMap.put(namespaceURI, namespaceURI);  //add the namespace to the map both as a value and as that value's key
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
		return literalAttributeSerializationNamespaceMap.get(namespaceURI)!=null; //if this namespace is listed in the map, return true
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
		setLiteralAttributeSerialization(compact);  //if we should be compact, show literals as attributes
	}

	/**Creates a default RDF XML document.
	@param domImplementation The DOM implementation to use.
	@return A newly created default RDF XML document with an RDF section.
	*/
	public static Document createDefaultRDFDocument(final DOMImplementation domImplementation)
	{
		  //create an RDF document
		final Document document=domImplementation.createDocument(RDF_NAMESPACE_URI.toString(), XMLUtilities.createQualifiedName(RDF_NAMESPACE_PREFIX, ELEMENT_RDF), null);
		final Element rdfElement=createRDFElement(document);  //G***fix; the DOM should already create this
		document.appendChild(rdfElement);	//add the RDF element to the document //G***del when DOM is updated
		  //add the RDF namespace declaration prefix, xmlns:rdf
		document.getDocumentElement().setAttributeNS(XMLConstants.XMLNS_NAMESPACE_URI, XMLUtilities.createQualifiedName(XMLConstants.XMLNS_NAMESPACE_PREFIX, RDFConstants.RDF_NAMESPACE_PREFIX), RDFConstants.RDF_NAMESPACE_URI.toString());
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
//G***del		final Document document=createDefaultRDFDocument(domImplementation);  //create a default RDF document
/*G***del
		final String rdfElementQualifiedName=XMLUtilities.createQualifiedName(RDF_NAMESPACE_PREFIX, ELEMENT_RDF);  //create a qualified name for the document root element
		final Document document=domImplementation.createDocument(RDF_NAMESPACE_URI, rdfElementQualifiedName, null);	//create a document with <rdf:RDF> as the body element
		final Element rdfElement=createRDFElement(document);  //G***fix; the DOM should already create this
		document.appendChild(rdfElement);	//add the RDF element to the document
*/
/*G***del
		final Element rootElement=document.getDocumentElement();  //get the document element
		createElements(rdf, rootElement); //XMLify the resource as elements under the document element
		return document;  //return the document we created
*/
		return createDocument(rdf, null, domImplementation);  //create a document for the whole RDF data model, without specifying a specific resource
	}

	/**Creates an XML document with <code>&lt;rdf:RDF&gt;</code> as the document
		element, and creates an element representing the given resource or, if
		no resource is given, creates elements representing all the non-anonymous
		resources of the given RDF data model as children of the that element.
	@param rdf The RDF data model to XMLify.
	@param resource The resource to store as an XML element, or <code>null</code>
		if all non-anonymous resources should be stored.
	@param domImplementation The DOM implementation to use.
	@return The XML document tree that represents the given RDF resource or data
		model.
	*/
	public Document createDocument(final RDF rdf, final RDFResource resource, final DOMImplementation domImplementation)
	{
		final Document document=createDefaultRDFDocument(domImplementation);  //create a default RDF document
		final Element rootElement=document.getDocumentElement();  //get the document element
		if(resource!=null)  //if a resource as passed
		{
			final Element resourceElement=createResourceElement(document, resource); //create an element from this resource
			rootElement.appendChild(resourceElement); //add the resource element to the root element
		}
		else  //if no resource was passed, we'll create a document with all the resources
		{
			createElements(rdf, rootElement); //XMLify the resource as elements under the document element
		}
		return document;  //return the document we created
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
			//create an <rdf:RDF> element
		final Element rdfElement=document.createElementNS(RDF_NAMESPACE_URI.toString(), XMLUtilities.createQualifiedName(RDF_NAMESPACE_PREFIX, ELEMENT_RDF));
		createElements(rdf, rdfElement); //XMLify the resourcs as elements under the RDF element
		return rdfElement;  //return the element we constructed with RDF resources as child elements
	}

	/**Creates elements representing the given RDF data model as children of the
		given parent element.
	@param rdf The RDF data model to XMLify.
	@param parentElement The parent XML element of the elements to be added.
	*/
	public void createElements(final RDF rdf, final Element parentElement)
	{
		final Iterator resourceIterator=rdf.getResourceIterator();  //get an iterator to all the RDF resources
		while(resourceIterator.hasNext()) //while there are resources remaining
		{
			final RDFResource resource=(RDFResource)resourceIterator.next();  //get the next resource
//G***del Debug.trace("looking at resource: ", resource); //G***del
			final URI referenceURI=resource.getReferenceURI(); //get the resource reference URI
			//G***in throwing away anonymous resources, some might have been described at the top of the hierarchy---we should really just check to see which resources are not referenced
//G***del when works			referenceURI!=null && referenceURI.indexOf("anonymous")<0 //if this is not an anonymous node G***fix the way we determine anonymous resources

//G***check to see if there are no references to this node---if so, we'll probably want to serialize it anyway
			if(!resource.isAnonymous() //if this is not an anonymous node
				  && resource.getPropertyCount()>0)  //if this resource actually has properties (even properties such as type identifiers are resources, but they don't have properties)
			{
				final Element resourceElement=createResourceElement(/*G***del if not needed rdf, */parentElement.getOwnerDocument(), resource); //create an element from this resource
				parentElement.appendChild(resourceElement); //add the resource element to our parent element
			}
		}
	}

	/**Creates an XML element to represent the given resource.
//G***del if not needed	@param rdf The RDF data model.
	@param document The document to be used as an element factory.
	@param resource The resource for which an element should be created.
	@return The XML element that represents the given RDF resource.
	*/  //G***del rdf if we don't need
	public Element createResourceElement(/*G***del if not needed final RDF rdf, */final Document document, final RDFResource resource)
	{
//G***del Debug.trace("creating resource element for resource: ", resource);  //G***del
		final Element resourceElement;  //we'll store here the element that we create
		RDFResource resourceType=RDFUtilities.getType(resource); //get the type of the resource
			//if the resource has a type, but we can't extract a namespace URI and local name from it G***add more logic here later to automatically determine the namespace
		if(resourceType!=null && (resourceType.getNamespaceURI()==null || resourceType.getLocalName()==null))
			resourceType=null;  //don't use the resource type for creating the element name
		if(resourceType!=null)   //if thie resource has a type that we can use for the element name
		{
			final String prefix=XMLSerializer.getNamespacePrefix(getNamespacePrefixMap(), resourceType.getNamespaceURI().toString());  //get the prefix for use with this namespace
			final String qualifiedName=XMLUtilities.createQualifiedName(prefix, resourceType.getLocalName());  //create a qualified name for the element
			resourceElement=document.createElementNS(resourceType.getNamespaceURI().toString(), qualifiedName);  //create an element from the resource type
		}
		else  //if we can't create an element from its type
		{
			final String qualifiedName=XMLUtilities.createQualifiedName(RDF_NAMESPACE_PREFIX, ELEMENT_DESCRIPTION);  //create a qualified name for the element
		  resourceElement=document.createElementNS(RDF_NAMESPACE_URI.toString(), qualifiedName); //create an rdf:Description element
		}
		if(resource.getReferenceURI()!=null && !resource.isAnonymous())  //if this resource has a reference URI and it isn't an anonymous resource G***do we really want to check for the reference URI, because it isn't needed anymore---should this be an assert?
		{
				//set the rdf:about attribute to the reference URI
			final String aboutAttributeQualifiedName=XMLUtilities.createQualifiedName(RDF_NAMESPACE_PREFIX, ATTRIBUTE_ABOUT);  //create a qualified name for reference URI
				//add the rdf:about attribute with the value set to the reference URI of the resource
			resourceElement.setAttributeNS(RDF_NAMESPACE_URI.toString(), aboutAttributeQualifiedName, resource.getReferenceURI().toString());
		}
		addProperties(document, resourceElement, resource);  //add all the properties of the resource to the resource element
/*G***del when works
		final Iterator propertyIterator=resource.getPropertyIterator(); //get an iterator to all the element properties
		while(propertyIterator.hasNext()) //while there are more properties
		{
			final NameValuePair propertyNameValuePair=(NameValuePair)propertyIterator.next(); //get the next property name/value pair
		  final RDFResource propertyResource=(RDFResource)propertyNameValuePair.getName();  //get the property predicate
			final RDFObject propertyValue=(RDFObject)propertyNameValuePair.getValue();  //get the property value
				//if this property is not a type property we already used for creating the element name
		  if(!(propertyResource.equals(TYPE_PROPERTY) && propertyValue.equals(resourceType)))
			{
Debug.trace("looking at non-type property: ", propertyResource);
Debug.trace("looking at non-type property value: ", propertyValue);

					//if we know the namespace URI and local name of the property,
					//  and the property value is a literal
				if(propertyResource.getNamespaceURI()!=null && propertyResource.getLocalName()!=null && propertyValue instanceof Literal)
				{
					final String prefix=getPrefix(propertyResource.getNamespaceURI());  //get the prefix for use with this namespace
					final String qualifiedName=XMLUtilities.createQualifiedName(prefix, propertyResource.getLocalName());  //create a qualified name for the attribute
						//store the property as an attribute
				  resourceElement.setAttributeNS(propertyResource.getNamespaceURI(), qualifiedName, ((Literal)propertyValue).getValue());
				}
				else  //if the value is a resource or we don't know the property's namespace URI and local name G***add more logic here later to automatically determine the namespace
				{
					final Element propertyElement=createPropertyElement(document, propertyResource, propertyValue); //create a property element
					resourceElement.appendChild(propertyElement); //append the property element we created
				}
			}
		}
*/
		return resourceElement; //return the resource element we created
	}

	/**Adds all properties of the resource to the given element.
	@param document The document to be used as an element factory.
	@param element The XML element to which the properties should be added
	@param resource The resource the properties of which should be added to the
		DOM tree, either as elements or attributes.
	*/
	protected void addProperties(final Document document, final Element element, final RDFResource resource)
	{
		RDFResource resourceType=RDFUtilities.getType(resource); //get the type of the resource G***do we want this in the general routine? we probably have to have it
		final Iterator propertyIterator=resource.getPropertyIterator(); //get an iterator to all the element properties
		while(propertyIterator.hasNext()) //while there are more properties
		{
			final NameValuePair propertyNameValuePair=(NameValuePair)propertyIterator.next(); //get the next property name/value pair
		  final RDFResource propertyResource=(RDFResource)propertyNameValuePair.getName();  //get the property predicate
			final RDFObject propertyValue=(RDFObject)propertyNameValuePair.getValue();  //get the property value
				//if this property is not a type property we already used for creating the element name
		  if(!(propertyResource.equals(TYPE_PROPERTY) && propertyValue.equals(resourceType)))
			{
Debug.trace("looking at non-type property: ", propertyResource);
Debug.trace("looking at non-type property value: ", propertyValue);
				final URI propertyNamespaceURI=propertyResource.getNamespaceURI(); //get the namespace URI of the property
				boolean serializeLiteralAttribute=false; //start out by assuming we won't serialize as an attribute
				  //if the property value is a literal
				if(propertyValue instanceof Literal)
				{
					if(isLiteralAttributeSerialization()) //if we should serialize all literal property values as attributes
					{
						serializeLiteralAttribute=true;  //show that we should use an attribute
					}
							  //if we know the namespace URI and local name of the property
					else if(propertyNamespaceURI!=null && propertyResource.getLocalName()!=null)
					{
						if(isLiteralAttributeSerializationNamespaceURI(propertyNamespaceURI)) //if we should serialize literal property values from this namespace
						{
							serializeLiteralAttribute=true;  //show that we should use an attribute
						}
					}
				}
/*G***del
					//if we know the namespace URI and local name of the property,
					//  and the property value is a literal (and we're allowed to serialize
					//  literals as attributes).
				if(isLiteralAttributeSerialization() && propertyResource.getNamespaceURI()!=null && propertyResource.getLocalName()!=null && propertyValue instanceof Literal)
				{
*/
				if(serializeLiteralAttribute)  //if we should use an attribute for serializing this property value
				{
					final String prefix=XMLSerializer.getNamespacePrefix(getNamespacePrefixMap(), propertyResource.getNamespaceURI().toString());  //get the prefix for use with this namespace
					final String qualifiedName=XMLUtilities.createQualifiedName(prefix, propertyResource.getLocalName());  //create a qualified name for the attribute
						//store the property as an attribute
				  element.setAttributeNS(propertyResource.getNamespaceURI().toString(), qualifiedName, ((Literal)propertyValue).getValue());
				}
				else  //if the value is a resource or we don't know the property's namespace URI and local name G***add more logic here later to automatically determine the namespace
				{
					final Element propertyElement=createPropertyElement(/*G***del if not neededrdf, */document, propertyResource, propertyValue); //create a property element
					element.appendChild(propertyElement); //append the property element we created
				}
			}
		}
	}

	/**Creates an XML element to represent the given property.
		All anonymous	property value resources will be created as child elements.
		If an anonymous property value's properties are all literals, that property
		resource value's properties will be created as attributes of the property.
G***del if not needed	@param rdf The RDF data model.
	@param document The document to be used as an element factory.
	@param propertyResource The property predicate for which an element should be
		created.
	@param propertyValue The property value for which an element should be created.
	@return The XML element that represents the given RDF property.
	*/  //G***del rdf if we don't need
	protected Element createPropertyElement(/*G***del if not neededfinal RDF rdf, */final Document document, final RDFResource propertyResource, final RDFObject propertyValue)
	{
Debug.trace("creating property element for property: ", propertyResource);  //G***del
		final URI namespaceURI; //we'll store here the namespace URI of the property element
		final String qualifiedName; //we'll store here the qualified name of the property element
			//if we know the namespace URI and local name of the property
				  //G***replace all this code with the new utility method getLabel(RDFResource)
		if(propertyResource.getNamespaceURI()!=null && propertyResource.getLocalName()!=null)
		{
		  namespaceURI=propertyResource.getNamespaceURI();  //get the namespace URI
			final String prefix=XMLSerializer.getNamespacePrefix(getNamespacePrefixMap(), namespaceURI.toString());  //get the prefix for use with this namespace
			String localName=propertyResource.getLocalName(); //get the local name of the property
			if(RDF_NAMESPACE_URI.equals(namespaceURI))  //if this is the RDF namespace
			{
				if(localName.startsWith(CONTAINER_MEMBER_PREFIX)) //if this is one of the rdf:li_XXX members
					localName=ELEMENT_LI; //just use the normal rdf:li property name---the order is implicit in the serialization
			}
			qualifiedName=XMLUtilities.createQualifiedName(prefix, localName);  //create a qualified name for the element
		}
		else  //if either the namespace URI or the local name of the property is unknown G***add more logic here later to automatically determine the namespace
		{
				//G***we might want to check this for rdf:li_XXX as well
			namespaceURI=null;  //show that we don't know the namespace
			qualifiedName=propertyResource.getReferenceURI().toString(); //use the entire reference URI for the element name G***fix; this won't work---what can we do here?
		}
		final Element propertyElement=document.createElementNS(namespaceURI.toString(), qualifiedName);  //create an element from the property resource
		if(propertyValue instanceof RDFResource) //if the property value is a resource
		{
			final RDFResource valueResource=(RDFResource)propertyValue; //cast the value to a resource
			final URI valueReferenceURI=valueResource.getReferenceURI(); //get the reference URI of the value
//G***del when works			if(valueReferenceURI==null || valueReferenceURI.indexOf("anonymous")>=0) //if this is an anonymous node G***fix the way we determine anonymous resources
			if(valueResource.isAnonymous()) //if this is an anonymous node
			{
					//G***rename allSubPropertiesLiterals
				boolean allSubPropertiesLiterals=true; //we'll see if all the subproperties are literals; if so, we'll just add them as attributes
				final Iterator propertyIterator=valueResource.getPropertyIterator(); //get an iterator to all the element properties
				while(propertyIterator.hasNext()) //while there are more properties
				{
					final NameValuePair subPropertyNameValuePair=(NameValuePair)propertyIterator.next(); //get the next property name/value pair
					final RDFResource subPropertyResource=(RDFResource)subPropertyNameValuePair.getName();  //get the property resource
					final RDFObject subPropertyValue=(RDFObject)subPropertyNameValuePair.getValue();  //get the property value
				  final URI subPropertyNamespaceURI=subPropertyResource.getNamespaceURI(); //get the namespace URI of the property
					if(!(subPropertyValue instanceof Literal)) //if this value is not a literal
					{
						allSubPropertiesLiterals=false; //show that all subproperties are not literals
						break;  //stop looking for a non-literal
					}
					else if(!isLiteralAttributeSerialization()) //if we shouldn't serialize all literal property values as attributes
					{
						if(subPropertyNamespaceURI!=null && subPropertyResource.getLocalName()!=null)
						{
							if(!isLiteralAttributeSerializationNamespaceURI(subPropertyNamespaceURI)) //if we should serialize literal property values from this namespace
							{
								allSubPropertiesLiterals=false; //show that all subproperties are not literals, because we shouldn't serialize this literal as an attribute
							}
						}
						else  //we couldn't check to see if we should serialize literals as attributes from this namespace; assume we can't
							allSubPropertiesLiterals=false; //show that all subproperties are not literals, because we shouldn't serialize this one as an attribute
					}
				}
				if(allSubPropertiesLiterals)  //if all subproperties are literals, then just add them as attributes
				{
				  addProperties(document, propertyElement, valueResource);  //add all the properties of the value resource to the element representing the property
				}
				else  //if not all subproperties of the anonymous element are literals, add the value resource as a subelement of the property element
			  {
					final Element anonymousResourceElement=createResourceElement(/*G***del if not needed rdf, */document, valueResource); //create an element from this resource
					propertyElement.appendChild(anonymousResourceElement); //add the anonymous resource element to the property element
			  }
			}
			else  //if this is not an anymous mode, just link to it with rdf:resource
			{
				final String resourceAttributeQualifiedName=XMLUtilities.createQualifiedName(RDF_NAMESPACE_PREFIX, ATTRIBUTE_RESOURCE);  //create a qualified name for the link attribute
					//add the rdf:resource attribute with the value set to the reference URI of the property value resource
				propertyElement.setAttributeNS(RDF_NAMESPACE_URI.toString(), resourceAttributeQualifiedName, valueReferenceURI.toString());
			}
		}
		else if(propertyValue instanceof Literal)  //if the value is a literal
		{
			final Literal valueLiteral=(Literal)propertyValue;  //cast the value to a literal
			XMLUtilities.appendText(propertyElement, valueLiteral.getValue());  //append the literal value as content of the property element
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
	public String getLabel(final RDFResource resource)
	{
		URI namespaceURI=resource.getNamespaceURI(); //get the resource namespace URI, if it has one
		String localName=resource.getLocalName(); //get the resource's local name
		  //if there is no namespace URI known, see if a namespace URI is registered that matches the beginning of this reference URI
		if(namespaceURI==null || localName==null)  //if there is no namespace URI or local name
		{
			final URI referenceURI=resource.getReferenceURI(); //get the reference URI of the resource
				//G***right now we iterate through the namespaces each time; there might be a better way to do this
			final Iterator namespaceIterator=getNamespacePrefixMap().keySet().iterator(); //get an iterator to the namespace URIs
			while(namespaceIterator.hasNext())  //while there are more namespaces
			{
//G***del				final Map.Entry namespaceEntry=namespaceEntryIterator.next(); //get the next namespace entry
				final URI registeredNamespaceURI=(URI)namespaceIterator.next();  //get this namespace URI
				if(referenceURI.toString().startsWith(registeredNamespaceURI.toString())) //if this reference URI is in this namespace	//G***fix, now that we're using real URIs
				{
//G***del					final String prefix=(String)namespaceEntry.getKey();  //get this namespace prefix
				  namespaceURI=registeredNamespaceURI;  //we'll use the registered namespace URI
				  localName=referenceURI.toString().substring(registeredNamespaceURI.toString().length()); //remove the namespace URI from the front of the reference URI to get the local name	//G***fix better, now that we're using real URIs
//G***del					return XMLUtilities.createQualifiedName(prefix, localName);  //create an XML qualified name for this namespace prefix and local name
				}
			}
//G***del			return referenceURI; //just use the reference URI as the label, if we can't find anything else G***we might want to check for rdf:li_ here as well; maybe not
		}
		if(namespaceURI!=null && localName!=null) //if we've determined a namespace URI and a local name
		{
Debug.trace("has namespace: ", namespaceURI); //G***del
Debug.trace("has local name: ", localName); //G***del
				//get the prefix from the to be used for this namespace
			final String prefix=XMLSerializer.getNamespacePrefix(getNamespacePrefixMap(), namespaceURI.toString());
Debug.trace("prefix: ", prefix); //G***del
			if(RDF_NAMESPACE_URI.equals(namespaceURI))  //if this is the RDF namespace
			{
				if(localName.startsWith(CONTAINER_MEMBER_PREFIX)) //if this is one of the rdf:li_XXX members
					localName=ELEMENT_LI; //just use the normal rdf:li property name---the order is implicit in the serialization
			}
			return XMLUtilities.createQualifiedName(prefix, localName);  //create an XML qualified name for this namespace prefix and local name
		}
		else  //if there is no namespace URI known, see if a namespace URI is registered that matches the beginning of this reference URI
		{
/*G***del
			final String referenceURI=resource.getReferenceURI(); //get the reference URI of the resource
				//G***right now we iterate through the namespaces each time; there might be a better way to do this
			final Iterator namespaceEntryIterator=getNamespacePrefixMap().entrySet().iterator(); //get an iterator to the namespace prefixes and URIs
			while(namespaceEntryIterator.hasNext())  //while there are more namespace entries
			{
				final Map.Entry namespaceEntry=namespaceEntryIterator.next(); //get the next namespace entry
				final String namespaceURI=(String)namespaceEntry.getValue();  //get this namespace URI
				if(referenceURI.startsWith(namespaceURI)) //if this reference URI is in this namespace
				{
					final String prefix=(String)namespaceEntry.getKey();  //get this namespace prefix
					final String localName=referenceURI.substring(namespaceURI.length()); //remove the namespace URI from the front of the reference URI to get the local name
					return XMLUtilities.createQualifiedName(prefix, localName);  //create an XML qualified name for this namespace prefix and local name
				}
			}
*/
			return resource.getReferenceURI().toString(); //just use the reference URI as the label, if we can't find anything else G***we might want to check for rdf:li_ here as well; maybe not
		}
	}
	
	static
	{
		TYPE_PROPERTY=RDFUtilities.createReferenceURI(RDF_NAMESPACE_URI, TYPE);	//initialize the RDF type property URI
	}
}