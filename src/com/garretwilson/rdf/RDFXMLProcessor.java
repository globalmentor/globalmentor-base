package com.garretwilson.rdf;

import java.net.MalformedURLException;
import java.util.*;
import com.garretwilson.net.URLConstants;
import com.garretwilson.text.xml.XMLBase;
import com.garretwilson.text.xml.XMLUtilities;
import com.garretwilson.util.Debug;
import com.garretwilson.util.NameValuePair;
import org.w3c.dom.*;

/**Class that is able to construct an RDF data model from an XML-based
	RDF serialization. Each instance of an RDF processor maintains an internal
	RDF data model throughout its lifetime that is continually updated with
	every new RDF processing that occurs.
	<p>The RDF processor maintains RDF data in two separate formats: the RDF
	data model <code>RDF</code>, as well as a list of statements used to create
	the data model. The RDF data model may be replaced and its members modified,
	but these actions will not update the list of RDF statements. The RDF
	statements are only generated by the RDF processor itself as it parses
	RDF serializations, and are available to give information on the parser
	actions.</p>
@author Garret Wilson
*/
public class RDFXMLProcessor extends AbstractRDFProcessor
{

	/**Default constructor.*/
	public RDFXMLProcessor()
	{
		super();  //construct parent base class
	}

	/**Constructor that specifies an existing data model to continue filling.
	@param newRDF The RDF data model to use.
	*/
	public RDFXMLProcessor(final RDF newRDF)
	{
		super(newRDF);  //construct the parent class
	}

	/**Processes RDF serialized in an XML document. Processes data contained in
		every <code>&lt;rdf:RDF&gt;</code> data island.
	@param document The XML document that might contain RDF data.
	@return The RDF data model resulting from this processing and any previous
		processing.
	*/
	public RDF process(final Document document)
	{
		return process(document, null);  //process this document with no base URI specified
	}

	/**Processes RDF serialized in an XML document. Processes data contained in
		every <code>&lt;rdf:RDF&gt;</code> data island.
	@param document The XML document that might contain RDF data.
	@param baseURI The base URI, or <code>null</code> if the base URI is not
		known.
	@return The RDF data model resulting from this processing and any previous
		processing.
	*/
	public RDF process(final Document document, final String baseURI)
	{
		return process(document.getDocumentElement(), baseURI); //process the data in the document element
	}

	/**Processes RDF serialized in an XML document. Searches the given element and
		all its children, processing data contained in every
		<code>&lt;rdf:RDF&gt;</code> data island.
	@param element The XML element that might contain RDF data.
	@return The RDF data model resulting from this processing and any previous
		processing.
	*/
	public RDF process(final Element element)
	{
		return process(element, null);  //process this element without specifying a base URI
	}


	/**Processes RDF serialized in an XML document. Searches the given element and
		all its children, processing data contained in every
		<code>&lt;rdf:RDF&gt;</code> data island.
	@param element The XML element that might contain RDF data.
	@param baseURI The base URI, or <code>null</code> if the base URI is not
		known.
	@return The RDF data model resulting from this processing and any previous
		processing.
	*/
	public RDF process(final Element element, final String baseURI)
	{
		setBaseURI(baseURI);  //set the base URI
		if(RDF_NAMESPACE_URI.equals(element.getNamespaceURI()) //if this element is in the RDF namespace
			  && ELEMENT_RDF.equals(element.getLocalName())) //if this element indicates that the children are RDF
		{
			final NodeList childNodeList=element.getChildNodes(); //get a list of child nodes
			for(int i=0; i<childNodeList.getLength(); ++i)  //look at each child node
			{
				final Node childNode=childNodeList.item(i); //get a reference to this child node
				if(childNode.getNodeType()==childNode.ELEMENT_NODE) //if this is an element
				{
					processRDF((Element)childNode);  //parse the contents of the RDF container element
				}
			}
		}
		else  //if this is a normal, non-RDF node
		{
			final NodeList childNodeList=element.getChildNodes(); //get a list of child nodes
			for(int i=0; i<childNodeList.getLength(); ++i)  //look at each child node
			{
				final Node childNode=childNodeList.item(i); //get a reference to this child node
				if(childNode.getNodeType()==childNode.ELEMENT_NODE) //if this is an element
				{
					process((Element)childNode);  //parse the contents of the element, not knowing if this is an RDF element or not
				}
			}
		}
		return getRDF();  //return the RDF data collected
	}

	/**Processes the given element categorically as RDF, as if it were contained
		in an <code>&lt;rdf:RDF&gt;</code> element.
	@param element The XML element that represents RDF data.
	*/
	public void processRDF(final Element element) //G***where is this used? does it really need to be public? if so, it probably needs a baseURI parameter version
	{
		//G***what do we do if <rdf:RDF> occurs inside <rdf:RDF>?
		processResource(element); //process the given element as an RDF resource
	}

	/**Processes the given element as representing an RDF resource.
	@param element The XML element that represents the RDF resource.
	@return The constructed resource the XML element represents.
	*/
	protected RDFResource processResource(final Element element)
	{
		final String elementNamespaceURI=element.getNamespaceURI(); //get the element's namespace
		final String elementLocalName=element.getLocalName(); //get the element's local name
Debug.trace("processing resource with XML element namespace: ", elementNamespaceURI); //G***del
Debug.trace("processing resource with XML local name: ", elementLocalName); //G***del
		//G***what do we do if <rdf:RDF> occurs inside <rdf:RDF>?
		/*G***bring back final */String referenceURI;  //we'll determine the reference URI from the rdf:about or rdf:ID attribute
		{
			final String referenceURIValue=getRDFAttribute(element, ATTRIBUTE_ABOUT); //get the reference URI, if there is one
			final String anchorID=getRDFAttribute(element, ATTRIBUTE_ID); //get the anchor ID if there is one
		  Debug.assert(referenceURIValue==null || anchorID==null, "Resource cannot have both reference URI "+referenceURIValue+" and anchor ID "+anchorID+"."); //G***change to an actual RDF error
			if(referenceURIValue!=null) //if there is a reference URI
			{
Debug.trace("found reference URI: ", referenceURIValue);  //G***del
//G***del				//G***we need to normalize the reference URI
				try
				{
				referenceURI=XMLBase.resolveURI(referenceURIValue, element, getBaseURI());  //resolve the reference URI to the base URI
				}
				catch(MalformedURLException malformedURLException)
				{
//G***fix					Debug.warn(malformedURLException);  //G***fix
					referenceURI=referenceURIValue; //G***fix
				}
			}
			else if(anchorID!=null)  //if there is an anchor ID
			{
Debug.trace("found anchor ID: ", anchorID);  //G***del
				try
				{
				referenceURI=XMLBase.getBaseURI(element, getBaseURI())+URLConstants.FRAGMENT_SEPARATOR_CHAR+anchorID;  //create a reference URI from the document base URI and the anchor ID
				}
				catch(MalformedURLException malformedURLException)
				{
//G***fix					Debug.warn(malformedURLException);  //G***fix
					referenceURI=getBaseURI()+URLConstants.FRAGMENT_SEPARATOR_CHAR+anchorID;  //G***fix
				}
			}
			else  //if there is neither a resource ID nor an anchor ID
			{
				referenceURI=getRDF().createAnonymousReferenceURI();  //generate an anonymous reference URI for the resource
			}
		}
Debug.trace("resulting reference URI: ", referenceURI);  //G***del

//G***del		final String anchorID=getRDFAttribute(element, ATTRIBUTE_ID); //get the anchor ID, if there is one
		final RDFResource resource;  //we'll create a resource and store it here
//G***del when works		final Resource resource=getRDF().locateResource(referenceURI); //get or create a new resource with the given reference URI
		  //if this is an <rdf:Description> element
		if(RDF_NAMESPACE_URI.equals(elementNamespaceURI) && ELEMENT_DESCRIPTION.equals(elementLocalName))
		{
//G***del Debug.trace("Is rdf:Description."); //G***del
			resource=getRDF().locateResource(referenceURI); //get or create a new resource with the given reference URI
		}
		else  //if this is not an <rdf:Description> element, we already know its type (but it may not have been added if the resource was created earlier without a type)
		{
//G***del Debug.trace("locating a resource with reference URI: ", referenceURI); //G***del
			  //get or create a new resource with the given reference URI and type;
				//  this allows a resource factory to create the appropriate type of
				//  resource object
			resource=getRDF().locateResource(referenceURI, elementNamespaceURI, elementLocalName);
/*G***del when works
			final Resource typeProperty=RDFUtilities.getTypeProperty(getRDF()); //get a rdf:type resource G***maybe create this beforehand somewhere
			final Resource typeValue=getRDF().locateResource(elementNamespaceURI, elementLocalName);  //get a resource for the value of the property
			resource.addProperty(typeProperty, typeValue);  //add the property to the resource
*/
				  //G***we need to just use the RDFUtilities to get the type, probably, to replace this early version
			final RDFResource typeProperty=RDFUtilities.locateTypeProperty(getRDF()); //get the rdf:type resource
//G***del Debug.trace("type property: ", typeProperty); //G***del
			RDFObject typeValue=resource.getPropertyValue(typeProperty); //get the value of the type that was added when the object was created
//G***del Debug.trace("type value: ", typeValue); //G***del
		  if(typeValue==null) //if the resource has already been created, but it does not have this type
			{
			  typeValue=getRDF().createResource(elementNamespaceURI, elementLocalName); //create a type value resource
			}
				//add a statement to our data model in the form {rdf:type, resource, elementName}
				//this will in most cases attempt to add the type value again, if it
				//  was constructed autmotically when the resource was created, but
				//  the property will be seen as a duplicate and ignored
		  addStatement(typeProperty, resource, typeValue);
		}
		processAttributeProperties(resource, element, DESCRIPTION_CONTEXT);  //parse the attributes for the resource description
			//parse the child elements
		final String RDF_LI_REFERENCE_URI=RDFUtilities.createReferenceURI(RDF_NAMESPACE_URI, ELEMENT_LI);  //create a reference URI from the rdf:li element qualified name
		int memberCount=0; //show that we haven't found any container members, yet
		final NodeList childNodeList=element.getChildNodes(); //get a list of child nodes
		for(int i=0; i<childNodeList.getLength(); ++i)  //look at each child node
		{
			final Node childNode=childNodeList.item(i); //get a reference to this child node
			if(childNode.getNodeType()==childNode.ELEMENT_NODE) //if this is an element
			{
				final NameValuePair propertyNameValuePair=processProperty((Element)childNode);  //parse the element representing an RDF property
				final RDFResource property;  //we'll see whether we should convert <rdf:li>
				if(propertyNameValuePair.getName().equals(RDF_LI_REFERENCE_URI))  //if this is a rdf:li property
				{
					++memberCount;  //show that we have another member
					//create a local name in the form "_X"
					final String propertyLocalName=CONTAINER_MEMBER_PREFIX+memberCount;
					property=getRDF().locateResource(RDF_NAMESPACE_URI, propertyLocalName); //use the revised member form as the property
				}
				else  //if this is a normal property
					property=(RDFResource)propertyNameValuePair.getName(); //just use the property as is
					//add a statement to our data model in the form {property, resource, value}
				addStatement(property, resource, (RDFObject)propertyNameValuePair.getValue());
			}
		}
		return resource;  //return the resource we created
	}

	/**Parses the attributes of the given element and assign them as properties
	  to the given resource. Special RDF properties such as <code>rdf:about</code>
		are ignored.
	@param resource The resource to which the properties should be added.
	@param element The element that contains the attributes to be considered
		properties.
	@param context The context, <code>DESCRIPTION_CONTEXT</code> or
		<code>REFERENCE_CONTEXT</code>, describing whether the attributes are part
		of a resource description or a resource reference.
	*/
	protected void processAttributeProperties(final RDFResource resource, final Element element, final int context)
	{
		final String elementNamespaceURI=element.getNamespaceURI(); //get the element's namespace
		final NamedNodeMap attributeNodeMap=element.getAttributes();  //get a map of the attributes
		for(int i=attributeNodeMap.getLength()-1; i>=0; --i)  //look at each of the attributes
		{
			final Attr attribute=(Attr)attributeNodeMap.item(i);  //get a reference to this attribute
		  final String attributeNamespaceURI=attribute.getNamespaceURI(); //get the attribute's namespace URI
		  final String attributeLocalName=attribute.getLocalName(); //get the attribute's local name
		  final String attributeValue=attribute.getValue(); //get the attribute's value
/*G***del
Debug.trace("processing attribute from namespace: ", attributeNamespaceURI);
Debug.trace("processing attribute from local name: ", attributeLocalName);
Debug.trace("processing attribute from value: ", attributeValue);
*/
			  //ignore the rdf:about attribute in descriptions, disallow it in references
			if(ATTRIBUTE_ABOUT.equals(attributeLocalName))
			{
			  if(RDF_NAMESPACE_URI.equals(attributeNamespaceURI) || (attributeNamespaceURI==null && RDF_NAMESPACE_URI.equals(elementNamespaceURI)))
				{
					if(context==DESCRIPTION_CONTEXT)  //ignore rdf:about in descriptions
				    continue;
					else if(context==REFERENCE_CONTEXT) //rdf:about isn't allowed in a reference
						Debug.error("rdf:about attribute is not allowed in a resource reference."); //G***fix with real exceptions
				}
			}
			  //ignore the rdf:ID attribute in descriptions, disallow it in references
			if(ATTRIBUTE_ID.equals(attributeLocalName))
			{
			  if(RDF_NAMESPACE_URI.equals(attributeNamespaceURI) || (attributeNamespaceURI==null && RDF_NAMESPACE_URI.equals(elementNamespaceURI)))
				{
					if(context==DESCRIPTION_CONTEXT)  //ignore rdf:ID in descriptions
				    continue;
					else if(context==REFERENCE_CONTEXT) //rdf:ID isn't allowed in a reference
						Debug.error("rdf:ID attribute is not allowed in a resource reference."); //G***fix with real exceptions
				}
			}
			  //ignore the rdf:resource attribute in reference, disallow it in descriptions
			if(ATTRIBUTE_RESOURCE.equals(attributeLocalName))
			{
			  if(RDF_NAMESPACE_URI.equals(attributeNamespaceURI) || (attributeNamespaceURI==null && RDF_NAMESPACE_URI.equals(elementNamespaceURI)))
				{
					if(context==DESCRIPTION_CONTEXT)  //ignore rdf:ID in descriptions
						Debug.error("rdf:resource attribute is not allowed in a resource description."); //G***fix with real exceptions
					else if(context==REFERENCE_CONTEXT) //rdf:ID isn't allowed in a reference
				    continue;
				}
			}
				//add a statement to our data model in the form {attributeName, resource, attributeValue}
		  addStatement(getRDF().locateResource(attributeNamespaceURI, attributeLocalName), resource, new Literal(attributeValue));
		}
	}

	/**Processes the given element as representing an RDF property.
	@param element The XML element that represents the RDF property.
	@return A name/value pair the name of which is a property resource (the RDF
		statement predicate) and the value of which is a value resource or a literal
		(the RDF statement object).
	*/
	protected NameValuePair processProperty(final Element element)
	{
		final String elementNamespaceURI=element.getNamespaceURI(); //get the element's namespace
		final String elementLocalName=element.getLocalName(); //get the element's local name
//G***del Debug.trace("processing property with XML element namespace: ", elementNamespaceURI); //G***del
//G***del Debug.trace("processing property with XML local name: ", elementLocalName); //G***del
		final RDFResource propertyResource=getRDF().locateResource(elementNamespaceURI, elementLocalName); //get a resource from the element name
		final RDFObject propertyValue;  //we'll assign the property value to this variable
		final String referenceURIValue=getRDFAttribute(element, ATTRIBUTE_RESOURCE); //get the reference URI of the referenced resource, if there is one
//G***del Debug.trace("RDF attribute: ", referenceURIValue);  //G***del
		if(referenceURIValue!=null) //if there is a reference URI
		{
//G***del				//G***we need to normalize the reference URI
		  /*G***bring back final */String referenceURI;
			try
			{
Debug.trace("Resolving rdf:resource: ", referenceURIValue); //G***del
					referenceURI=XMLBase.resolveURI(referenceURIValue, element, getBaseURI());  //resolve the reference URI to the base URI
				}
				catch(MalformedURLException malformedURLException)
				{
//G***fix					Debug.warn(malformedURLException);  //G***fix
					referenceURI=referenceURIValue; //G***fix
				}
Debug.trace("found referene URI: ", referenceURI);  //G***del
		  final RDFResource propertyValueResource=getRDF().locateResource(referenceURI); //get or create a new resource with the given reference URI
//G***del Debug.trace("property value resource: ", propertyValueResource);  //G***del

			processAttributeProperties(propertyValueResource, element, REFERENCE_CONTEXT);  //parse the property attributes, assigning them to the property value
		  propertyValue=propertyValueResource;  //the resource value is our property value
//G***make sure there are no attributes at all
		}
		else if(element.getChildNodes().getLength()==0) //if there are no child elements, this is an anonymous resource
		{
		  final RDFResource propertyValueResource=getRDF().locateResource(getRDF().createAnonymousReferenceURI());  //get or create a resource from a generated anonymous reference URI
			processAttributeProperties(propertyValueResource, element, REFERENCE_CONTEXT);  //parse the property attributes, assigning them to the property value
		  propertyValue=propertyValueResource;  //the resource value is our property value
		}
		else  //if there is no reference URI
		{
			//G***we should make sure there are no attributes
			//G***check to if we should parse literally or not
			Element childElement=null; //show that we haven't found any child elements, yet
				//parse the child elements
			final NodeList childNodeList=element.getChildNodes(); //get a list of child nodes
			for(int i=0; i<childNodeList.getLength(); ++i)  //look at each child node
			{
				final Node childNode=childNodeList.item(i); //get a reference to this child node
				if(childNode.getNodeType()==childNode.ELEMENT_NODE) //if this is an element
				{
					if(childElement==null)  //if we haven't already found a child element
					{
						childElement=(Element)childNode;  //cast the child node to an element
					}
					else if(childElement!=null)  //if we've already found a child element
					{
						Debug.warn("Only one property value allowed for "+propertyResource); //G***fix with real error handling
					}
				}
			}
			if(childElement!=null)  //if we found a child element for the property value
			{
				propertyValue=processResource(childElement); //process the child element as an RDF resource, the value of the property in this case
			}
			else  //if we didn't find any child elements
			{
				propertyValue=new Literal(XMLUtilities.getText(element, false));  //create a literal from the element's text
			}
		}
//G***del Debug.trace("returning name/value pair: ", new NameValuePair(propertyResource, propertyValue)); //G***del
		return new NameValuePair(propertyResource, propertyValue);  //return a name/value pair consisting of the property resource (the predicate) and its value (the object)
	}

	/**Retrieves an RDF attribute from an element, if it exists, recognizing
		either prefixed or non-prefixed attributes. If the non-prefixed form is
		used, a warning is generated.
		It is assumed that this method will only be called once for a particular
		attribute, as each call could produce another warning.
	@param element The element being checked for attributes.
	@param attributeLocalName The local name of the RDF attribute to check for.
	@return The specified RDF attribute.
	*/
	protected String getRDFAttribute(final Element element, final String attributeLocalName)
	{
		if(element.hasAttributeNS(RDF_NAMESPACE_URI, attributeLocalName))  //if there is a prefixed attribute value
		{
		  return element.getAttributeNS(RDF_NAMESPACE_URI, attributeLocalName); //get the prefixed attribute value
		}
		else if(RDF_NAMESPACE_URI.equals(element.getNamespaceURI()) && element.hasAttributeNS(null, attributeLocalName)) //if there is a non-prefixed attribute value
		{
			Debug.warn("Non-prefixed rdf:"+attributeLocalName+" attribute deprecated."); //G***put in a real warning
		  return element.getAttributeNS(null, attributeLocalName); //return the non-prefixed attribute value
		}
		else  //if neither attribute is available
			return null;  //show that the RDF attribute is not available
	}

}