package com.garretwilson.text.xml.soap;

import java.io.*;
import java.util.*;
import javax.activation.*;
import javax.xml.soap.*;
import com.garretwilson.activation.ByteArrayDataSource;
import com.garretwilson.text.xml.XML;
import com.garretwilson.text.xml.XMLNamespaceProcessor;
import com.garretwilson.text.xml.XMLUtilities;
import com.globalmentor.java.*;
import com.globalmentor.util.Debug;

import org.w3c.dom.*;

/**Convenience functions for working with JAXM SOAP objects.
@author Garret Wilson
*/
public class SOAPUtilities
{

	/**This class cannot be publicly instantiated.*/
	private SOAPUtilities()
	{
	}

	/**Creates a SOAP attachment part from an array of bytes as an attachment.
	@param soapMessage The SOAP message for which the attachment will be created.
	@param bytes The bytes to store in the attachment to the SOAP message.
	@return The new attachment that contains the given bytes.
	*/
	public static AttachmentPart createAttachmentPart(final SOAPMessage soapMessage, final byte[] bytes)
	{
						//create a byte input stream from the byte array value
	//G***fix or del				final ByteArrayInputStream resourceStream=new ByteArrayInputStream((byte[])entry.getValue());
						//create a SOAP attachment part with the resource data
	//G***fix				final AttachmentPart attachmentPart=soapMessage.createAttachmentPart(resourceStream, "text/plain"); //G***fix; testing
	//G***fix or del		  final AttachmentPart attachmentPart=soapMessage.createAttachmentPart(resourceStream, MediaType.APPLICATION_OCTET_STREAM);
		final DataSource dataSource=new ByteArrayDataSource(bytes);  //create a data source from the byte array
		final DataHandler dataHandler=new DataHandler(dataSource);  //create a data handler from the data source
		  //create a SOAP attachment part with the resource data
		return soapMessage.createAttachmentPart(dataHandler); //create an attachment using the data handler as input, and return the attachment
	}

	/**Creates a SOAP message and transfers information from the first-level child
		trees of the given XML document.
		The document element of the XML document is ignored.
	@param document The XML document to be converted to a SOAP message.
	@param messageFactory The message factory to use when creating SOAP messages.
	@return The SOAP message created from the XML document.
	*/
	public static SOAPMessage createSOAPMessage(final Document document, final MessageFactory messageFactory)
	{
/*G***del
if(Debug.isDebug());  //G***del
	XMLUtilities.printTree(document, Debug.getOutput());  //G***del
*/
//G***del Debug.trace("creating SOAP message for document: ", XMLUtilities.toString(document)); //G***del
		try
		{
			final SOAPMessage soapMessage=messageFactory.createMessage(); //create a new SOAP message
			final SOAPPart soapPart=soapMessage.getSOAPPart();  //get the SOAP part
			final SOAPEnvelope soapEnvelope=soapPart.getEnvelope(); //get the SOAP envelope
			createSOAPBodyElements(soapEnvelope, document); //create SOAP body elements and add them to the SOAP message
			return soapMessage; //return the message we created
		}
		catch(SOAPException soapException)  //if a SOAP error occurs
		{
			Debug.error(soapException); //G***fix; convert to a DOM exception
			return null;  //G***fix
		}
	}


	/**Creates a SOAP message containing a SOAP fault element from the given
		exception.
	@param genericFaultCode One of generic SOAP fault codes.
	@param exception The exception to convert to a SOAP fault.
	@param messageFactory The message factory to use when creating SOAP messages.
	@return The SOAP message containing the fault object created from the given
		exception.
	@see #createSOAPFault
	*/
	public static SOAPMessage createSOAPFaultMessage(final String genericFaultCode, final Exception exception, final MessageFactory messageFactory)
	{
		try
		{
			final SOAPMessage soapMessage=messageFactory.createMessage(); //create a new SOAP message
			final SOAPPart soapPart=soapMessage.getSOAPPart();  //get the SOAP part
			final SOAPEnvelope soapEnvelope=soapPart.getEnvelope(); //get the SOAP envelope
			final SOAPBody soapBody=soapEnvelope.getBody(); //get the SOAP body
		  createSOAPFault(soapBody, genericFaultCode, exception); //create and add a fault from the exception
			return soapMessage; //return the message we created
		}
		catch(SOAPException soapException)  //if a SOAP error occurs
		{
			Debug.error(soapException); //G***fix; convert to a DOM exception
			return null;  //G***fix
		}
	}

	/**Creates a SOAP fault element from the given exception and adds it to the
		given SOAP body. The exception name will be used to construct a fault code
		in the form "genericFaultCode.exceptionName".
	@param soapBody The body of an existing SOAP message.
	@param genericFaultCode One of generic SOAP fault codes.
	@param exception The exception to convert to a SOAP fault.
	@return The SOAP fault object created from the given exception.
	@exception SOAPException Thrown if there is a problem constructing a SOAP fault.
	*/
	public static SOAPFault createSOAPFault(final SOAPBody soapBody, final String genericFaultCode, final Exception exception) throws SOAPException
	{
		final SOAPFault soapFault=soapBody.addFault();  //create a SOAP fault
		  //create a fault code by combining the generic fault code with the exception class local name
		final String faultCode=genericFaultCode+'.'+Classes.getLocalName(exception.getClass());  //G***use a constant here
		soapFault.setFaultCode(faultCode);  //set the fault code
		soapFault.setFaultString(exception.getMessage()); //set the fault string to the exception message
		return soapFault; //return the fault we created
	}

	/**Converts all first-level child trees of the given document to SOAP elements
		and stores them in the SOAP body of the given SOAP envelope.
		The document element of the XML document is ignored.
	@param soapEnvelope The SOAP envelope that holds the SOAP message body.
	@param document The document to be stored in the SOAP message body.
	*/
	public static	void createSOAPBodyElements(final SOAPEnvelope soapEnvelope, final Document document)
	{
		try
		{
/*G***del
				final Name attributeName=soapEnvelope.createName("foo", null, null); //G***del
Debug.trace("SOAP attribute qname: ", attributeName.getQualifiedName()!=null?attributeName.getQualifiedName():"null");
Debug.trace("SOAP attribute local name: ", attributeName.getLocalName()!=null?attributeName.getLocalName():"null");
Debug.trace("SOAP attribute namespace URI: ", attributeName.getURI()!=null?attributeName.getURI():"null");
Debug.trace("SOAP attribute prefix: ", attributeName.getPrefix()!=null?attributeName.getPrefix():"null");
*/
//G***del Debug.trace("getting SOAP body"); //G***del
			final SOAPBody soapBody=soapEnvelope.getBody(); //get the SOAP body
			createSOAPBodyElements(soapBody, document, soapEnvelope); //create the SOAP body elements
		}
		catch(SOAPException soapException)  //if a SOAP error occurs
		{
			Debug.error(soapException); //G***fix; convert to a DOM exception
		}
	}

	/**Converts all first-level child trees of the given document to SOAP elements
		and stores them in the given SOAP body.
		The document element of the XML document is ignored.
	@param soapBody The body of the SOAP message.
	@param document The document to be stored in the SOAP message body.
	@param soapEnvelope The SOAP envelope to be used as a name factory.
	*/
	public static	void createSOAPBodyElements(final SOAPBody soapBody, final Document document, final SOAPEnvelope soapEnvelope)
	{
//G***del Debug.trace("creating body elements: ", document.getDocumentElement().getChildNodes().getLength()); //G***del
		final NodeList childNodeList=document.getDocumentElement().getChildNodes(); //get a list of first-level child nodes
		for(int i=0; i<childNodeList.getLength(); ++i)  //look at each child node
		{
			final org.w3c.dom.Node childNode=childNodeList.item(i); //get a reference to this node
			if(childNode.getNodeType()==childNode.ELEMENT_NODE) //if this is an element
			{
				createSOAPElement((Element)childNode, soapBody, soapEnvelope);  //create a SOAP element for this DOM element and add it to the SOAP body
			}
		}
	}

	/**Converts all first-level child trees of the given document to SOAP elements
		and stores them in the SOAP header of the given SOAP envelope.
		The document element of the XML document is ignored.
	@param soapEnvelope The SOAP envelope that holds the SOAP message header.
	@param document The document to be stored in the SOAP message header.
	*/
	public static	void createSOAPHeaderElements(final SOAPEnvelope soapEnvelope, final Document document)
	{
		try
		{
			final SOAPHeader soapHeader=soapEnvelope.getHeader(); //get the SOAP header
			createSOAPHeaderElements(soapHeader, document, soapEnvelope); //create the SOAP header elements
		}
		catch(SOAPException soapException)  //if a SOAP error occurs
		{
			Debug.error(soapException); //G***fix; convert to a DOM exception
		}
	}

	/**Converts all first-level child trees of the given document to SOAP elements
		and stores them in the given SOAP header.
		The document element of the XML document is ignored.
	@param soapHeader The header of the SOAP message.
	@param document The document to be stored in the SOAP message header.
	@param soapEnvelope The SOAP envelope to be used as a name factory.
	*/
	public static	void createSOAPHeaderElements(final SOAPHeader soapHeader, final Document document, final SOAPEnvelope soapEnvelope)
	{
//G***del Debug.trace("creating body elements: ", document.getDocumentElement().getChildNodes().getLength()); //G***del
		final NodeList childNodeList=document.getDocumentElement().getChildNodes(); //get a list of first-level child nodes
		for(int i=0; i<childNodeList.getLength(); ++i)  //look at each child node
		{
			final org.w3c.dom.Node childNode=childNodeList.item(i); //get a reference to this node
			if(childNode.getNodeType()==childNode.ELEMENT_NODE) //if this is an element
			{
				createSOAPElement((Element)childNode, soapHeader, soapEnvelope);  //create a SOAP element for this DOM element and add it to the SOAP header
			}
		}
	}

	/**Converts a DOM element to a SOAP element and stores it as a child of the
		given parent SOAP element.
	@param element The DOM element to convert to SOAP.
	@param parentSOAPElement The element to serve as a parent to the SOAP element.
	@param soapEnvelope The SOAP envelope to be used as a name factory.
	@return The SOAP element constructed with the information from the DOM element.
//G***add SOAP and DOM exception reporting documentation
	*/
	public static SOAPElement createSOAPElement(final Element element, final SOAPElement parentSOAPElement, final SOAPEnvelope soapEnvelope)
	{
		try
		{
//G***del Debug.trace("creating SOAP element for element: ", element.getNodeName());  //G***del
			XMLNamespaceProcessor.ensureNamespaceDeclarations(element); //make sure this element's namespaces are all declared
				//create a SOAP element
			final SOAPElement soapElement=parentSOAPElement.addChildElement(element.getLocalName(), element.getPrefix(), element.getNamespaceURI());
			final NamedNodeMap attributeMap=element.getAttributes();  //get a map of DOM attributes
			for(int i=attributeMap.getLength()-1; i>=0; --i)  //look at each attribute in the map (order doesn't matter)
			{
				final Attr attribute=(Attr)attributeMap.item(i);  //get a reference to this attribute
/*G***del
Debug.trace("DOM attribute qname: ", attribute.getNodeName());  //G***del
Debug.trace("DOM attribute local name: ", attribute.getLocalName());  //G***del
Debug.trace("DOM attribute namespace URI: ", attribute.getNamespaceURI());  //G***del
Debug.trace("DOM attribute prefix: ", attribute.getPrefix()); //G***del
Debug.trace("DOM attribute value: ", attribute.getValue()); //G***del
*/

				  //the Spring 2002 XML pack has a SOAP RI that doesn't correctly
					//  process attribute namespaces; add hacks to get around this
				String namespaceURI=attribute.getNamespaceURI();  //get the namespace URI
				if(XML.XMLNS_NAMESPACE_URI.toString().equals(attribute.getNamespaceURI()) //if this is the xmlns namespace (e.g. xmlns:xxx="")
						|| XML.XMLNS_NAMESPACE_PREFIX.equals(attribute.getLocalName()) //or if this is the xmlns attribute being defined (e.g. xmlns="")
						|| attribute.getNamespaceURI()==null)  //if the attribute has no namespace at all
				{
					soapElement.addAttribute(soapEnvelope.createName(XMLUtilities.createQualifiedName(attribute.getPrefix(), attribute.getLocalName())), attribute.getValue());
				}
				else
				{
						//add a SOAP attribute
				  soapElement.addAttribute(soapEnvelope.createName(attribute.getLocalName(), attribute.getPrefix(), attribute.getNamespaceURI()), attribute.getValue());
				}
			}
			final NodeList childNodeList=element.getChildNodes(); //get a list of child nodes
			for(int i=0; i<childNodeList.getLength(); ++i)  //look at each of the child nodes
			{
				final org.w3c.dom.Node childNode=childNodeList.item(i); //get a reference to this child node
				switch(childNode.getNodeType()) //see what type of node this is
				{
					case org.w3c.dom.Node.ELEMENT_NODE: //if the child node is an element
						createSOAPElement((Element)childNode, soapElement, soapEnvelope);  //convert the node to SOAP and add it as a child of the SOAP element
						break;
					case org.w3c.dom.Node.TEXT_NODE: //if the child node is a text node
						soapElement.addTextNode(((org.w3c.dom.Text)childNode).getData()); //add the text to the SOAP element
//G***fix for other node types
				}
			}
			return soapElement; //return the element we constructed
		}
		catch(SOAPException soapException)  //if a SOAP error occurs
		{
			Debug.error(soapException); //G***fix; convert to a DOM exception
			return null;  //G***fix
		}
	}

	/**Converts a SOAP body of a SOAP message to a DOM XML document.
	@param soapMessage The SOAP message that contains the SOAP body to be converted.
	@param domImplementation The DOM implementation to use as a document factory.
	@return The DOM element constructed with the information from the SOAP body
		element.
//G***add SOAP and DOM exception reporting documentation
	*/
	public static Document createBodyDocument(final SOAPMessage soapMessage, final DOMImplementation domImplementation)
	{
		try
		{
			final SOAPPart soapPart=soapMessage.getSOAPPart();  //get the SOAP part
			final SOAPEnvelope soapEnvelope=soapPart.getEnvelope(); //get the envelope
			return createBodyDocument(soapEnvelope, domImplementation); //create an XML document from the SOAP body in the envelope
		}
		catch(SOAPException soapException)  //if a SOAP error occurs
		{
			Debug.error(soapException); //G***fix; convert to a DOM exception
			return null;  //G***fix
		}
	}

	/**Converts a SOAP body to a DOM XML document.
	@param soapEnvelope The SOAP envelope to be used as a name factory and in
		which the SOAP body is located.
	@param domImplementation The DOM implementation to use as a document factory.
	@return The DOM element constructed with the information from the SOAP body
		element.
//G***add SOAP and DOM exception reporting documentation
	*/
	public static Document createBodyDocument(final SOAPEnvelope soapEnvelope, final DOMImplementation domImplementation)
	{
		try
		{
			final SOAPBody soapBody=soapEnvelope.getBody(); //get the SOAP body
			return createDocument(soapBody, domImplementation, soapEnvelope);  //create a document from the SOAP body
		}
		catch(SOAPException soapException)  //if a SOAP error occurs
		{
			Debug.error(soapException); //G***fix; convert to a DOM exception
			return null;  //G***fix
		}
	}

	/**Converts a SOAP header of a SOAP message to a DOM XML document.
	@param soapMessage The SOAP message that contains the SOAP header to be converted.
	@param domImplementation The DOM implementation to use as a document factory.
	@return The DOM element constructed with the information from the SOAP header
		element.
//G***add SOAP and DOM exception reporting documentation
	*/
	public static Document createHeaderDocument(final SOAPMessage soapMessage, final DOMImplementation domImplementation)
	{
		try
		{
			final SOAPPart soapPart=soapMessage.getSOAPPart();  //get the SOAP part
			final SOAPEnvelope soapEnvelope=soapPart.getEnvelope(); //get the envelope
			return createHeaderDocument(soapEnvelope, domImplementation); //create an XML document from the SOAP header in the envelope
		}
		catch(SOAPException soapException)  //if a SOAP error occurs
		{
			Debug.error(soapException); //G***fix; convert to a DOM exception
			return null;  //G***fix
		}
	}

	/**Converts a SOAP header to a DOM XML document.
	@param soapEnvelope The SOAP envelope to be used as a name factory and in
		which the SOAP header is located.
	@param domImplementation The DOM implementation to use as a document factory.
	@return The DOM element constructed with the information from the SOAP header
		element.
//G***add SOAP and DOM exception reporting documentation
	*/
	public static Document createHeaderDocument(final SOAPEnvelope soapEnvelope, final DOMImplementation domImplementation)
	{
		try
		{
			final SOAPHeader soapHeader=soapEnvelope.getHeader(); //get the SOAP header
			return createDocument(soapHeader, domImplementation, soapEnvelope);  //create a document from the SOAP header
		}
		catch(SOAPException soapException)  //if a SOAP error occurs
		{
			Debug.error(soapException); //G***fix; convert to a DOM exception
			return null;  //G***fix
		}
	}





	/**Converts a SOAP body to a DOM XML document.
	@param soapParentElement The SOAP element ody to convert to an XML document.
	@param domImplementation The DOM implementation to use as a document factory.
	@param soapEnvelope The SOAP envelope to be used as a name factory.
	@return The DOM element constructed with the information from the SOAP element.
//G***add SOAP and DOM exception reporting documentation
	*/
	public static Document createDocument(final SOAPElement soapParentElement, final DOMImplementation domImplementation, final SOAPEnvelope soapEnvelope)
	{
		final Name soapParentElementName=soapParentElement.getElementName();  //get the name object of the element body
		//create a qualified name for the document root element
		final String documentElementQualifiedName=XMLUtilities.createQualifiedName(soapParentElementName.getPrefix(), soapParentElementName.getLocalName());
		final Document document=domImplementation.createDocument(soapParentElementName.getURI(), documentElementQualifiedName, null);	//create a document with the same element name as the SOAP body
		final Element rootElement=document.getDocumentElement();	//get the document element
		final Iterator soapElementIterator=soapParentElement.getChildElements(); //get an iterator to all SOAP body elements
		while(soapElementIterator.hasNext())  //while there are more SOAP body elements
		{
			final SOAPElement soapElement=(SOAPElement)soapElementIterator.next();  //get the next SOAP body element
			createElement(soapElement, rootElement, soapEnvelope);  //create a DOM element that represents this SOAP element and add it to our root element
		}
		return document;  //return the document we created
	}

	/**Converts a SOAP element to a DOM element and stores it as a child of the
		given parent DOM element.
	@param soapElement The SOAP element to convert to DOM.
	@param parentElement The element to serve as a parent to the DOM element.
//G***del if not needed	@param document The DOM document to be used as an element factory.
	@param soapEnvelope The SOAP envelope to be used as a name factory.
	@return The DOM element constructed with the information from the SOAP element.
//G***add SOAP and DOM exception reporting documentation
	*/
	public static Element createElement(final SOAPElement soapElement, final Element parentElement, final SOAPEnvelope soapEnvelope)
	{
			//create a DOM element
		final Element element=parentElement.getOwnerDocument().createElementNS(soapElement.getElementName().getURI(), soapElement.getElementName().getQualifiedName());
//G***del Debug.trace("converting attributes for SOAP element: ", soapElement.getElementName().getQualifiedName()); //G***del
		final Iterator attributeIterator=soapElement.getAllAttributes();  //get an iterator to all attributes
		while(attributeIterator.hasNext())  //while there are more attributes
		{
			final Name attributeName=(Name)attributeIterator.next();  //get the next attribute name
			final String attributeValue=soapElement.getAttributeValue(attributeName); //get the value of the attribute
/*G***del
Debug.trace("SOAP attribute qname: ", attributeName.getQualifiedName());
Debug.trace("SOAP attribute local name: ", attributeName.getLocalName());
Debug.trace("SOAP attribute namespace URI: ", attributeName.getURI());
Debug.trace("SOAP attribute prefix: ", attributeName.getPrefix());
*/
				//add an attribute to the DOM element, testing for empty namespace URIs
				//  because the JAXM reference implementation converts all null namespace URIs to the empty string
			element.setAttributeNS(attributeName.getURI().length()>0 ? attributeName.getURI() : null,
				  attributeName.getQualifiedName(),
					attributeValue);
		}
		final Iterator childElementIterator=soapElement.getChildElements(); //get an iterator to all child elements
		while(childElementIterator.hasNext()) //while there are child elements left
		{
			final Object childObject=childElementIterator.next(); //get the next child
			if(childObject instanceof SOAPElement)  //if this child object is a SOAP element
			{
				createElement((SOAPElement)childObject, element, soapEnvelope); //convert the SOAP element to DOM and add it as a child of the DOM element
			}
			else if(childObject instanceof javax.xml.soap.Text)  //if this child object is a Text node
			{
				XMLUtilities.appendText(element, ((javax.xml.soap.Text)childObject).getValue());  //add the text to the DOM element
			}
//G***fix for other node types
		}
/*G***del when works; this looks like it was mistakenly left in
		final NodeList childNodeList=element.getChildNodes(); //get a list of child nodes
		for(int i=0; i<childNodeList.getLength(); ++i)  //look at each of the child nodes
		{
			final org.w3c.dom.Node childNode=childNodeList.item(i); //get a reference to this child node
			switch(childNode.getNodeType()) //see what type of node this is
			{
				case org.w3c.dom.Node.ELEMENT_NODE: //if the child node is an element
					toSOAP((Element)childNode, soapElement, soapEnvelope);  //convert the node to SOAP and add it as a child of the SOAP element
					break;
				case org.w3c.dom.Node.TEXT_NODE: //if the child node is a text node
					soapElement.addTextNode(((org.w3c.dom.Text)childNode).getData()); //add the text to the SOAP element
			}
		}
*/
		parentElement.appendChild(element); //append the element we constructed to the DOM element parent
		return element; //return the element we constructed
	}

	/**Converts a SOAP element to a string. If an error occurs converting the
		element to a string, the normal object string will be returned.
	@param soapElement The SOAP element to convert.
	@return A string representation of the SOAP element.
	*/
/*G***del
	public static String toString(final SOAPElement soapElement)
	{
		try
		{
			final ByteArrayOutputStream outputStream=new ByteArrayOutputStream(); //create an output stream of bytes
			soapElement.
			soapMessage.writeTo(outputStream);  //write the SOAP message to the stream
			return new String(outputStream.toByteArray());  //convert the stream to a string and return it
		}
		catch(IOException ioException)  //if an IO exception occurs
		{
			return soapMessage.toString();  //ask the SOAP message to convert itself to a string
		}
		catch(SOAPException soapException)  //if a SOAP exception occurs
		{
			return soapMessage.toString();  //ask the SOAP message to convert itself to a string
		}
	}
*/

	/**Converts a SOAP message to a string. If an error occurs converting the
		message to a string, the normal object string will be returned.
	@param soapMessage The SOAP message to convert.
	@return A string representation of the SOAP message.
	*/
	public static String toString(final SOAPMessage soapMessage)
	{
		try
		{
			final ByteArrayOutputStream outputStream=new ByteArrayOutputStream(); //create an output stream of bytes
			soapMessage.writeTo(outputStream);  //write the SOAP message to the stream
			return new String(outputStream.toByteArray());  //convert the stream to a string and return it
		}
		catch(IOException ioException)  //if an IO exception occurs
		{
			return soapMessage.toString();  //ask the SOAP message to convert itself to a string
		}
		catch(SOAPException soapException)  //if a SOAP exception occurs
		{
			return soapMessage.toString();  //ask the SOAP message to convert itself to a string
		}
	}

}