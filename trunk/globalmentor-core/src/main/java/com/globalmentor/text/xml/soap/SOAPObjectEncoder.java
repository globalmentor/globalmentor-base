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

package com.globalmentor.text.xml.soap;

import java.util.*;

import javax.xml.soap.*;

import com.globalmentor.model.BinaryObjectHolder;
import com.globalmentor.text.xml.XMLObjectEncoder;
import com.globalmentor.text.xml.soap.SOAP;

import org.w3c.dom.*;

/**
 * A SOAP factory to create SOAP messages from objects. Most XML encoding is performed in the XML encoder, but special decoding is performed here for each
 * object that is a <code>BinaryObjectHolder</code>; for each such object, its binary objects are stored as SOAP attachments.
 * @author Garret Wilson
 * @see BinaryObjectHolder
 */
public class SOAPObjectEncoder extends XMLObjectEncoder //TODO fix resource message attachment stuff
{

	/**
	 * Encodes an object to a SOAP message.
	 * @param object The object to encode.
	 * @param domImplementation The DOMImplementation to be used as an element factory.
	 * @param messageFactory The SOAP message factory to be used to create messages.
	 * @return A SOAP message containing the given object encoded in XML. //TODO add the DOM exceptions
	 */
	public SOAPMessage encode(final Object object, final DOMImplementation domImplementation, final MessageFactory messageFactory) {
		return encode(new Object[] { object }, domImplementation, messageFactory); //store the object in an array and encode the array of objects in a SOAP message, returning the SOAP message
	}

	/**
	 * Encodes an array of objects to a SOAP message.
	 * @param objects The objects to encode.
	 * @param domImplementation The DOMImplementation to be used as an element factory.
	 * @param messageFactory The SOAP message factory to be used to create messages.
	 * @return A SOAP message containing the given objects encoded in XML. //TODO add the DOM exceptions
	 */
	public SOAPMessage encode(final Object[] objects, final DOMImplementation domImplementation, final MessageFactory messageFactory) {
		final Document document = encode(objects, domImplementation); //convert the objects to an XML document
		//convert the XML document to a SOAP message
		final SOAPMessage soapMessage = SOAP.createSOAPMessage(document, messageFactory);
		//add any attachments from all objects that hold binary objects
		for(int i = 0; i < objects.length; ++i) { //look at each object
			if(objects[i] instanceof BinaryObjectHolder) { //if this object is a binary object holder
				final BinaryObjectHolder binaryObjectHolder = (BinaryObjectHolder)objects[i]; //cast the object to a binary object holder
				final Iterator entryIterator = binaryObjectHolder.getBinaryObjectMap().entrySet().iterator(); //look at all the entries in the binary object map
				while(entryIterator.hasNext()) { //while there are more binary objects
					final Map.Entry entry = (Map.Entry)entryIterator.next(); //get the next map entry
					final String id = entry.getKey().toString(); //store the string value of the key as the attachment ID
					final byte[] bytes = (byte[])entry.getValue(); //get the bytes from this entry
					//create an attachment part containing the bytes of this binary object
					final AttachmentPart attachmentPart = SOAP.createAttachmentPart(soapMessage, bytes);
					attachmentPart.setContentId(id); //set the content ID to the binary object ID
					soapMessage.addAttachmentPart(attachmentPart); //add the attachment part to the message
				}
			}
		}
		return soapMessage; //return the SOAP message we created
	}

}