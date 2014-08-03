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

import java.io.*;
import java.util.*;

import javax.xml.soap.*;

import com.globalmentor.io.InputStreams;
import com.globalmentor.log.Log;
import com.globalmentor.model.BinaryObjectHolder;
import com.globalmentor.text.xml.XMLObjectDecoder;
import com.globalmentor.text.xml.soap.SOAP;

import org.w3c.dom.*;

/**
 * An object factory to create objects from SOAP messages. Most XML decoding is performed in the XML decoder, but special decoding is performed here for the
 * first <code>BinaryObjectHolder</code>; for that object, each attachment is stored as a byte array.
 * @author Garret Wilson
 * @see BinaryObjectHolder
 */
public class SOAPObjectDecoder extends XMLObjectDecoder //TODO fix resource message attachment stuff
{

	/**
	 * Decodes an array of objects from a SOAP message.
	 * @param soapMessage The SOAP message to decode.
	 * @param domImplementation The DOMImplementation to use in processing the SOAP message.
	 * @return An array of objects, each representing a SOAP body element or <code>null</code> if a SOAP body element was not recognized. //TODO add the DOM
	 *         exceptions
	 */
	public Object[] decode(final SOAPMessage soapMessage, final DOMImplementation domImplementation) {
		//convert the SOAP message into a DOM XML document
		final Document document = SOAP.createBodyDocument(soapMessage, domImplementation);
		final Object[] objects = decode(document); //decode an array of objects from the document
		//extract any attachments for the first binary object holder encountered
		for(int i = 0; i < objects.length; ++i) { //look at each object
			try {
				if(objects[i] instanceof BinaryObjectHolder) { //if the first message is a binary object holder
					//add all the attachments to the same binary object holder
					final BinaryObjectHolder binaryObjectHolder = (BinaryObjectHolder)objects[i]; //cast the first message to a binary object holder
					final Iterator attachmentIterator = soapMessage.getAttachments(); //get an iterator to all the SOAP attachments
					while(attachmentIterator.hasNext()) { //while there are more attachments
						final AttachmentPart attachmentPart = (AttachmentPart)attachmentIterator.next(); //get the next attachment
						final String id = attachmentPart.getContentId(); //get the ID of this attachment, which should be the resource reference URI
						final InputStream attachmentInputStream = (InputStream)attachmentPart.getContent(); //get the content of the resource as an input stream
						//TODO close the attachment input stream
						final byte[] bytes = InputStreams.getBytes(attachmentInputStream); //get the attachmentn from the stream as a sequence of bytes
						//TODO do we need to close the attachment input stream?
						binaryObjectHolder.getBinaryObjectMap().put(id, bytes); //store the binary object bytes in the message, keyed to the ID
						break; //we've added all the attachments to the same binary object holder; stop looking for binary object holders
					}
				}
			} catch(IOException ioException) { //if an I/O error occurs
				Log.error(ioException); //TODO fix;
			} catch(SOAPException soapException) { //if a SOAP error occurs
				Log.error(soapException); //TODO fix;
			}
		}
		return objects; //return the decoded objects
	}

}