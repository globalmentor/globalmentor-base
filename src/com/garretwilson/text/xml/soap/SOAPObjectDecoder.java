package com.garretwilson.text.xml.soap;

import java.io.*;
import java.util.*;
import javax.xml.soap.*;
import com.garretwilson.io.InputStreamUtilities;
import com.garretwilson.text.xml.XMLObjectDecoder;
import com.garretwilson.text.xml.XMLUtilities;
import com.garretwilson.text.xml.soap.SOAPUtilities;
import com.garretwilson.util.BinaryObjectHolder;
import com.garretwilson.util.Debug;
import org.w3c.dom.*;

/**An object factory to create objects from SOAP messages.
	Most XML decoding is performed in the XML decoder, but special decoding is
	performed here for the first <code>BinaryObjectHolder</code>; for that object,
	each attachment is stored as a byte array.
@author Garret Wilson
@see BinaryObjectHolder
*/
public class SOAPObjectDecoder extends XMLObjectDecoder  //G***fix resource message attachment stuff
{

	/**Decodes an array of objects from a SOAP message.
	@param soapMessage The SOAP message to decode.
	@param domImplementation The DOMImplementation to use in processing the SOAP
		message.
	@return An array of objects, each representing a SOAP body element or
		<code>null</code> if a SOAP body element was not recognized.
	//G***add the DOM exceptions
	*/
	public Object[] decode(final SOAPMessage soapMessage, final DOMImplementation domImplementation)
	{
if(Debug.isDebug()) //G***del
	Debug.trace("Decoding SOAP objects: ", SOAPUtilities.toString(soapMessage));  //G***del
		  //convert the SOAP message into a DOM XML document
		final Document document=SOAPUtilities.createBodyDocument(soapMessage, domImplementation);
/*G***del
if(Debug.isDebug());  //G***del
	XMLUtilities.printTree(document, Debug.getOutput());  //G***del
*/

/*G***del
if(Debug.isDebug()) //G***del
{
		try //G***del
		{
Debug.trace("decoded SOAP message as DOM:");  //G***del all this; testing
			final java.io.ByteArrayOutputStream baos=new java.io.ByteArrayOutputStream();
			new com.garretwilson.text.xml.XMLSerializer(false).serialize(document, baos);
			Debug.trace(new String(baos.toByteArray()));  //G***del
		  com.garretwilson.text.xml.XMLUtilities.printTree(document, Debug.getOutput());
		}
		catch(Exception e) {Debug.error(e);}
}
*/
		final Object[] objects=decode(document);  //decode an array of objects from the document
				//extract any attachments for the first binary object holder encountered
		for(int i=0; i<objects.length; ++i) //look at each object
		{
			try
			{
				if(objects[i] instanceof BinaryObjectHolder) //if the first message is a binary object holder
				{
						//add all the attachments to the same binary object holder
	//G***del Debug.trace("decoding binary object holder"); //G***del
					final BinaryObjectHolder binaryObjectHolder=(BinaryObjectHolder)objects[i];  //cast the first message to a binary object holder
					final Iterator attachmentIterator=soapMessage.getAttachments(); //get an iterator to all the SOAP attachments
					while(attachmentIterator.hasNext()) //while there are more attachments
					{
	//G***del Debug.trace("found attachment"); //G***del
						final AttachmentPart attachmentPart=(AttachmentPart)attachmentIterator.next(); //get the next attachment
						final String id=attachmentPart.getContentId();  //get the ID of this attachment, which should be the resource reference URI
	//G***del Debug.trace("attachment part ID: ", id); //G***del
						final InputStream attachmentInputStream=(InputStream)attachmentPart.getContent();  //get the content of the resource as an input stream
	//G***close the attachment input stream
						final byte[] bytes=InputStreamUtilities.getBytes(attachmentInputStream); //get the attachmentn from the stream as a sequence of bytes
						//G***do we need to close the attachment input stream?
						binaryObjectHolder.getBinaryObjectMap().put(id, bytes);  //store the binary object bytes in the message, keyed to the ID
						break;  //we've added all the attachments to the same binary object holder; stop looking for binary object holders
					}
				}
			}
			catch(IOException ioException)  //if an I/O error occurs
			{
				Debug.error(ioException); //G***fix;
			}
			catch(SOAPException soapException)  //if a SOAP error occurs
			{
				Debug.error(soapException); //G***fix;
			}
		}
		return objects; //return the decoded objects
	}

}