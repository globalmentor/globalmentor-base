package com.garretwilson.rdf;

import java.io.*;
import java.net.*;
import com.garretwilson.io.*;
import com.garretwilson.rdf.*;
import com.garretwilson.text.xml.XMLDOMImplementation;
import com.garretwilson.text.xml.XMLSerializer;
import com.garretwilson.util.*;
import org.w3c.dom.*;

/**A modifiable object that knows how to store and retrieve itself as RDF.
<p>The data storage and retrieval operations of this class are thread-safe.</p>
<p>Bound properties:</p>
<ul>
	<li><code>Modifiable.MODIFIED_PROPERTY_NAME</code> ("modified")</li>
</ul>
@author Garret Wilson
@see Modifiable#MODIFIED_PROPERTY_NAME
*/
public abstract class AbstractRDFStorage extends DefaultModifiable implements URIInputStreamable, URIOutputStreamable 
{
	/**The location where the RDF should be stored.*/
	private final URI storageURI;
	
		/**@return The location where the RDF should be stored.*/
		public URI getStorageURI() {return storageURI;}

	/**Storage location constructor.
	@param storageURI The location where the RDF should be stored.
	*/
	public AbstractRDFStorage(final URI storageURI)
	{
		this.storageURI=storageURI;	//save the storage URI
	}
	
	/**@return An XML serializer appropriately configured for storing the RDF XML.
	@see RDFXMLifier#registerNamespacePrefix
	*/
	public RDFXMLifier getRDFXMLifier()
	{
		return new RDFXMLifier();	//create an object to convert the RDF data model to an XML data model
	}
	
	/**@return An XML serializer appropriately configured for storing the RDF XML.*/
	public XMLSerializer getXMLSerializer()
	{
		return new XMLSerializer(true);	//create a formatted XML serializer
	}

	/**@return The RDF data model that represents the information.*/
	public abstract RDF getRDF();

	/**Stores the information in RDF at the storage URI.
	@exception IOException Thrown if there is a problem storing the information.
	@see #getStorageURI()
	*/
	public void store() throws IOException
	{
		final RDF rdf=getRDF();	//get the RDF data model representing the data
			//create an XML document from the RDF
		final Document document=getRDFXMLifier().createDocument(rdf, new XMLDOMImplementation());  //G***try to make this XML parser agnostic
		store(document);	//store the XML document at the storage URI
		setModified(false);	//show that we are no longer modified
	}

	/**Stores the RDF information at the storage URI.
	@param document The XML document that contains the RDF data to store
	@exception IOException Thrown if there is a problem storing the information.
	@see #getStorageURI()
	*/
	protected void store(final Document document) throws IOException
	{
		store(document, getStorageURI());	//store the information at the storage URI
	}

	/**Stores the RDF information at the given URI.
	@param document The XML document that contains the RDF data to store
	@param uri The URI at which the information should be stored
	@exception IOException Thrown if there is a problem storing the information.
	@see #getStorageURI()
	*/
	protected synchronized void store(final Document document, final URI uri) throws IOException
	{
		final XMLSerializer xmlSerializer=getXMLSerializer();	//get an XML serializer
		final OutputStream outputStream=new BufferedOutputStream(getOutputStream(uri));	//get a buffered output stream to the URI
		try
		{
			xmlSerializer.serialize(document, outputStream);	//serialize the XML document to the output stream	
		}
		finally
		{
			outputStream.close();	//always close the output stream
		}
	}
	
}
