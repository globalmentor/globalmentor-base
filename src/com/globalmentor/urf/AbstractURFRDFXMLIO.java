package com.globalmentor.urf;

import java.io.*;
import java.net.*;

import javax.xml.parsers.*;

import com.garretwilson.text.xml.XMLUtilities;
import com.garretwilson.util.DataException;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**Base functionality for loading and saving URF information stored in RDF/XML.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@param <T> The type to read and write.
@author Garret Wilson
*/
public abstract class AbstractURFRDFXMLIO<T> extends AbstractURFIO<T>
{

	/**Class constructor.
	@param objectClass The class representing the type of object being loaded and saved.
	@exception NullPointerException if the given class is <code>null</code>.
	*/
	public AbstractURFRDFXMLIO(final Class<T> objectClass)
	{
		super(objectClass);	//construct the parent class
	}

	/**Creates a document builder appropriate for parsing XML storing RDF.
	@return A new namespace-aware document builder.
	@exception ParserConfigurationException if a document builder cannot be created which satisfies the configuration requested.
	*/
	protected DocumentBuilder createDocumentBuilder() throws ParserConfigurationException
	{
		return XMLUtilities.createDocumentBuilder(true);	//create a document builder that understands namespaces
	}

	/**Reads URF data from an input stream.
	This implementation reads RDF/XML.
	@param urf The URF instance to use in creating new resources.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The URF instance representing the data read.
	@exception IOException if there is an error reading the data.
	@see #readRDFXML(URF, InputStream, URI)
	*/ 
	protected URF readURF(final URF urf, InputStream inputStream, final URI baseURI) throws IOException
	{
		return readRDFXML(urf, inputStream, baseURI);	//read the RDF/XML
	}

	/**Reads URF data from an RDF/XML input stream using a default URF data model.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The URF instance representing the data read.
	@exception IOException if there is an error reading the data.
	*/ 
	public URF readRDFXML(final InputStream inputStream, final URI baseURI) throws IOException
	{
		return readRDFXML(new URF(), inputStream, baseURI);	//read RDF/XML using a default URF data model		
	}

	/**Reads URF data from an RDF/XML input stream.
	@param urf The URF instance to use in creating new resources.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The URF instance representing the data read.
	@exception IOException if there is an error reading the data.
	*/ 
	public URF readRDFXML(final URF urf, final InputStream inputStream, final URI baseURI) throws IOException
	{
		return readRDFXML(new URFRDFXMLProcessor(urf), inputStream, baseURI);	//create a new URF processor and process the data
	}

	/**Reads URF data from an RDF/XML input stream using an existing URF processor.
	@param urfProcessor The URF processor for processing the data.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The URF instance representing the data read.
	@exception IOException if there is an error reading the data.
	*/ 
	public URF readRDFXML(final URFRDFXMLProcessor urfProcessor, final InputStream inputStream, final URI baseURI) throws IOException
	{
		try
		{
			final DocumentBuilder documentBuilder=createDocumentBuilder();	//create a new namespace-aware document builder
			final Document document=documentBuilder.parse(inputStream);	//parse the input stream
			document.normalize(); //normalize the document
			return urfProcessor.processRDF(document, baseURI);  //parse the RDF from the document into URF and return the URF
		}
		catch(final ParserConfigurationException parserConfigurationException)	//if we can't find an XML parser
		{
			throw (IOException)new IOException(parserConfigurationException.getMessage()).initCause(parserConfigurationException);	//convert the exception into an IO exception
		}
		catch(final SAXException saxException)
		{
			throw (IOException)new IOException(saxException.getMessage()).initCause(saxException);	//convert the exception into an IO exception
		}
		catch(final DataException dataException)	//if any of the data was incorrect
		{
			throw (IOException)new IOException(dataException.getMessage()).initCause(dataException);	//convert the exception into an IO exception
		}
	}

	/**Writes an URF resource to an output stream.
	This implementation does not support this method
	@param outputStream The output stream to which to write the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@param resource The resource to write to the given output stream.
	@throws IOException Thrown if there is an error writing the data.
	*/
	protected void writeURFResource(final OutputStream outputStream, final URI baseURI, final URFResource resource) throws IOException
	{
		throw new UnsupportedOperationException("Writing URF as RDF/XML is not yet supported.");
	}

}
