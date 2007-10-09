package com.garretwilson.urf;

import java.io.*;
import java.net.*;

/**Class for saving and loading an URF instance as RDF/XML.
@author Garret Wilson
*/
public class DefaultURFRDFXMLIO extends AbstractURFRDFXMLIO<URF>
{

	/**Default constructor.*/
	public DefaultURFRDFXMLIO()
	{
		super(URF.class);	//construct the parent class
	}

	/**Reads URF from an input stream using an existing URF instance.
	@param urf The URF instance to use in creating new resources.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The URF instance read from the input stream.
	@exception NullPointerException if the given URF instance and/or input stream is <code>null</code>.
	@exception IOException if there is an error reading the data.
	@exception ClassCastException if no appropriate resource factory was installed, and the loaded resource is not of the correct Java class.
	*/ 
	public URF read(final URF urf, final InputStream inputStream, final URI baseURI) throws IOException
	{
		return readURF(urf, inputStream, baseURI);	//read URF from the input stream and return it
	}

	/**Writes URF to an output stream.
	This implementation does not support this method
	@param outputStream The output stream to which to write the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@param urf The URF instance to write to the given output stream.
	@throws IOException Thrown if there is an error writing the data.
	*/
	public void write(final OutputStream outputStream, final URI baseURI, final URF urf) throws IOException
	{
		throw new UnsupportedOperationException("Writing URF as RDF/XML is not yet supported.");
	}

}
