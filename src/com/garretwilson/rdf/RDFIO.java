package com.garretwilson.rdf;

import java.io.*;
import java.net.*;

import com.garretwilson.io.IO;

/**Support for reading or writing a particular type stored in RDF.
@param <T> The type to read and write.
@author Garret Wilson
*/
public interface RDFIO<T> extends IO<T>
{

	/**Reads a resource from an input stream using an existing RDF instance.
	@param rdf The RDF instance to use in creating new resources.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The resource read from the input stream.
	@exception NullPointerException if the given RDF instance and/or input stream is <code>null</code>.
	@exception IOException if there is an error reading the data.
	*/ 
	public T read(final RDF rdf, final InputStream inputStream, final URI baseURI) throws IOException;

}
