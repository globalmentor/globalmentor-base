package com.garretwilson.rdf;

import java.io.*;
import java.net.*;

import com.globalmentor.io.IO;

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

	/**Registers a resource factory to be used to create resources with a type from the specified namespace. If a resource factory is already registered for this namespace, it will be replaced.
	@param typeNamespaceURI The namespace of the resource type for which this factory should be used to create objects.
	@param factory The resource factory that will be used to create resources of types from this namespace.
	*/
	public void registerResourceFactory(final URI typeNamespaceURI, final RDFResourceFactory factory);

	/**Removes the resource factory being used to create resources with a type from the specified namespace. If there is no resource factory registered for this namespace, no action will be taken.
	@param typeNamespaceURI The namespace of the resource type for which this factory should be used to create objects.
	*/
	public void unregisterResourceFactory(final URI typeNamespaceURI);

}
