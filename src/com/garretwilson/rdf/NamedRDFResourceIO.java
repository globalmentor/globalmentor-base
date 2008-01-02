package com.garretwilson.rdf;

import java.io.*;
import java.net.*;

import static com.globalmentor.java.Objects.*;

/**Class for saving and loading a RDF resource by its reference URI.
Whenever an RDF instance is read, a resource will be retrieved using the given reference URI, resolved against the given base URI if one is available.
Resolution of a URI against the base URI is performed according to <a href="http://www.w3.org/TR/2003/PR-rdf-syntax-grammar-20031215/#section-baseURIs">RDF/XML Syntax Specification (Revised) 5.3 Resolving URIs</a>.
@param <T> The type to read and write.
@author Garret Wilson
@see RDFXMLProcessor#resolveURI(URI, URI)
*/
public class NamedRDFResourceIO<T extends RDFResource> extends AbstractRDFXMLIO<T>
{

	/**The unresolved reference URI of the resource supported.*/
	private final URI resourceURI;

		/**@return The unresolved reference URI of the resource supported.*/
		public URI getResourceURI() {return resourceURI;}

	/**Class and resource URI constructor.
	@param resourceClass The class representing the type of resource expected from the RDF instance.
	@param resourceURI The unresolved reference URI of the resource supported..
	@exception NullPointerException if the given class and/or resource URI is <code>null</code>.
	*/
	public NamedRDFResourceIO(final Class<T> resourceClass, final URI resourceURI)
	{
		super(resourceClass);	//construct the parent class
		this.resourceURI=checkInstance(resourceURI, "Resource URI must be provided.");
	}

	/**Reads a resource from an input stream using an existing RDF instance.
	If a base URI is given, the resource URI will be resolved against the base URI before it is used to determine the resource to return.
	@param rdf The RDF instance to use in creating new resources.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The resource read from the input stream.
	@exception NullPointerException if the given RDF instance and/or input stream is <code>null</code>.
	@exception IOException if there is an error reading the data.
	@exception ClassCastException if no appropriate resource factory was installed, and the loaded resource is not of the correct Java class.
	*/ 
	public T read(final RDF rdf, final InputStream inputStream, final URI baseURI) throws IOException
	{
		readRDF(rdf, inputStream, baseURI);	//read RDF from the input stream
		final URI resourceURI=getResourceURI();	//get the URI of the resource to return
		final URI resolvedResourceURI=baseURI!=null ? RDFXMLProcessor.resolveURI(baseURI, resourceURI) : resourceURI;	//resolve the resource URI if possible
		final RDFResource resource=rdf.getResource(resolvedResourceURI);	//look for a resource with the given URI
		if(resource==null)	//if there is no resource
		{
			throw new IOException("No resource found with URI "+resolvedResourceURI+".");
		}
		return getObjectClass().cast(resource);	//cast the resource to the correct type and return it
	}

	/**Writes a resource to an output stream.
	@param outputStream The output stream to which to write the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@param resource The resource to write to the given output stream.
	@throws IOException Thrown if there is an error writing the data.
	*/
	public void write(final OutputStream outputStream, final URI baseURI, final T resource) throws IOException
	{
		writeRDFResource(outputStream, baseURI, resource);	//write the RDF resource as-is
	}

}
