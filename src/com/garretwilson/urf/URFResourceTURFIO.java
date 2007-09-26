package com.garretwilson.urf;

import java.io.*;
import java.net.*;

import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.net.URIUtilities.*;

/**Class for saving and loading an URF resource by its reference URI.
Whenever an URF instance is read, a resource will be retrieved using the given URI, resolved against the given base URI if one is available.
Resolution of a URI against the base URI is performed according to TURF resolution rules.
@param <T> The type to read and write.
@author Garret Wilson
*/
public class URFResourceTURFIO<T extends URFResource> extends AbstractTURFIO<T>
{

	/**The unresolved URI of the resource supported.*/
	private final URI resourceURI;

		/**@return The unresolved URI of the resource supported.*/
		public URI getResourceURI() {return resourceURI;}

	/**Class and URI constructor.
	@param resourceClass The class representing the type of resource expected from the URF instance.
	@param resourceURI The unresolved URI of the resource supported..
	@exception NullPointerException if the given class and/or resource URI is <code>null</code>.
	*/
	public URFResourceTURFIO(final Class<T> resourceClass, final URI resourceURI)
	{
		super(resourceClass);	//construct the parent class
		this.resourceURI=checkInstance(resourceURI, "Resource URI must be provided.");
	}

	/**Reads a resource from an input stream using an existing URF instance.
	If a base URI is given, the resource URI will be resolved against the base URI before it is used to determine the resource to return.
	@param urf The URF instance to use in creating new resources.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The resource read from the input stream.
	@exception NullPointerException if the given URF instance and/or input stream is <code>null</code>.
	@exception IOException if there is an error reading the data.
	@exception ClassCastException if no appropriate resource factory was installed, and the loaded resource is not of the correct Java class.
	*/ 
	public T read(final URF urf, final InputStream inputStream, final URI baseURI) throws IOException
	{
		readURF(urf, inputStream, baseURI);	//read URF from the input stream
		final URI resourceURI=getResourceURI();	//get the URI of the resource to return
		final URI resolvedResourceURI=baseURI!=null ? resolve(baseURI, resourceURI) : resourceURI;	//resolve the resource URI if possible
		final URFResource resource=urf.getResource(resolvedResourceURI);	//look for a resource with the given URI
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
		writeURFResource(outputStream, baseURI, resource);	//write the URF resource as-is
	}

}
