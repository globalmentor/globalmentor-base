package com.garretwilson.io;

import java.io.*;
import java.net.URI;

/**Default implementation of a class that allows access to resources by
	providing input streams and indicating a base URI against which relative URIs
	should be resolved.
@author Garret Wilson
*/
public class DefaultURIAccessible implements URIAccessible 
{

	/**The lazily-created singleton default instance of a default implementation with no base URI and default stream access.*/
	private static URIAccessible defaultURIAccessible=null;

		/**@return The lazily-created singleton default instance of a default implementation with no base URI and default stream access.*/
		public static URIAccessible getDefaultURIAccessible()
		{
			if(defaultURIAccessible==null)	//if the default URI accessible object has not yet been created
			{
				defaultURIAccessible=new DefaultURIAccessible();	//create a default instance of the default class
			}
			return defaultURIAccessible;	//return the default object
		}

	/**The base URI of the model, or <code>null</code> if unknown.*/
	private final URI baseURI;
	
		/**@return The base URI of the model, or <code>null</code> if unknown.*/
		public URI getBaseURI() {return baseURI;}

	/**The implementation to use for retrieving an input stream to a URI.*/
	private final URIInputStreamable uriInputStreamable;

		/**@return The non-<code>null</code> implementation to use for retrieving an input stream to a URI.*/
		public URIInputStreamable getURIInputStreamable() {return uriInputStreamable;}

	/**Default constructor.*/
	public DefaultURIAccessible()
	{
		this((URI)null);
	}

	/**Base URI constructor.
	@param baseURI The base URI, or <code>null</code> if unknown.
	*/
	public DefaultURIAccessible(final URI baseURI)
	{
		this(baseURI, null);
	}

	/**URI input stream locator constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	*/
	public DefaultURIAccessible(final URIInputStreamable uriInputStreamable)
	{
		this(null, uriInputStreamable);
	}

	/**Full constructor.
	@param baseURI The base URI, or <code>null</code> if unknown.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	 */
	public DefaultURIAccessible(final URI baseURI, final URIInputStreamable uriInputStreamable)
	{
		this.baseURI=baseURI;	//save the base URI
		this.uriInputStreamable=uriInputStreamable!=null ? uriInputStreamable : this;	//save the URI input stream locator, using our default if one was not given
	}
		
	/**Returns an input stream for the given URI.
	The calling class has the responsibility for closing the input stream.
	@param uri A URI to a resource.
	@return An input stream to the contents of the resource represented by the given URI.
	@exception IOException Thrown if an I/O error occurred.
	*/
	public InputStream getInputStream(final URI uri) throws IOException
	{
		return uri.toURL().openConnection().getInputStream();	//convert the URI to a URL, open a connection to it, and get an input stream to it
	}

}
