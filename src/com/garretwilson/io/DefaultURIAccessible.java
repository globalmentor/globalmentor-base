package com.garretwilson.io;

import java.io.*;
import java.net.URI;
import com.garretwilson.net.URIConstants;
import com.garretwilson.util.BoundPropertyObject;

/**Default implementation of a class that allows access to resources by
	providing input streams and indicating a base URI against which relative URIs
	should be resolved.
@author Garret Wilson
*/
public class DefaultURIAccessible extends BoundPropertyObject implements URIAccessible 
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

	/**The implementation to use for retrieving an input stream to a URI.*/
	private final URIInputStreamable uriInputStreamable;

		/**@return The non-<code>null</code> implementation to use for retrieving an input stream to a URI.*/
//G***fix		public URIInputStreamable getURIInputStreamable() {return uriInputStreamable;}	//TODO refactor these into a ProxyURIAccessible

	/**The implementation to use for retrieving an output stream to a URI.*/
	private final URIOutputStreamable uriOutputStreamable;

		/**@return The non-<code>null</code> implementation to use for retrieving an output stream to a URI.*/
//G***fix		public URIOutputStreamable getURIOutputStreamable() {return uriOutputStreamable;}

	/**Default constructor.*/
	public DefaultURIAccessible()
	{
		this(null, null);
	}

	/**URI input stream locator constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	*/
	public DefaultURIAccessible(final URIInputStreamable uriInputStreamable)
	{
		this(uriInputStreamable, null);
	}

	/**URI output stream locator constructor.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public DefaultURIAccessible(final URIOutputStreamable uriOutputStreamable)
	{
		this(null, uriOutputStreamable);
	}

	/**Full constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public DefaultURIAccessible(final URIInputStreamable uriInputStreamable, final URIOutputStreamable uriOutputStreamable)
	{
		this.uriInputStreamable=uriInputStreamable;	//save the URI input stream locator
		this.uriOutputStreamable=uriOutputStreamable;	//save the URI output stream locator
/*G***del when works
		this.uriInputStreamable=uriInputStreamable!=null ? uriInputStreamable : this;	//save the URI input stream locator, using our default if one was not given
		this.uriOutputStreamable=uriOutputStreamable!=null ? uriOutputStreamable : this;	//save the URI output stream locator, using our default if one was not given
*/
	}
		
	/**Returns an input stream for the given URI.
	The calling class has the responsibility for closing the input stream.
	@param uri A URI to a resource.
	@return An input stream to the contents of the resource represented by the given URI.
	@exception IOException Thrown if an I/O error occurred.
	*/
	public InputStream getInputStream(final URI uri) throws IOException
	{
		if(uriInputStreamable!=null)	//if we have a delegate input streamable
		{
			return uriInputStreamable.getInputStream(uri);	//delegate to the stored implementation
		}
		return uri.toURL().openConnection().getInputStream();	//convert the URI to a URL, open a connection to it, and get an input stream to it
	}

	/**Returns an output stream for the given URI.
	The calling class has the responsibility for closing the output stream.
	@param uri A URI to a resource.
	@return An output stream to the contents of the resource represented by the given URI.
	@exception IOException Thrown if an I/O error occurred.
	*/
	public OutputStream getOutputStream(final URI uri) throws IOException
	{
		if(uriOutputStreamable!=null)	//if we have a delegate output streamable
		{
			return uriOutputStreamable.getOutputStream(uri);	//delegate to the stored implementation
		}
		if(URIConstants.FILE_SCHEME.equals(uri.getScheme()))	//if this is a file URI
		{
			return new FileOutputStream(new File(uri));	//create and return an output stream to the file
		}
//TODO fix for other types of URIs
		return uri.toURL().openConnection().getOutputStream();	//convert the URI to a URL, open a connection to it, and get an output stream to it
	}

}
