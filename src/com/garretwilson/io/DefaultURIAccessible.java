package com.garretwilson.io;

import java.io.*;
import java.net.URI;

import org.apache.commons.httpclient.HttpURL;
import org.apache.webdav.lib.WebdavResource;

import com.garretwilson.util.BoundPropertyObject;
import com.garretwilson.util.Debug;

import static com.garretwilson.net.URIConstants.*;

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
		final String scheme=uri.getScheme();	//see what type of URI this is
		if(FILE_SCHEME.equals(scheme))	//if this is a file URI
		{
			return new FileOutputStream(new File(uri));	//create and return an output stream to the file
		}
		else if(HTTP_SCHEME.equals(scheme) || HTTPS_SCHEME.equals(scheme))	//if this is an HTTP URI, try to use WebDAV
		{
			final HttpURL httpURL=new HttpURL(uri.toString());	//create an Apache HTTP URL from the URI TODO make sure the string is properly escaped
			final WebdavResource webdavResource=new WebdavResource(httpURL);	//create a WebDAV resource to the URI
			return new WebdavResourceOutputStreamAdapter(webdavResource);	//adapt the resource to an output stream and return it
		}
//TODO fix for other types of URIs
		return uri.toURL().openConnection().getOutputStream();	//convert the URI to a URL, open a connection to it, and get an output stream to it
	}


	/**Creates an output stream that simply collects bytes until closed,
	 	at which point the data is written to the WebDAV resource.
	@author Garret Wilson
	*/
	protected static class WebdavResourceOutputStreamAdapter extends OutputStreamDecorator<ByteArrayOutputStream>	//TODO maybe move this to its own independt class at some point
	{
		
		/**The WebDAV resource being adapted.*/
		protected WebdavResource webdavResource=null;
		
		/**Constructor that adapts a WebDAV resource.
		@param webdavResource The WebDAV resource to adapt to an output stream.
		*/
		public WebdavResourceOutputStreamAdapter(final WebdavResource webdavResource)
		{
			super(new ByteArrayOutputStream());	//collect bytes in a decorated byte array output stream
			this.webdavResource=webdavResource;		//save the WebDAV resource we'll be writing to
		}
		
		//TODO improve flush() at some point to actually send data to the Webdav Resource
		
	  /**Closes this output stream and releases any system resources associated with this stream.
	  This version writes the accumulated data to the WebDAV Resource.
	  @exception  IOException  if an I/O error occurs.
	  */
	  public void close() throws IOException
		{
	  	if(webdavResource!=null)	//if we have a WebDAV resource object to write to (i.e. we haven't closed the adapter stream, yet)
	  	{
	  		try
	  		{
	  			final ByteArrayOutputStream byeAtrrayOutputStream=getOutputStream();	//get the decorated output stream
	  			super.close();	//do the default closing, releasing the decorated output stream from the decorator
	  			final byte[] bytes=byeAtrrayOutputStream.toByteArray();	//get the collected bytes (use our reference of the decorated output stream, because it will have been released by the decorator at this point) 
	  			try
	  			{
	  				webdavResource.putMethod(bytes);	//put the bytes to the WebDAV resource
	  			}
	  			finally
					{
	  				webdavResource.close();	//close the WebDAV resource
					}
	  		}
	  		finally
				{
	  			webdavResource=null;	//show that we don't have a WebDAV resource object to write to anymore
				}
	  	}
		}		
	}
}
