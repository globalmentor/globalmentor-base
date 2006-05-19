package com.garretwilson.net.http;

import java.io.*;
import java.net.*;
import java.util.*;

import javax.servlet.http.HttpServletRequest;

import static com.garretwilson.lang.ObjectUtilities.*;
import com.garretwilson.model.*;
import com.garretwilson.net.URIUtilities;
import com.garretwilson.util.CollectionUtilities;
import com.garretwilson.util.Debug;

/**The default implementation of an HTTP servlet that accesses files in the web application.
This servlet may access files within a War file because it uses general servlet routines for resource access.
For this reason the servlet is read-only by default, because the servlet context does not offer any writing methods.
@author Garret Wilson
*/
public class DefaultHTTPServlet extends AbstractHTTPServlet<DefaultHTTPServlet.HTTPServletResource>	//TODO implement writing using the resource URL methods
{

	//TODO fix checks for WEB-INF

	/**Default constructor.
	This servlet defaults to being read-only.
	*/
	public DefaultHTTPServlet()
	{
		setReadOnly(true);	//default to being read-only, because the servlet context only provides read methods
	}

  /**Determines if the resource at a given URI exists.
  @param resourceURI The URI of the requested resource.
  @return <code>true</code> if the resource exists, else <code>false</code>.
	@exception IOException if there is an error accessing the resource.
  */
  protected boolean exists(final URI resourceURI) throws IOException
  {
		final String resourceContextAbsolutePath=getResourceContextAbsolutePath(resourceURI.getPath());	//get the absolute path relative to the context
  	return getServletContext().getResource(resourceContextAbsolutePath)!=null;	//return whether the servlet has mapped a resource to this path
  }

  /**Determines if the resource at a given URI is an existing collection.
  @param resourceURI The URI of the requested resource.
  @return <code>true</code> if the resource is a collection, else <code>false</code>.
	@exception IOException if there is an error accessing the resource.
	@see #exists(URI)
  */
  protected boolean isCollection(final URI resourceURI) throws IOException
  {
		final String resourceContextAbsolutePath=getResourceContextAbsolutePath(resourceURI.getPath());	//get the absolute path relative to the context
		return URIUtilities.isContainerPath(resourceContextAbsolutePath) && exists(resourceURI);	//return whether the context absolute path ends in a slash and the resource exists
  }

	/**Determines the requested resource.
	@param resourceURI The URI of the requested resource.
  @return An object providing an encapsulation of the requested resource,
  	but not necessarily the contents of the resource. 
	@exception IllegalArgumentException if the given resource URI does not represent a valid resource.
	@exception IOException if there is an error accessing the resource.
  */
	protected HTTPServletResource getResource(final URI resourceURI) throws IllegalArgumentException, IOException
	{
		final String resourceContextAbsolutePath=getResourceContextAbsolutePath(resourceURI.getPath());	//get the absolute path relative to the context
		try
		{
			final URL resourceURL=getServletContext().getResource(resourceContextAbsolutePath);	//get the URL of the resource
			return new DefaultHTTPServletResource(resourceURI, resourceURL);	//create a new default resource
		}
		catch(final MalformedURLException malformedURLException)	//if the path was not well-formed
		{
			throw new IllegalArgumentException(malformedURLException);
		}
	}

	/**Determines the content length of the given resource.
	@param request The HTTP request in response to which the content length is being retrieved.
	@param resource The resource for which the content length should be determined.
	@return The content length of the given resource, or <code>-1</code> if no
		content type could be determined.
	@exception IOException Thrown if there is an error accessing the resource.
	*/
	protected long getContentLength(final HttpServletRequest request, final HTTPServletResource resource) throws IOException
	{
		return resource.getContentLength(request);	//return the content length of the resource
	}

	/**Determines the last modified date of the given resource.
	@param request The HTTP request in response to which the last modified date is being retrieved.
	@param resource The resource for which the last modified date should be determined.
	@return The last modified date of the given resource, or <code>null</code> if no there is no known last modified date.
	@exception IOException Thrown if there is an error accessing the resource.
	*/
	protected Date getLastModifiedDate(final HttpServletRequest request, final HTTPServletResource resource) throws IOException
	{
		final long lastModified=resource.getLastModified(request);	//get the last modified information from the resource
		return lastModified>=0 ? new Date(lastModified) : null;	//return the last modified date, if we have that information
	}

	/**Retrieves an input stream to the given resource.
	@param request The HTTP request in response to which the input stream is being retrieved.
	@param resource The resource for which an input stream should be retrieved.
	@return An input stream to the given resource.
	@exception IOException Thrown if there is an error accessing the resource,
		such as a missing file or a resource that has no contents.
	*/
	protected InputStream getInputStream(final HttpServletRequest request, final HTTPServletResource resource) throws IOException
	{
		return resource.getInputStream(request);	//return the input stream to the resource, creating one if we haven't yet done so
	}

	/**Retrieves an output stream to the given resource.
	@param request The HTTP request in response to which the output stream is being retrieved.
	@param resource The resource for which an output stream should be retrieved.
	@return An output stream to the given resource.
	@exception IOException Thrown if there is an error accessing the resource.
	*/
	protected OutputStream getOutputStream(final HttpServletRequest request, final HTTPServletResource resource) throws IOException
	{
		throw new UnsupportedOperationException("DefaultHTTPServlet writing not yet implemented.");
	}

	/**Creates a resource.
	For collections, <code>createCollection</code> should be used instead.
	@param resourceURI The URI of the resource to create.
	@return The description of a newly created resource, or <code>null</code> if
		the resource is not allowed to be created.
	@exception IllegalArgumentException if the given resource URI does not represent a valid resource in a valid burrow.
	@exception IOException Thrown if there is an error creating the resource.
	@exception HTTPConflictException if an intermediate collection required for creating this collection does not exist.
	@see #createCollection(URI)
	*/
	protected HTTPServletResource createResource(final URI resourceURI) throws IllegalArgumentException, IOException, HTTPConflictException
	{
		throw new UnsupportedOperationException("DefaultHTTPServlet writing not yet implemented.");
	}

	/**Creates a collection resource.
	@param resourceURI The URI of the resource to create.
	@return The description of a newly created resource, or <code>null</code> if the resource is not allowed to be created.
	@exception IllegalArgumentException if the given resource URI does not represent a valid resource in a valid burrow.
	@exception IOException Thrown if there is an error creating the resource.
	@exception HTTPConflictException if an intermediate collection required for creating this collection does not exist.
	@see #createResource(URI)
	*/
	protected HTTPServletResource createCollection(final URI resourceURI) throws IllegalArgumentException, IOException, HTTPConflictException
	{
		throw new UnsupportedOperationException("DefaultHTTPServlet writing not yet implemented.");
	}

	/**Deletes a resource.
	@param resource The resource to delete.
	@exception IOException Thrown if the resource could not be deleted.
	*/
	protected void deleteResource(final HTTPServletResource resource) throws IOException
	{
		throw new UnsupportedOperationException("DefaultHTTPServlet writing not yet implemented.");		
	}

	/**Retrieves an list of child resources of the given resource.
	@param resource The resource for which children should be returned.
	@return A list of child resources.
	@exception IOException Thrown if there is an error retrieving the list of child resources.
	*/
	protected List<HTTPServletResource> getChildResources(final HTTPServletResource resource) throws IOException
	{
		return CollectionUtilities.emptyList();	//TODO implement		
//	TODO del when works  	return false;	//TODO fix, noting that getResourcePaths() seems to take a web application-relative path rather than a context-relative path
	}

	/**A resource that can return connections and other information.
	@author Garret Wilson
	*/
	protected interface HTTPServletResource extends Resource
	{

		/**Returns the content length of the resource.
		@param request The HTTP request in response to which the content length is being retrieved.
		@return The content length of the resource.
		@exception IOException if there is an error getting the length of the resource.
		*/
		public long getContentLength(final HttpServletRequest request) throws IOException;

		/**Returns the last modification time of the resource.
		@param request The HTTP request in response to which the last modified time is being retrieved.
		@return The time of last modification as the number of milliseconds since January 1, 1970 GMT.
		@exception IOException if there is an error getting the last modified time.
		*/
		public long getLastModified(final HttpServletRequest request) throws IOException;

		/**Returns an input stream to the resource.
		@param request The HTTP request in response to which the input stream is being retrieved.
		@return The lazily-created input stream to the resource.
		@exception IOException if there is an error getting an input stream to the resource.
		*/
		public InputStream getInputStream(final HttpServletRequest request) throws IOException;
	}

	/**A resource that knows how to retrieve information from a URL.
	@author Garret Wilson
	*/
	protected abstract static class AbstractURLHTTPServletResource extends DefaultResource implements HTTPServletResource	//TODO create a cache of these resources with cached content lengths, etc.; but that would entail checking cache settings and such
	{

		/**The URL of the resource.*/
		private final URL url;

			/**@return The URL of the resource.*/
			protected final URL getURL() {return url;}

		/**The lazily-created URL connection to the resource.*/
		private URLConnection urlConnection=null;

			/**@return The lazily-created URL connection to the resource.
			@exception IOException if there is an error getting a connection to the resource.
			*/
			protected URLConnection getURLConnection() throws IOException
			{
				if(urlConnection==null)	//if we don't yet have a URL connection to the resource
				{
					final URL url=getURL();	//get the resource URL
					assert url!=null : "URL unexpectedly null.";	//TODO check elsewhere to make sure we don't create instances of non-existent resource
					urlConnection=url.openConnection();	//open a connection to the resource
				}
				return urlConnection;	//return the connection we created, or the one we already had
			}

			/**Returns the content length of the resource.
			@param request The HTTP request in response to which the content length is being retrieved.
			@return The content length of the resource.
			@exception IOException if there is an error getting the length of the resource.
			*/
			public long getContentLength(final HttpServletRequest request) throws IOException
			{
				return getURLConnection().getContentLength();	//get a connection to the resource and return the length from the connection
			}

			/**Returns the last modification time of the resource.
			@param request The HTTP request in response to which the last modified time is being retrieved.
			@return The time of last modification as the number of milliseconds since January 1, 1970 GMT.
			@exception IOException if there is an error getting the last modified time.
			*/
			public long getLastModified(final HttpServletRequest request) throws IOException
			{
				return getURLConnection().getLastModified();	//get a connection to the resource and return the last modified information from the connection
			}

		/**The lazily-created input stream to the resource.*/
		private InputStream inputStream=null;

			/**Returns an input stream to the resource.
			@param request The HTTP request in response to which the input stream is being retrieved.
			@return The lazily-created input stream to the resource.
			@exception IOException if there is an error getting an input stream to the resource.
			*/
			public InputStream getInputStream(final HttpServletRequest request) throws IOException	//TODO do we really want to assume there's only one input stream needed to the resource? maybe; probably not
			{
				if(inputStream==null)	//if we don't yet have an input stream to the resource
				{
					inputStream=getURLConnection().getInputStream();	//get an output stream to the resource from its URL connection
				}
				return inputStream;	//return the input stream we created, or the one we already had
			}

		/**Constructs a resource with a reference URI and a URL to connect to.
		@param referenceURI The reference URI for the new resource.
		@param resourceURL The URL to use for connecting to the resource.
		@exception NullPointerException if the reference URI and/or URL <code>null</code>.
		*/
		public AbstractURLHTTPServletResource(final URI referenceURI, final URL resourceURL)
		{
			super(checkInstance(referenceURI, "HTTP resource reference URI cannot be null."));	//construct the parent class
			this.url=checkInstance(resourceURL, "HTTP resource URL cannot be null.");
		}
	}

	/**A resource with associated context-relative absolute path.
	This resource knows how to retrieve information from the servlet web path.
	@author Garret Wilson
	*/
	protected static class DefaultHTTPServletResource extends AbstractURLHTTPServletResource	//TODO create a cache of these resources with cached content lengths, etc.; but that would entail checking cache settings and such
	{

		/**The absolute path of the resource relative to the servlet context.*/
//TODO del if not needed		private final String resourceContextAbsolutePath;

			/**@public The absolute path of the resource relative to the servlet context.*/
//TODO del if not needed			public String getResourceContextAbsolutePath() {return resourceContextAbsolutePath;}

		/**Constructs a resource with a reference URI and a context-relative absolute path.
		@param referenceURI The reference URI for the new resource.
		@param resourceURL The URL to use for connecting to the resource.
		@exception NullPointerException if the reference URI and/or URL <code>null</code>.
		*/
		public DefaultHTTPServletResource(final URI referenceURI, final URL resourceURL)
		{
			super(referenceURI, resourceURL);	//construct the parent class
		}

	}

}
