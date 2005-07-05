package com.garretwilson.net.http;

import java.io.*;
import java.net.*;
import java.util.*;

import com.garretwilson.io.FileResource;
import static com.garretwilson.io.FileUtilities.*;
import com.garretwilson.net.URIUtilities;

/**The default implementation of an HTTP servlet that accesses files in the web application directory.
@author Garret Wilson
*/
public class DefaultHTTPServlet extends AbstractHTTPServlet<FileResource>
{

  /**Determines if the resource at a given URI exists.
  @param resourceURI The URI of the requested resource.
  @return <code>true</code> if the resource exists, else <code>false</code>.
	@exception IOException if there is an error accessing the resource.
  */
  protected boolean exists(final URI resourceURI) throws IOException
  {
  	return getResource(resourceURI).getFile().exists();	//return whether the file exists
  }

  /**Determines if the resource at a given URI is a collection.
  @param resourceURI The URI of the requested resource.
  @return <code>true</code> if the resource is a collection, else <code>false</code>.
	@exception IOException if there is an error accessing the resource.
  */
  protected boolean isCollection(final URI resourceURI) throws IOException
  {
  	return getResource(resourceURI).getFile().isDirectory();	//return whether the file is a directory
  }

	/**Determines the requested resource.
	@param resourceURI The URI of the requested resource.
  @return An object providing an encapsulation of the requested resource,
  	but not necessarily the contents of the resource. 
	@exception IllegalArgumentException if the given resource URI does not represent a valid resource.
	@exception IOException if there is an error accessing the resource.
  */
	protected FileResource getResource(final URI resourceURI) throws IllegalArgumentException, IOException
	{
		final String contextPath=getContextPath();	//get the servlet context path
		final String path=resourceURI.getPath();	//get the path of the resource URI
		if(!path.startsWith(contextPath))	//if the path does not start with the context path
		{
			throw new IllegalArgumentException("Resource URI "+resourceURI+" path "+path+" is not located under context path "+contextPath);
		}
		final String contextRelativePath=URIUtilities.getRelativePath(path.substring(contextPath.length()));	//remove the context path and then make the path relative
		return new FileResource(new File(getServletContext().getRealPath(contextRelativePath)), resourceURI);	//create a file to the real path in the file system
	}

	/**Determines the content length of the given resource.
	@param resource The resource for which the content length should be determined.
	@return The content length of the given resource, or <code>-1</code> if no
		content type could be determined.
	*/
	protected long getContentLength(final FileResource resource)
	{
		return resource.getFile().length();	//return the length of the file
	}

	/**Retrieves an input stream to the given resource.
	@param resource The resource for which an input stream should be retrieved.
	@return An input stream to the given resource.
	@exception IOException Thrown if there is an error accessing the resource,
		such as a missing file or a resource that has no contents.
	*/
	protected InputStream getInputStream(final FileResource resource) throws IOException
	{
		return new BufferedInputStream(new FileInputStream(resource.getFile()));	//return a buffered input stream from the file
	}

	/**Retrieves an output stream to the given resource.
	@param resource The resource for which an output stream should be retrieved.
	@return An output stream to the given resource.
	@exception IOException Thrown if there is an error accessing the resource.
	*/
	protected OutputStream getOutputStream(final FileResource resource) throws IOException
	{
		return new BufferedOutputStream(new FileOutputStream(resource.getFile()));	//return a buffered output stream to the file		
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
	protected FileResource createResource(final URI resourceURI) throws IllegalArgumentException, IOException, HTTPConflictException
	{
		final FileResource fileResource=getResource(resourceURI);	//get the resource associated with this URI
		createNewFile(fileResource.getFile());	//create a new file
		return fileResource;	//return the file resource
	}

	/**Creates a collection resource.
	@param resourceURI The URI of the resource to create.
	@return The description of a newly created resource, or <code>null</code> if the resource is not allowed to be created.
	@exception IllegalArgumentException if the given resource URI does not represent a valid resource in a valid burrow.
	@exception IOException Thrown if there is an error creating the resource.
	@exception HTTPConflictException if an intermediate collection required for creating this collection does not exist.
	@see #createResource(URI)
	*/
	protected FileResource createCollection(final URI resourceURI) throws IllegalArgumentException, IOException, HTTPConflictException
	{
		final FileResource fileResource=getResource(resourceURI);	//get the resource associated with this URI
		final File file=fileResource.getFile();	//get the associated file
		if(!file.getParentFile().isDirectory())	//if the file's parent is not an existing directory
		{
			throw new HTTPConflictException();	//indicate the conflict with the parent resource TODO report the URI at some point, which is not the same as the URI of the parent file
		}
		mkdir(file);	//create the directory
		return fileResource;	//return the file resource		
	}

	/**Deletes a resource.
	@param resource The resource to delete.
	@exception IOException Thrown if the resource could not be deleted.
	*/
	protected void deleteResource(final FileResource resource) throws IOException
	{
		
	}

	/**Retrieves an list of child resources of the given resource.
	@param resource The resource for which children should be returned.
	@return A list of child resources.
	@exception IOException Thrown if there is an error retrieving the list of child resources.
	*/
	protected List<FileResource> getChildResources(final FileResource resource) throws IOException
	{
		return resource.getChildResources();	//return the child resources of this file resource
	}

}