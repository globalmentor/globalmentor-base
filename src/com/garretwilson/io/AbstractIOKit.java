package com.garretwilson.io;

import java.io.*;
import java.net.URI;

/**Abstract implementation of reading and writing functionality for an object.
@author Garret Wilson
*/
public abstract class AbstractIOKit<T> extends DefaultURIAccessible implements IOKit<T>
{

	/**Default constructor.*/
	public AbstractIOKit()
	{
		this(null, null);
	}

	/**URI input stream locator constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	*/
	public AbstractIOKit(final URIInputStreamable uriInputStreamable)
	{
		this(uriInputStreamable, null);
	}

	/**URI output stream locator constructor.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public AbstractIOKit(final URIOutputStreamable uriOutputStreamable)
	{
		this(null, uriOutputStreamable);
	}

	/**Full constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public AbstractIOKit(final URIInputStreamable uriInputStreamable, final URIOutputStreamable uriOutputStreamable)
	{
		super(uriInputStreamable, uriOutputStreamable);	//construct the parent class
	}

	/**Loads an object from given URI.
	<p>This version opens a stream and delegates to <code>load(InputStream, URI)</code>.</p>
	@param uri The URI that identifies the resource to be loaded.
	@return An object containing the data of the resouce represented by the URI.
	@exception IOException Thrown if there is an error reading the object.
	*/
	public T load(final URI uri) throws IOException
	{
			//get an input stream to the resource
		final InputStream inputStream=getInputStream(uri);
		try
		{
			return load(inputStream, uri);	//load the object from the input stream
		}
		finally
		{
			inputStream.close();	//always close the input stream
		}
	}

	/**Saves an object to a given URI.
	<p>This version opens a stream and delegates to <code>save(T, OutputStream)</code>.</p>
	@param object The object the data of which will be saved at the given URI.
	@param uri The URI at which the object should be saved.
	@exception IOException Thrown if there is an error writing the object.
	*/
	public void save(final T object, final URI uri) throws IOException
	{
		final OutputStream outputStream=getOutputStream(uri);	//get an output stream to this URI
		try
		{
			save(object, outputStream);	//write to the output stream
		}
		finally
		{
			outputStream.close();	//always close the output stream
		}
	}

}
