package com.garretwilson.io;

import java.io.*;
import java.net.URI;
import com.garretwilson.io.URIAccessible;

/**Provides reading and writing functionality for an object.
@author Garret Wilson
*/
public interface IOKit<T> extends URIAccessible
{

	/**Loads an object from a given URI.
	@param uri The URI that identifies the object to be loaded.
	@return The object containing the data of the resouce represented by the URI.
	@exception IOException Thrown if there is an error reading the object.
	*/
	public T load(final URI uri) throws IOException;

	/**Loads an object from an input stream.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the content, or <code>null</code> if no base
		URI is available.
	@throws IOException Thrown if there is an error reading the data.
	*/ 
	public T load(final InputStream inputStream, final URI baseURI) throws IOException;

	/**Saves an object to a given URI.
	@param object The object the data of which will be saved at the given URI.
	@param uri The URI at which the object should be saved.
	@exception IOException Thrown if there is an error writing the object.
	*/
	public void save(final T object, final URI uri) throws IOException;

	/**Saves an object to an output stream.
	@param object The object the data of which will be written to the given output stream.
	@param outputStream The output stream to which to write the object content.
	@throws IOException Thrown if there is an error writing the object.
	*/
	public void save(final T object, final OutputStream outputStream) throws IOException;

	/**Checks to see if the object exists.
	@param uri The URI that identifies the model.
	@return <code>true</code> if the model exists,
		else <code>false</code> if the model does not exist.
	@exception IOException Thrown if there is a problem determining if the model exists.
	*/
//TODO fix, maybe	public boolean exists(final URI uri) throws IOException;

}
