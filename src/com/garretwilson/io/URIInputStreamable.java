package com.garretwilson.io;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;

/**Class that knows how to get an input stream for the given URI.
@author Garret Wilson
@see InputStream
*/
public interface URIInputStreamable
{

	/**Returns an input stream for the given URI.
	The calling class has the responsibility for closing the input stream.
	@param uri A URI to a resource.
	@return An input stream to the contents of the resource represented by the given URI.
	@exception IOException Thrown if an I/O error occurred.
	*/
	public InputStream getInputStream(final URI uri) throws IOException;

}
