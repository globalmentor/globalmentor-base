package com.garretwilson.io;

import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;

/**Class that knows how to get an output stream for the given URI.
@author Garret Wilson
@see OutputStream
*/
public interface URIOutputStreamable
{

	/**Returns an output stream for the given URI.
	The implementing class has the responsibility for closing the output stream.
	@param uri A URI to a resource.
	@return An output stream to the contents of the resource represented by the given URI.
	@exception IOException Thrown if an I/O error occurred.
	*/
	public OutputStream getOutputStream(final URI uri) throws IOException;

}
