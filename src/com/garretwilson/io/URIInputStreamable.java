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
	<p>The implementation may decide to simply connect to the URI and return an input
		stream from the connection, if the URI is a URL.
		The URL may be to a file inside a .zip file, and the implementation may
		open an input stream directly from the .zip file. Whatever the case, whatever
		class implements this input stream takes on the responsibility of returning
		an input stream from the given URL.</p>
	<p>The calling class has the responsibility for closing the input stream.</p>
	@param uri A URI to a resource.
	@return An input stream to the contents of the resource represented by the given URI.
	@exception IOException Thrown if an I/O error occurred.
	*/
	public InputStream getInputStream(final URI uri) throws IOException;

}
