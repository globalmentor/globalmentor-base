package com.garretwilson.io;

import java.net.URI;
import com.garretwilson.lang.JavaConstants;

/**Indicates an object that allow access to resources by providing input and
	output streams and indicating a base URI from which relative URIs should be
	resolved.
@author Garret Wilson
*/
public interface URIAccessible extends URIInputStreamable, URIOutputStreamable
{

	/**The base URI property, if it is bound to an object.*/
	public final String BASE_URI_PROPERTY=URIAccessible.class.getName()+JavaConstants.PACKAGE_SEPARATOR+"baseURI";

	/**The URI input streamable property, if it is bound to an object..*/
	public final String URI_INPUT_STREAMABLE_PROPERTY=URIAccessible.class.getName()+JavaConstants.PACKAGE_SEPARATOR+"uriInputStreamable";

	/**The URI output streamable property, if it is bound to an object..*/
	public final String URI_OUTPUT_STREAMABLE_PROPERTY=URIAccessible.class.getName()+JavaConstants.PACKAGE_SEPARATOR+"uriOutputStreamable";

	/**@return The base URI from which relative URIs should be resolved, or
		<code>null</code> if there is no base URI.
	*/
	public URI getBaseURI();

}
