package com.garretwilson.io;

import java.net.URI;

/**Indicates an object that allow access to resources by providing input
	streams and indicating a base URI from which relative URIs should be
	resolved.
@author Garret Wilson
*/
public interface URIAccessible extends URIInputStreamable
{
	/**@return The base URI from which relative URIs should be resolved, or
		<code>null</code> if there is no base URI.
	*/
	public URI getBaseURI();

}
