package com.garretwilson.net;

import java.net.URI;

import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.net.URIConstants.*;
import static com.garretwilson.net.URIUtilities.*;

/**Represents the path of a hierarchical URI with the same encoding requirements as URIs in general.
@author Garret Wilson
*/
public class URIPath
{

	/**The local URI form of the path.*/
	private final URI uri;

	/**Creates a URI path from the given string version of the raw path.
	@param path The raw, encoded path information.
	@exception NullPointerException if the given path is <code>null</code>.
	@exception IllegalArgumentException if the given string violates URI RFC 2396.
	@exception IllegalArgumentException if the provided path specifies a URI scheme (i.e. the path represents an absolute URI) and/or authority.
	*/
	public URIPath(final String path)
	{
		this.uri=checkPathURI(URI.create(checkInstance(path, "Path cannot be null.")));
	}

	/**Determines whether this path is absolute.
	@return <code>true</code> if the path begins with {@value com.garretwilson.net.URIConstants#ROOT_PATH}.
	*/
	public boolean isAbsolute()
	{
		return toString().startsWith(ROOT_PATH);	//see if the path begins with '/'		
	}

	/**Checks to see if the path is absolute.
	If the path is not absolute, an exception is thrown.
	@return This path.
	@exception IllegalArgumentException if the path is not absolute.
	@see #isAbsolute()
	*/
	public URIPath checkAbsolute() throws IllegalArgumentException
	{
		if(!isAbsolute())	//if this path is not absolute
		{
			throw new IllegalArgumentException("The path "+this+" is not absolute.");
		}
		return this;	//return this path
	}

	/**Checks to see if the path is relative.
	If the path is not relative, an exception is thrown.
	@return This path.
	@exception IllegalArgumentException if the path is absolute.
	@see #isAbsolute()
	*/
	public URIPath checkRelative() throws IllegalArgumentException
	{
		if(isAbsolute())	//if this path is absolute
		{
			throw new IllegalArgumentException("The path "+this+" is not relative.");
		}
		return this;	//return this path
	}

	/**@return A path-only URI containing this URI path.*/
	public URI toURI() {return uri;}

	/**@return A string representation of the raw, encoded path as it would appear in a URI.*/
	public String toString()
	{
		return uri.getRawPath();	//return the raw path from the local URI
	}

	/**Encodes the given string so that it is a valid URI path according <a href="http://www.ietf.org/rfc/rfc3986.txt">RFC 3986</a>, "Uniform Resource Identifiers (URI): Generic Syntax".
	@param path The path to URI-encode.
	@return A string containing the escaped data.
	*/
	public static String encode(final String path)
	{
		return uriEncode(path, PATH_CHARACTERS);	//encode all non-path characters
	}

	/**Encodes the given string so that it is a valid URI path segment according <a href="http://www.ietf.org/rfc/rfc3986.txt">RFC 3986</a>, "Uniform Resource Identifiers (URI): Generic Syntax".
	@param pathSegment The path segment to URI-encode.
	@return A string containing the escaped data.
	*/
	public static String encodeSegment(final String pathSegment)
	{
		return uriEncode(pathSegment, PATH_SEGMENT_CHARACTERS);	//encode all non-path segment characters
	}
}
