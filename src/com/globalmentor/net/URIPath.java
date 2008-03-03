package com.globalmentor.net;

import java.net.URI;

import static com.globalmentor.java.Objects.*;
import static com.globalmentor.net.URIs.*;

/**Represents the path of a hierarchical URI with the same encoding requirements as URIs in general.
<p>Note that this class does not represent what RFC 3986 Section 4.1 calls a <dfn>relative reference</dfn>,
because such references forbid the first path segment of relative references from containing the character
{@value URIs#SCHEME_SEPARATOR} to prevent confusion with a URI containing a scheme.
RFC 3986 Section 4.2 makes this prohibition so that a specification may call for a <dfn>URI reference</dfn>,
which may be either a <dfn>URI</dfn> or a <dfn>relative reference</dfn>.
This class represents a URI path in a context in which it is known to be a URI path and not a URI,
and thus allows the {@value URIs#SCHEME_SEPARATOR} character to appear in the first segment
of a relative path.</p> 
@author Garret Wilson
@see <a href="http://www.ietf.org/rfc/rfc3986.txt">Uniform Resource Identifier (URI): Generic Syntax</a>
*/
public final class URIPath
{

	/**The empty path ("").*/
	public final static URIPath EMPTY=new URIPath("");

	/**The path to root, consisting of a single path separator ("/").*/
	public final static URIPath ROOT=new URIPath(ROOT_PATH);

	/**The root path in the {@value URIs#PATH_SCHEME} scheme; used for creating URI paths using {@link URI#relativize(URI)} for initial relative segments containing a colon.*/
	private final static URI PATH_ROOT_URI=URI.create(PATH_SCHEME+SCHEME_SEPARATOR+PATH_SEPARATOR);	//path:/

	/**The local URI form of the path.*/
	private final URI uri;

	/**Creates a URI path from the given string version of the raw path.
	@param path The raw, encoded path information.
	@exception NullPointerException if the given path is <code>null</code>.
	@exception IllegalArgumentException if the given string violates URI RFC 2396.
	@exception IllegalArgumentException if the provided path specifies a URI authority, query, and/or fragment.
	*/
	public URIPath(final String path)
	{
		this(createURIPathURI(checkInstance(path, "Path cannot be null.")));	//construct the class with a URI created from the path, compensating for relative paths that contain a colon in the first path segment
	}

	/**Creates a URI path from the raw path of the given path URI.
	@param pathURI The URI that represents a path.
	@exception NullPointerException if the given path URI is <code>null</code>.
	@exception IllegalArgumentException if the provided URI specifies a URI scheme (i.e. the URI is absolute), authority, query, and/or fragment.
	@exception IllegalArgumentException if the given URI is not a path.
	*/
	public URIPath(final URI pathURI)
	{
		this.uri=checkPathURI(pathURI);	//check and store the path URI		
	}
	
	/**Determines whether this path is absolute.
	@return <code>true</code> if the path begins with {@value com.globalmentor.net.URIs#ROOT_PATH}.
	*/
	public boolean isAbsolute()
	{
		return uri.getRawPath().startsWith(ROOT_PATH);	//see if the path begins with '/'		
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

	/**Determines whether the path is a canonical collection path.
	@return <code>true</code> if the path ends with a slash ('/').
	*/
	public boolean isCollection()
	{
		return isCollectionPath(uri.getRawPath());	//see if the raw path is a collection path		
	}

	/**Checks to see if the path represents a collection.
	If the path does not represent a collection, an exception is thrown.
	@return This path.
	@exception IllegalArgumentException if the path does not end with a slash ('/').
	@see #isCollection()
	*/
	public URIPath checkCollection() throws IllegalArgumentException
	{
		if(!isCollection())	//if this path is not a collection
		{
			throw new IllegalArgumentException("The path "+this+" is not a collection.");
		}
		return this;	//return this path
	}

	/**Determines if this is an empty path.
	@return <code>true</code> if this path is the empty path.
	*/
	public boolean isEmpty()
	{
		return uri.getRawPath().isEmpty();	//return whether the path is empty
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

	/**Normalizes the path by removing all <code>.</code> and <code>..</code> segments.
	@return A URI path equivalent to this URI path, but in normal form.
	*/
	public URIPath normalize()
	{
		final URI normalizedURI=uri.normalize();	//normalize the URI
		return normalizedURI==uri ? this : new URIPath(normalizedURI);	//if the URI was already normalized and we got back the same URI, we're already normalized
	}


	/**Determines the current level of this path.
	This is equivalent to resolving the path {@value URIs#CURRENT_LEVEL_PATH_SEGMENT} to this path.
	@return A path representing the current hierarchical level this path.	
	*/
	public URIPath getCurrentLevel()
	{
		final URI currentLevelURI=URIs.getCurrentLevel(uri);	//get the current level as a URI
		return currentLevelURI.equals(uri) ? this : new URIPath(currentLevelURI);	//only create a new URI path if it's a different path than this one
	}

	/**Determines the parent level of this path.
	This is equivalent to resolving the path {@value URIs#PARENT_LEVEL_PATH_SEGMENT} to this path.
	@return A path representing the parent hierarchical level of a hierarchical URI; equivalent to resolving the path ".." to the URI.	
	*/
	public URIPath getParentLevel()
	{
		final URI parentLevelURI=URIs.getParentLevel(uri);	//get the parent level as a URI
		return parentLevelURI.equals(uri) ? this : new URIPath(parentLevelURI);	//only create a new URI path if it's a different path than this one
	}

	/**Relativizes the given path against this path.
	This is a convenience method that functions by creating a new {@link URIPath} from the given string and delegating to {@link #relativize(URIPath)}.
	@param path The path to be relativized against this path.
	@return The resulting path.
	@exception NullPointerException if the given path is <code>null</code>.
	*/
	public URIPath relativize(final String path)
	{
		return relativize(new URIPath(checkInstance(path, "Path cannot be null.")));	//convert the String to a URI path and relativize it against this path
	}

	/**Relativizes the given path against this path.
	@param path The path to be relativized against this path.
	@return The resulting path.
	@exception NullPointerException if the given path is <code>null</code>.
	*/
	public URIPath relativize(final URIPath path)
	{
		return new URIPath(uri.relativize(checkInstance(path, "Path cannot be null.").toURI()).getRawPath());	//relativize the URI form of the given path against the URI form of this path and create a new path from the resulting URI's raw path
	}

	/**Resolves the given path against this path.
	This is a convenience method that functions by creating a new {@link URIPath} from the given string and delegating to {@link #resolve(URIPath)}.
	@param path The path to be resolved against this path.
	@return The resulting path.
	@exception NullPointerException if the given path is <code>null</code>.
	@exception IllegalArgumentException if the given string violates URI RFC 2396.
	@exception IllegalArgumentException if the provided path specifies a URI scheme (i.e. the path represents an absolute URI) and/or authority.
	*/
	public final URIPath resolve(final String path)
	{
		return resolve(new URIPath(checkInstance(path, "Path cannot be null.")));	//convert the String to a URI path and resolve it against this path
  }

	/**Resolves the given path against this path.
	@param path The path to be resolved against this path.
	@return The resulting path.
	@exception NullPointerException if the given path is <code>null</code>.
	*/
	public URIPath resolve(final URIPath path)
	{
		return new URIPath(uri.resolve(checkInstance(path, "Path cannot be null.").toURI()).getRawPath());	//resolve the URI form of the given path against the URI form of this path and create a new path from the resulting URI's raw path
  }

	/**Resolves the given URI against this path.
	@param uri The uri to be resolved against this path.
	@return The resulting URI.
	@exception NullPointerException if the given URI is <code>null</code>.
	*/
	public URI resolve(final URI uri)
	{
		return this.uri.resolve(checkInstance(uri, "URI cannot be null."));	//resolve the URI form of the given path against given URI
  }

	/**@return A path-only URI containing this URI path.*/
	public URI toURI() {return uri;}

	/**@return A URI containing this URI path in the {@value URIs#PATH_SCHEME} scheme.*/
	public URI toPathURI() {return createURI(PATH_SCHEME, uri.getRawPath());}

	/**@return A string representation of the raw, encoded path as it would appear in a URI.*/
	public String toString()
	{
		return uri.getRawPath();	//return the raw path from the local URI
	}

	/**Returns the hash code of this object.
	@return The hash code of this object.
	*/
	public int hashCode()
	{
		return uri.hashCode();	//return the URI's hash code
	}
	
	/**Determines if this object equals another object.
	@param object The object to compare with this object.
	@return <code>true</code> if the given object is considered equal to this object.
	*/
	public boolean equals(final Object object)
	{
		return object instanceof URIPath && uri.equals(((URIPath)object).uri);	//see if the object is a URI path with the same URI
	}

	/**Creates a URI consisting of the given string version of the raw path.
	This method performs special processing for relative paths that begin with a segment containing {@value URIs#SCHEME_SEPARATOR}.
	@param path The raw, encoded path information.
	@exception NullPointerException if the given path is <code>null</code>.
	@exception IllegalArgumentException if the given string violates URI RFC 2396.
	*/
	public static URI createURIPathURI(final String path)
	{
			//see if this is a relative path that contains a colon in the first segment
		final int length=path.length();	//get the length of the path
		for(int i=0; i<length; ++i)	//for each character
		{
			final char c=path.charAt(i);	//get this character
			if(c==PATH_SEPARATOR)	//if we find a path separator, there was no colon in the first segment of a relative path
			{
				break;	//process the path normally
			}
			else if(c==SCHEME_SEPARATOR)	//if we find a colon in the first segment of a relative path
			{
				return PATH_ROOT_URI.relativize(URI.create(PATH_SCHEME+SCHEME_SEPARATOR+PATH_SEPARATOR+path));	//create a URI in the form <path:/path> and relativize it to <path:/>, which will create a URI with the correct path
			}
		}
		return URI.create(path);	//create a URI version of the path normally
	}

	/**Encodes the given string so that it is a valid URI path according <a href="http://www.ietf.org/rfc/rfc3986.txt">RFC 3986</a>, "Uniform Resource Identifiers (URI): Generic Syntax".
	@param path The path to URI-encode.
	@return A string containing the escaped data.
	*/
	public static String encode(final String path)
	{
		return uriEncode(path, PATH_CHARACTERS);	//encode all non-path characters
		//TODO important: encode "." and ".."
	}

	/**Encodes the given string so that it is a valid URI path segment according <a href="http://www.ietf.org/rfc/rfc3986.txt">RFC 3986</a>, "Uniform Resource Identifiers (URI): Generic Syntax".
	@param pathSegment The path segment to URI-encode.
	@return A string containing the escaped data.
	*/
	public static String encodeSegment(final String pathSegment)
	{
		return uriEncode(pathSegment, PATH_SEGMENT_CHARACTERS);	//encode all non-path segment characters
		//TODO important: encode "." and ".."
	}
}
