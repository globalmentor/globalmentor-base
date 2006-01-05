package com.garretwilson.net;

import java.io.*;
import java.net.*;
import java.util.*;
import static java.util.Collections.*;

import javax.mail.internet.ContentType;
import com.garretwilson.io.*;
import com.garretwilson.text.FormatUtilities;
import com.garretwilson.util.*;
import com.sun.mail.iap.Argument;

import static com.garretwilson.lang.CharSequenceUtilities.*;
import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.net.URIConstants.*;

/**Various URI manipulating functions for working with URIs as defined in
	<a href="http://www.ietf.org/rfc/rfc2396.txt">RFC 2396</a>,
	"Uniform Resource Identifiers (URI): Generic Syntax".
@see java.net.URI
*/
public class URIUtilities
{

	/**Creates a string of type <code>text/uri-list</code> as defined in <a href="http://www.ietf.org/rfc/rfc2483.txt">RFC 2483</a>, "URI Resolution Services Necessary for URN Resolution".
	@param uris The URIs to include in the list.
	@return A URI list string.
	*/ 
	public static String createURIList(final URI... uris)
	{
		return FormatUtilities.formatList(new StringBuilder(), "\r\n", (Object[])uris).toString();	//create the URI list
	}

	/**Creates a new URI identical to the supplied URI with a different path.
	@param uri The URI to change.
	@param path The unescaped path.
	@return A new URI with the new path information.
	*/
	public static URI changePath(final URI uri, final String path)	//TODO check all references to this method to make sure escaped/unescaped semantics are followed
	{
			//construct an identical URI except for the supplied path
		return create(uri.getScheme(), uri.getUserInfo(), uri.getHost(), uri.getPort(), path, uri.getQuery(), uri.getFragment());
	}

	/**Creates a new URI identical to the supplied URI with no query or fragment.
	@param uri The URI from which to remove the query and fragment, if any.
	@return A new URI with no query or fragment.
	*/
	public static URI getPlainURI(final URI uri)	//TODO change all references to raw access, and change create() to use raw inputs
	{
		return create(uri.getScheme(), uri.getUserInfo(), uri.getHost(), uri.getPort(), uri.getPath(), null, null);	//construct an identical URI except with no query or fragment
	}

	/**Constructs an absolute path from the given elements in the form:
	<code>/<var>element1</var>/<var>element2</var></code>.
	Each element in the path is URI-encoded using UTF-8.
	@param absolute <code>true</code> if the path should be absolute
		and therefore should begin with '/'.
	@param pathElements <code>true</code> if the path represents a collection
		and therefore should end with '/'. 
	@return A path constructed according to the given rules.
	@exception IllegalArgumentException if there are no path elements and an
		absolute non-collection or non-absolute collection is requested.
	*/
	public static String constructPath(final boolean absolute, final boolean collection, final String... pathElements)
	{
		if(pathElements.length==0 && absolute!=collection)	//if there are no path elements, an absolute URI must also be a collection
		{
			throw new IllegalArgumentException("A path with no elements must be an absolute collection or a relative non-collection.");
		}
		final StringBuilder stringBuilder=new StringBuilder();	//create a string builder
		if(absolute)	//if this should be an absolute path
		{
			stringBuilder.append(PATH_SEPARATOR);	//prepend '/'
		}
		boolean hasPath=false;	//don't assume we have any path elements
		for(final String pathElement:pathElements)	//look at each path element
		{
//G***fix			try
			{
//TODO fix encoding using a real encoder, not the www-encoding URLEncoder				stringBuilder.append(encode(pathElement, UTF_8));	//encode and append this path element
				stringBuilder.append(pathElement);	//encode and append this path element
				stringBuilder.append(PATH_SEPARATOR);	//separate the path elements
			}
/*G***fix
			catch(final UnsupportedEncodingException unsupportedEncodingException)	//we should always support UTF-8
			{
				throw new AssertionError(unsupportedEncodingException);
			}
*/
		}
		if(!collection && pathElements.length>0)	//if there were path elements but this wasn't a collection, we have one too many path separators 
		{
			stringBuilder.deleteCharAt(stringBuilder.length()-1);	//remove the last character, a '/'
		}
		return stringBuilder.toString();	//return the string version of the constructed path
	}
	
	/**Constructs a query string for a URI by URI-encoding each name-value pair,
	 	separating them with '&', and prepending the entire string (if there is
	 	at least one parameter) with '?'.
	@param params The name-value pairs representing the query parameters.
	@return A string representing the constructed query, or the empty string if
		there were no parameters.
	*/
	public static String constructQuery(final NameValuePair<String, String>... params)
	{
		return constructQuery(constructQueryParameters(params));	//construct a query, prepended with the query character
	}

	/**Constructs a query string for a URI by prepending the given query string, if it is not the empty string, with '?'.
	@param params The string representing the query parameters.
	@return A string representing the constructed query, or the empty string if there were no parameters.
	*/
	public static String constructQuery(final String params)
	{
		final StringBuilder query=new StringBuilder();
		if(params.length()>0)	//if there is at least one parameter character
		{
			query.append(QUERY_SEPARATOR);	//append the query prefix
			query.append(params);	//append the params
		}
		return query.toString();	//return the query string we constructed
	}

	/**Constructs a query string for a URI by URI-encoding each name-value pair, separating them with '&'.
	@param params The name-value pairs representing the query parameters.
	@return A string representing the constructed query, or the empty string if there were no parameters.
	*/
	public static String constructQueryParameters(final NameValuePair<String, String>... params)
	{
		final StringBuilder paramStringBuilder=new StringBuilder();
		if(params.length>0)	//if there is at least one parameter
		{
			for(NameValuePair<String, String> param : params)	//look at each parameter
			{
				paramStringBuilder.append(encode(param.getName()));	//append the parameter name
				paramStringBuilder.append(QUERY_NAME_VALUE_ASSIGNMENT);	//append the value-assignment character
				paramStringBuilder.append(encode(param.getValue()));	//append the parameter value
				paramStringBuilder.append(QUERY_NAME_VALUE_PAIR_DELIMITER);	//append the name-value pair delimiter
			}
			paramStringBuilder.delete(paramStringBuilder.length()-1, paramStringBuilder.length());	//remove the last name-value pair delimiter
		}
		return paramStringBuilder.toString();	//return the query parameter string we constructed
	}

	/**Creates a path-based query from a standard URI query.
	A query in the form <code>?var1=value1&amp;var2=value2</code> will be converted to the form
		<code>/var1%3Dvalue1/var2&3Dvalue2</code>.
	@param query The standard URI query string, optionally beginning with '?'.
	@return A query string converted to a path. A query string beginning with '?' will be converted into
		an absolute path.
	*/
/*TODO fix; do *not* use URLEncoder, which is for www-encoded forms	
	public static String createPathQuery(final String query)
	{
		if(query.length()>0)	//if the query has at least one character
		{
			final StringBuilder stringBuilder=new StringBuilder();	//create a string builder for creating a path query
			final int startIndex;	//find out if we should skip the first character			
			if(query.charAt(0)==QUERY_SEPARATOR)	//if the first character is '?'
			{
				stringBuilder.append(PATH_SEPARATOR);	//convert it to a '/'
				startIndex=1;	//skip the introductory character
			}
			else	//if the string doesn't begin with '?'
			{
				startIndex=0;	//we'll just start at the first
			}
				//tokenize the string on the attribute delimiter, '&'
			final StringTokenizer stringTokenizer=new StringTokenizer(query.substring(startIndex), String.valueOf(QUERY_NAME_VALUE_PAIR_DELIMITER));
			while(stringTokenizer.hasMoreTokens())	//while there are more tokens
			{
				final String token=stringTokenizer.nextToken();	//get the next token
				try
				{
					stringBuilder.append(URLEncoder.encode(token, UTF_8));	//encode and append the next token
				}
				catch(UnsupportedEncodingException unsupportedEncodingException)	//we should always support UTF-8
				{
					throw new AssertionError(unsupportedEncodingException);
				}					
				if(stringTokenizer.hasMoreTokens())	//if there are more tokens
				{
					stringBuilder.append(PATH_SEPARATOR);	//add a path separator, '/'					
				}
			}
			return stringBuilder.toString();	//return the string we constructed
		}
		else	//if the query is empty
		{
			return query;	//return the query as it is
		}		
	}
*/

	/**Creates a standard URI based query from a path-based query.
	A query in the form <code>/var1%3Dvalue1/var2&3Dvalue2</code> will be converted to the form
		<code>?var1=value1&amp;var2=value2</code>.
	@param pathQuery The standard URI query string, optionally beginning with '?'.
	@return A query string converted to a path. A query string beginning with '?' will be converted into
		an absolute path.
	*/
/*TODO fix; do *not* use URLEncoder, which is for www-encoded forms	
	public static String createQuery(final String pathQuery)
	{
		if(pathQuery.length()>0)	//if the query has at least one character
		{
			final StringBuilder stringBuilder=new StringBuilder();	//create a string builder for creating a query
			final int startIndex;	//find out if we should skip the first character			
			if(pathQuery.charAt(0)==PATH_SEPARATOR)	//if the first character is '/'
			{
				stringBuilder.append(QUERY_SEPARATOR);	//convert it to a '?'
				startIndex=1;	//skip the introductory character
			}
			else	//if the string doesn't begin with '/'
			{
				startIndex=0;	//we'll just start at the first
			}
				//tokenize the string on path separators, '/'
			final StringTokenizer stringTokenizer=new StringTokenizer(pathQuery.substring(startIndex), String.valueOf(PATH_SEPARATOR));
			while(stringTokenizer.hasMoreTokens())	//while there are more tokens
			{
				final String token=stringTokenizer.nextToken();	//get the next token
				try
				{
					stringBuilder.append(URLDecoder.decode(token, UTF_8));	//encode and append the next token
				}
				catch(UnsupportedEncodingException unsupportedEncodingException)	//we should always support UTF-8
				{
					throw new AssertionError(unsupportedEncodingException);
				}					
				if(stringTokenizer.hasMoreTokens())	//if there are more tokens
				{
					stringBuilder.append(QUERY_NAME_VALUE_PAIR_DELIMITER);	//add a query name/value pair separator, '&'					
				}
			}
			return stringBuilder.toString();	//return the string we constructed
		}
		else	//if the path query is empty
		{
			return pathQuery;	//return the path query as it is
		}		
	}
*/

	/**Retrieves name-value parameters from a standard URI query string.
	@param query The URI query string, optionally beginning with a '?' character.
	@return An array of name-value pairs representing query parameters.
	*/
/*TODO fix; do *not* use URLEncoder, which is for www-encoded forms	
	public static NameValuePair<String, String>[] getQueryParameters(final String query)
	{
		final List<NameValuePair<String, String>> parameterList=new ArrayList<NameValuePair<String, String>>();	//create a list to hold our parameters
		if(query.length()>0)	//if the query has at least one character
		{
			final int startIndex;	//find out if we should skip the first character			
			if(query.charAt(0)==QUERY_SEPARATOR)	//if the first character is '?'
			{
				startIndex=1;	//skip the introductory character
			}
			else	//if the string doesn't begin with '?'
			{
				startIndex=0;	//we'll just start at the first
			}
				//tokenize the string on the attribute delimiter, '&'
			final StringTokenizer stringTokenizer=new StringTokenizer(query.substring(startIndex), String.valueOf(QUERY_NAME_VALUE_PAIR_DELIMITER));
			while(stringTokenizer.hasMoreTokens())	//while there are more tokens
			{
				final String token=stringTokenizer.nextToken();	//get the next token
				final int equalsIndex=token.indexOf(QUERY_NAME_VALUE_ASSIGNMENT);	//get the index of the '=' character
				final String name;	//we'll determine the name and the value
				final String value;
				if(equalsIndex>=0)	//if there is an equals character
				{
					name=token.substring(0, equalsIndex);	//the name is everything up to but not including the '='
					value=token.substring(equalsIndex+1);	//the value is everything after the '='
				}
				else	//if there is no equals character
				{
					name=token;	//take the token as it is for the name
					value=null;	//there is no value G***is this the correct thing to do? should it be ""?
				}
				parameterList.add(new NameValuePair<String, String>(name, value));	//add this parameter to the list 
			}
		}
		return parameterList.toArray(new NameValuePair[parameterList.size()]);	//return the list as an array
	}
*/

	/**Retrieves the parameters from the query of a URI, if present.
	@param uri The URI from which to extract parameters.
	@return An array of parameters.
	*/
	public static ListMap<String, String> getParameterMap(final URI uri)
	{
		final NameValuePair<String, String>[] parameters=getParameters(uri);	//get the parameters from the URI
		final ListMap<String, String> parameterListMap=new ArrayListHashMap<String, String>();	//create a new list map in which to store the parameters
		if(parameters!=null)	//if this URI specified a query
		{
			for(final NameValuePair<String, String> parameter:parameters)	//for each parameter
			{
				parameterListMap.addItem(parameter.getName(), parameter.getValue());	//add this name and value, each of which may have been encoded
			}
		}
		return parameterListMap;	//return the parameters, if any
	}

	/**Retrieves the query parameters from a URI.
	@param uri The URI which may contain a query.
	@return An array of parameters represented by the URI query, or <code>null</code> if the given URI does not contain a query.
	*/
	public static NameValuePair<String, String>[] getParameters(final URI uri)
	{
		return getParameters(uri.getRawQuery());	//return the paramters for this URI query, if there is a query
	}		

	/**Retrieves the parameters from a URI query.
	@param query The string containing URI query parameters (without the '?' prefix), or <code>null</code>.
	@return An array of parameters represented by the query, or <code>null</code> if the given query is <code>null</code>.
	*/
	@SuppressWarnings("unchecked")	//we can't check the creation of a generic array
	public static NameValuePair<String, String>[] getParameters(final String query)
	{
		if(query!=null)	//if a query was given
		{
			final String[] parameterStrings=query.split(String.valueOf(QUERY_NAME_VALUE_PAIR_DELIMITER));	//split the query into parameters
			final NameValuePair<String, String>[] parameters=new NameValuePair[parameterStrings.length];	//create an array to hold parameters
			int i=0;
			for(final String parameterString:parameterStrings)	//for each parameters
			{
				final String[] nameValue=parameterString.split(String.valueOf(QUERY_NAME_VALUE_ASSIGNMENT));	//split the parameter into its name and value
				final String name;	//we'll get the parameter name
				final String value;	//we'll get the parameter value
				if(nameValue.length>0)	//if there was at least one token
				{
					name=decode(nameValue[0]);	//the first token is the name
					value=nameValue.length>1 ? decode(nameValue[1]) : "";	//use thet empty string for the value if no value was provided
				}
				else	//if there wasn't at least one token
				{
					name=value="";	//there is no name or value
				}
				parameters[i++]=new NameValuePair<String, String>(name, value);	//create a new parameter and advance to the next index
			}
			return parameters;	//return the parameters
		}
		else	//if no query is given
		{
			return null;	//there are no parameters
		}
	}

	/**Creates a URI from the given path, verifying that the string contains only a path.
	@param path The string version of a path to convert to a URI form of that same path.
	@exception NullPointerException if the given path is <code>null</code>.
	@exception IllegalArgumentException if the provided path specifies a URI scheme (i.e. the URI is absolute) and/or authority.
	@see #isPathURI(URI)
	*/
	public static URI createPathURI(final String path)
	{
		final URI pathURI=URI.create(checkNull(path, "Path cannot be null"));	//create a URI from the given path
		if(!isPathURI(pathURI))	//if there is a scheme or an authority
		{
			throw new IllegalArgumentException("Path cannot have a URI scheme or authority, and must include a path: "+path);
		}
		return pathURI;	//return the URI we created
	}

	/**Determines if a given path is only a path and not a URI with a scheme and/or authority.
	@param path The string version of a path to determine if it.
	@return <code>true</code> if the path is a path and does not specifiy a scheme (i.e. the URI is not absolute) or authority.
	@exception NullPointerException if the given path is <code>null</code>.
	@see #isPathURI(URI)
	*/
	public static boolean isPath(final String path)
	{
		final URI pathURI=URI.create(checkNull(path, "Path cannot be null"));	//create a URI from the given path
		return isPathURI(pathURI);	//indicate whether the constructed URI represents a path
	}

	/**Determines if a given URI contains only a path and does not have a scheme and/or authority.
	@param uri The URI to check to for path status.
	@exception NullPointerException if the given URI is <code>null</code>.
	@return <code>true</code> if the URI has a path and does not specifiy a scheme (i.e. the URI is not absolute) or authority.
	*/
	public static boolean isPathURI(final URI uri)
	{
		checkNull(uri, "URI cannot be null");
		return uri.getScheme()==null && uri.getRawAuthority()==null && uri.getPath()!=null;	//see if there is no scheme, no authority, and a path
	}

	/**Determines the current level of a hierarchical URI.
	@param uri The URI to examine.
	@return A URI representing the current hierarchical level of a hierarchical
		URI; equivalent to resolving the path "." to the URI.	
	*/
	public static URI getCurrentLevel(final URI uri)
	{
		return uri.resolve(CURRENT_LEVEL_PATH_SEGMENT);	//resolve the URI to "."
	}

	/**Determines the parent level of a hierarchical URI.
	@param uri The URI to examine.
	@return A URI representing the parent hierarchical level of a hierarchical
		URI; equivalent to resolving the path ".." to the URI.	
	*/
	public static URI getParentLevel(final URI uri)
	{
		return uri.resolve(PARENT_LEVEL_PATH_SEGMENT);	//resolve the URI to ".."
	}

	/**Determines the parent collection of a hierarchical URI.
	@param uri The URI to examine.
	@return A URI representing the parent collection of a hierarchical
		URI; if the URI ends in '/', equivalent to resolving the path ".." to the URI;
		if the URI does not end in '/', equivalent to resolving the path "." to the URI.	
	*/
	public static URI getParentURI(final URI uri)
	{
		return endsWith(uri.toString(), PATH_SEPARATOR) ? getParentLevel(uri) : getCurrentLevel(uri);	//if the path ends with a slash, get the parent level; otherwise, get the current level
	}

	/**Determines the canonical root URI a URI.
	@param uri The URI to examine.
	@return A URI representing the URI with no path and no query or fragment.
	*/
	public static URI getRootURI(final URI uri)
	{
		return create(uri.getScheme(), uri.getUserInfo(), uri.getHost(), uri.getPort(), null, null, null);
	}

	/**Retrieves a <code>File</code> representing the file of the URI.
	@param uri The URI for which to return a file.
	@return A <code>File</code> object representing the file path of the URI with no query string, or <code>null</code> if there is no path.
	*/
	public static File getFile(final URI uri)
	{
		final String path=uri.getPath();	//get the path of the URI
		return path!=null ? new File(uri.getPath()) : null; //create a new File from the URI path, if there is one
	}

	/**Retrieves the file name of the URI.
	@param uri The URI for which to return a file name.
	@return The name of the file in the URI.
	@see #getFile
	*/
	public static String getFileName(final URI uri)
	{
		return getFile(uri).getName();  //return the name of the file we construct from the URI
	}

	/**Returns the media type for the specified URI based on its file extension.
	@param uri The URI for which to return a media type.
	@return The default media type for the URI's file extension, or <code>null</code>
		if no known media type is associated with this URI's extension.
	@see ContentTypeUtilities#getMediaType
	*/
	public static ContentType getMediaType(final URI uri)
	{
		final File file=getFile(uri);	//get the file from the URI
		return file!=null ? ContentTypeUtilities.getMediaType(FileUtilities.getExtension(file)) : null; //return the media type based on the extension of the URI filename, if there is one
	}

	/**Normalizes the given path by resolving the '.' and '..' path segments.
	@param path The path to normalize.
	@return The normalized form of the given path.
	@exception NullPointerException if the given path is <code>null</code>.
	@exception IllegalArgumentException if the provided path specifies a URI scheme (i.e. the URI is absolute) and/or authority.
	@see URI#normalize()
	*/
	public static String normalizePath(final String path)
	{
		return createPathURI(path).normalize().getPath();	//get a URI from the path, normalize that URI, and then return the path of the resulting URI
	}

	/**Relativizes the given full path against the given base path.
	@param basePath The path against which the full path should be relativized.
	@param fullPath The full path to be relativized.
	@return A form of the full path relative to the base path.
	@exception NullPointerException if one of the given paths is <code>null</code>.
	@exception IllegalArgumentException if one of the provided path specifies a URI scheme (i.e. the URI is absolute) and/or authority.
	*/
	public static String relativizePath(final String basePath, final String fullPath)
	{
		final URI baseURI=createPathURI(basePath);	//create a URI for the base path, ensuring it's a path
		final URI fullURI=createPathURI(fullPath);	//create a URI for the full path, ensuring it's a path
		return baseURI.relativize(fullURI).getPath();	//relativize the URIs and return the path
	}

	/**Creates a URI from a URL.
	JDK 1.5 provides an equivalent {@link URL#toURI()}.
	This method is provided for backwards-compatibility using for example Retroweaver. 
	@param url The URL to convert to a URI. The URL should already be properly encoded.
	@return The URI form of the URL.
	@exception URISyntaxException Thrown if the URL could not be converted to a URI.
	*/ 
	public static URI createURI(final URL url) throws URISyntaxException
	{
		return new URI(url.toString());	//assuming the URL is already escaped, create a new URI from the string representation of the URL
	}

	/**Creates a URN in the form <code>urn:<var>nid</var>:nss</code>.
	@param nid The namespace identifier.
	@param nss The namespace-specific string.
	@return A URN based upon the given parameters.
	@see http://www.ietf.org/rfc/rfc2141.txt
	@throws  IllegalArgumentException if the resulting string violates RFC&nbsp;2396.
	*/
	public static URI createURN(final String nid, final String nss)
	{
		return URI.create(URN_SCHEME+SCHEME_SEPARATOR+nid+SCHEME_SEPARATOR+nss);	//construct and return the URN
	}

	/**Creates a URI from the given URI string relative to the given context object.
	@param contextObject The source context, such as a URL, a URI, a File, or <code>null</code> if
		the filename should not be referenced from any object.
	@param string The string version of a URI, either relative or absolute, or a URI
		fragment beginning with "#".
	@return A URI constructed from the URI string and context object.
	@exception URISyntaxException Thrown if the context object and string cannot be used to create a valid URI.
	@see File
	@see URI
	@see URL
	*/
	public static URI createURI(final Object contextObject, final String string) throws URISyntaxException	//TODO maybe delete this eventually
	{
		if(contextObject instanceof URI)	//if the context is a URI
		{
//TODO if the string contains illegal URI characters, such as spaces, this won't work
//TODO also check to see if the string is null.
			return ((URI)contextObject).resolve(new URI(string));	//resolve the URI form of the string, creating a URISyntaxException if there is a problem
		}
		else if(contextObject instanceof URL)	//if the context is a URL
		{
			return createURI(((URL)contextObject));	//convert the URL to a URI and use it as a context
		}
		else if(contextObject instanceof File)	//if the context object is a file
		{
			return createURI(((File)contextObject).toURI(), string);	//convert the File to a URI and use it as a context
		}
		else	//if we don't recognize the context object
		{
			return new URI(string);	//create a new URI from the string, ignoring the context object
		}
	}

	/**Creates an absolute URI from the given string, guessing what the string represents.
	<p>If the string is not a valid URL (e.g. it contains a space), this method
		assumes that a file was intended and a file URI is constructed.</p>
	<p>This method is convenient for creating URIs based upon user input.</p>
	@param string The string to convert to a URI. 
	@return A URI representing the contents of the string, interpreted in a
		lenient fashion.
	*/
	public static URI guessAbsoluteURI(final String string)
	{
Debug.trace("guessing URI: ", string);
		try
		{
			final URI uri=new URI(string);	//see if the string is already a valid URI
			if(uri.isAbsolute())	//if the URI is absolute
			{
				return uri;	//return the URI
			}
			else	//if the URI is not absolute
			{
				return new File(string).toURI();	//a local file must have been requested				
			}
		}
/*G***del if not needed
		catch(IllegalArgumentException illegalArgumentException)	//if the string is not an absolute URI
		{
			return new File(string).toURI();	//construct a file object and convert that to a URI
		}
*/
		catch(URISyntaxException uriSyntaxException)	//if the string is not a valid URI
		{
			return new File(string).toURI();	//construct a file object and convert that to an absolute URI
		}
	}

	/**Returns a URL representing the directory of the given file URL. (It is
		assumed that the given URL represents a file.)
	@param url The URL of a file.
	@return A URL of the file's directory, ending with '/'.
	@throws MalformedURLException Thrown if a directory URL cannot be created.
	*/
	public static URL getDirectoryURL(final URL url) throws MalformedURLException
	{
		return new URL(url, ".");  //create a new URL from the directory of the URL G***use a constant here
	}

	/**Returns the unencoded host and optional port of the given URI.
	@param uri The URI from which to extract the host and optional port.
	@return The host name and optional port of the given URI,
		or <code>null</code> if there is no host specified in the given URI.
	*/ 
	public static Host getHost(final URI uri)
	{
		final String host=uri.getHost();	//get the host
		final int port=uri.getPort();	//get the port
		return host!=null ? new Host(host, port) : null;	//if there is a hostname, return the host information
/*G***del		
		if(host!=null)	//if a host is given
		{
			final int port=uri.getPort();	//get the port
			if(port>=0)	//if a port is given
			{
				return new StringBuilder(host).append(PORT_SEPARATOR).append(port).toString();	//append the port
			}
			else	//if no port is given
			{
				return host;	//just return the host
			}
		}
		else	//if no host was given
		{
			return null;	//show that no host can be returned
		}
*/
	}

	/**Returns the unencoded path, optional unencoded query, and optional unencoded fragment of the given URI.
	@param uri The URI from which to extract the path, optional query, and optional fragment.
	@return An unencoded path in the form <code>[<var>path</var>][?<var>query</var>][#<var>fragment</var>]</code>,
		or <code>null</code> if none of those components are present in the given URI.
	*/ 
	public static String getRawPathQueryFragment(final URI uri)
	{
		final StringBuilder stringBuilder=new StringBuilder();
		final String rawPath=uri.getRawPath();	//get the path
		if(rawPath!=null)	//if there is a path
		{
			stringBuilder.append(rawPath);	//path
		}
		final String rawQuery=uri.getRawQuery();	//get the query
		if(rawQuery!=null)	//if there is a query
		{
			stringBuilder.append(QUERY_SEPARATOR).append(rawQuery);	//?query
		}
		final String rawFragment=uri.getRawFragment();	//get the fragment
		if(rawFragment!=null)	//if there is a fragment
		{
			stringBuilder.append(FRAGMENT_SEPARATOR).append(rawFragment);	//#query			
		}
			//if any of the components were present (which is distinct from them having string content), return the constructed string; otherwise, return null
		return rawPath!=null || rawFragment!=null || rawFragment!=null ? stringBuilder.toString() : null;		
	}
	
	/**Returns a relative path to the URL from the given context URL.
		This version requires the file to be on the same branch of the context
		path (e.g. "http://abc.de/a/c/d.html" is not on the same branch of
		"http://abc.de/a/b").
G***del The context URL must be a URL of a directory, ending with the directory divider character '/'
	@param contextURL The reference URL to use in making the relative
		path.
	@param url The URL for which a relative path should be returned, in relation
		to the context URL.
	@return A relative path to the URL in relation to the context URL.
	@throws MalformedURLException Thrown if a relative URL cannot be
		determined from the context URL.
	*/
/*G***fix
	public static String getRelativePath(final URL contextURL, final URL url) throws MalformedURLException
	{

		  //G***check this new implementation; this simply chops off everything that matches

		if(urlPath.startsWith(directoryURLPath)) //if the directory URL path is at the beginning of the URL path
		{
			final String relativePath=urlPath.substring(directoryURLPath.length());  //get everything after the directory URL
			return relativePath;  //return the relative path
		}
		throw new MalformedURLException("Cannot create relative path for "+url+" from context "+contextURL);  //show that we couldn't determine a relative path
	}
*/

	/**Opens a connection to the given URL, recognizing redirects.
		This method was inspired by the source code to
		<code>javax.swing.JTextPane.getStream()</code>.
	@param url The URL a connection to which should be opened.
	@return A connection to the given URL or the URL to which it redirects.
	*/
/*G***fix; we need to leave the old version in XMLTextPane because it changes the URL appropriately instead of just automatically redirecting
	public static URLConnection openConnection(final URL url)//G***fix throws IOException
	{
		final URLConnection urlConnection=url.openConnection(); //open a connection to the URL
		if(urlConnection instanceof HttpURLConnection)  //if this is a HTTP connection
		{
		  final HttpURLConnection httpURLConnectoin=(HttpURLConnection)urlConnection; //cast the
	    hconn.setInstanceFollowRedirects(false);
	    int response = hconn.getResponseCode();
	    boolean redirect = (response >= 300 && response <= 399);

//G***del In the case of a redirect, we want to actually change the URL
//G***del that was input to the new, redirected URL

	    if (redirect) {
		String loc = conn.getHeaderField("Location");
		if (loc.startsWith("http", 0)) {
		    page = new URL(loc);
		} else {
		    page = new URL(page, loc);
		}
		return getStream(page);
	    }
	}
	String contentType = conn.getContentType();
	if(contentType!=null) //if we receive at least a guess of the content type
	{
*/

	/**Loads the contents of a URL into an array of bytes.
	@param url The URL from which to read.
	@return An array of bytes from the URL.
	@exception IOException Thrown if there is an error loading the bytes.
	@see InputStreamUtilities#getBytes
	*/
/*G***fix
	public static byte[] readBytes(final URL url) throws IOException
	{
		final InputStream urlInputStream=url.openConnection().getInputStream();  //create an input stream to the URL
		try
		{
			return InputStreamUtilities.getBytes(urlInputStream);  //convert the URL to an array of bytes
		}
		finally
		{
			urlInputStream.close();  //always close the URL input stream
		}
	}
*/

	/**Loads the contents of a URL into a string.
	@param url The URL from which to read.
	@param encoding The encoding (such as UTF-8) used to store the string.
	@return A string containing the contents of the URL.
	@exception IOException Thrown if there is an error loading the bytes.
	*/
/*G***fix
	public static String readString(final URL url, final String encoding) throws IOException
	{
		final byte[] bytes=readBytes(url); //load the contents of the URL
		return new String(bytes, encoding); //convert the bytes into a string, using the given encoding
	}
*/

	/**Determines the relative path of the given absolute path by removing the root path '/' character from the beginning of the path.
	@param absolutePath The absolute path to convert to a relative path.
	@return A relative path from the root of the absolute path.
	@exception IllegalArgumentException if the given path is not absolute.
	*/
	public static String getRelativePath(final String absolutePath)
	{
		if(!isAbsolutePath(absolutePath))	//if the path is not really absolute
		{
			throw new IllegalArgumentException("Path is not absolute: "+absolutePath);
		}
		return absolutePath.substring(ROOT_PATH.length());	//remove the beginning root path indicator
	}

	/**Determines whether the path of the URI is a canonical container path.
	@param uri The URI the path of which to examine.
	@return <code>true</code> if the path of the given URI ends with a slash ('/').
	@see #isContainerPath(String)
	*/
	public static boolean isContainerPath(final URI uri)
	{
		return isContainerPath(uri.getRawPath());	//see if the path ends with '/' (use the raw path in case the last character is an encoded slash)		
	}

	/**Determines whether the given path is a canonical container path.
	@param path The path to examine.
	@return <code>true</code> if the path ends with a slash ('/').
	*/
	public static boolean isContainerPath(final String path)
	{
		return endsWith(path, PATH_SEPARATOR);	//see if the path ends with '/'		
	}

	/**Determines whether the path of the URI (which may or may not be absolute) is absolute.
	@param uri The URI the path of which to examine.
	@return <code>true</code> if the path of the given URI begins with a slash ('/').
	@see #isAbsolutePath(String)
	*/
	public static boolean isAbsolutePath(final URI uri)
	{
		return isAbsolutePath(uri.getRawPath());	//see if the path begins with '/' (use the raw path in case the first character is an encoded slash)		
	}

	/**Determines whether the given path is absolute.
	@param path The path to examine.
	@return <code>true</code> if the path begins with a slash ('/').
	@exception NullPointerException if the path is <code>null</code>.
	*/
	public static boolean isAbsolutePath(final String path)
	{
		return checkNull(path, "Path cannot be null").startsWith(ROOT_PATH);	//see if the path begins with '/'		
	}

	/**Determines whether the URI contains only a host and optional port.
	@param uri The URI the path of which to examine.
	@return <code>true</code> if the URI contains only a host and optionally a port.
	*/
	public static boolean isHost(final URI uri)
	{
		return uri.getHost()!=null	//a host URI contains only a host and nothing else except maybe a port
				&& uri.getScheme()==null && uri.getUserInfo()==null && uri.getPath()==null && uri.getQuery()==null && uri.getFragment()==null;
	}

	/**Creates a URL from a URI. If a valid URL cannot be
		formed, <code>null</code> is returned.
	@param uri The URI to convert to a URL, or <code>null</code>
		if no URI is available (in which case <code>null</code> will
		be returned).
	@return The URL form of the URI, or <code>null</code>
		if the URI cannot be converted to a valid URL. 
	*/ 
	public static URL toValidURL(final URI uri)
	{
		try
		{
//TODO we probably want to check for the condition java.lang.IllegalArgumentException: URI is not absolute
			return uri!=null ? uri.toURL() : null;	//convert the URI to a URL, if we have a URI	
		}
		catch (MalformedURLException e)	//if there was an error converting to a URL
		{
			return null;	//show that we couldn't create a valid URL from the given URI
		}
	}

	/**Returns a URI constructed from a given URI and a fragment identifier.
	<p>If the URI is not syntactically correct, an
		<code>IllegalArgumentException</code>	will be thrown.
	<p>This method should normally only be used when the format
		of the string is known to be a syntactically correct URI.</p>
	<p>If no URI is provided, a URI is created from the fragment itself.</p>
	@param uri The URI to which to add a fragement identifier, or <code>null</code> if a URI chould be created from just the fragment.
	@param fragment The fragment to add to the end of the URI.
	@exception IllegalArgumentException Thrown if the a URI cannot be constructed from the given information.
	@see URI#create
	*/
	public static URI resolveFragment(final URI uri, final String fragment) throws IllegalArgumentException
	{
		final String fragmentSuffix=new StringBuilder().append(FRAGMENT_SEPARATOR).append(fragment).toString();	//create a suffix that includes the fragment separator and the fragment
		if(uri!=null)	//if there is a URI
			return uri.resolve(fragmentSuffix);	//resolve the fragment against the URI
		else	//if there is no URI
			return URI.create(fragmentSuffix);	//create a URI from the fragment suffix itself 
	}

	/**Returns a URI constructed from the given parts, any of
		which can be <code>null</code>.
	<p>If the URI is not syntactically correct, an
		<code>IllegalArgumentException</code>	will be thrown, created from the
		<code>URISyntaxException</code>.</p>
	<p>This method should normally only be used when the format
		of the string is known to be a syntactically correct URI.</p>
	@param scheme The name of the URI scheme.
	@param ssp The scheme-specific part.
	@param fragment The fragment at the end of the URI.
	@exception IllegalArgumentException Thrown if the a URI cannot be constructed
		from the given strings.
	@see URI#create
	*/
	public static URI create(final String scheme, final String ssp, final String fragment) throws IllegalArgumentException
	{
		try
		{
			return new URI(scheme, ssp, fragment);	//create and return a new URI
		}
		catch(URISyntaxException uriSyntaxException)
		{
			throw (IllegalArgumentException)new IllegalArgumentException(uriSyntaxException.getMessage()).initCause(uriSyntaxException);	//create a new illegal argument exception from the URI syntax exception and rethrow it
		}	
	}

	/**Returns a URI constructed from the given parts, any of
		which can be <code>null</code>.
	<p>If the URI is not syntactically correct, an
		<code>IllegalArgumentException</code>	will be thrown, created from the
		<code>URISyntaxException</code>.</p>
	<p>This method should normally only be used when the format
		of the string is known to be a syntactically correct URI.</p>
	@param scheme The name of the URI scheme.
	@param userInfo The user information.
	@param host The host information.
	@param port The port number, or -1 for no defined port.
	@param path The path, correctly encoded.
	@param query The URI query.
	@param fragment The fragment at the end of the URI.
	@exception IllegalArgumentException Thrown if the a URI cannot be constructed
		from the given strings.
	@see URI#create
	*/
	public static URI create(final String scheme, final String userInfo, final String host, final int port, final String path, final String query, final String fragment) throws IllegalArgumentException
	{
		try
		{
			return new URI(scheme, userInfo, host, port, path, query, fragment);	//create and return a new URI
		}
		catch(URISyntaxException uriSyntaxException)	//if the given information is not correct URI syntax
		{
			throw (IllegalArgumentException)new IllegalArgumentException(uriSyntaxException.getMessage()).initCause(uriSyntaxException);	//create a new illegal argument exception from the URI syntax exception and rethrow it
		}	
	}

	/**Stores the contents of a URL in an output stream.
	@param url The URL to copy.
	@param outputStream The destination of the URL contents.
	@exception IOException Thrown if there is an error copying the URL.
	*/
/*G***fix
	public static void write(final URL url, final OutputStream outputStream) throws IOException
	{
		final InputStream fileInputStream=new BufferedInputStream(url.openConnection().getInputStream()); //created a buffered input stream to the URL
		try
		{
			OutputStreamUtilities.write(fileInputStream, outputStream);  //copy the contents of the input stream to the output stream
		}
		finally
		{
			fileInputStream.close();  //always close the file input stream
		}
	}
*/


	/**Encodes the URI reserved characters in the string,
		using '%' as an escape character, according to the URI encoding rules 
		in <a href="http://www.ietf.org/rfc/rfc2396.txt">RFC 2396</a>,
		"Uniform Resource Identifiers (URI): Generic Syntax".
	@param uri The data to URI-encode.
	@return A string containing the escaped data.
	@see URIConstants#ESCAPE_CHAR
	@see URIConstants#NORMAL_CHARS
	*/
	public static String encode(final String uri)
	{
		return escapeHex(uri, NORMAL_CHARS, null, ESCAPE_CHAR, 2);	//escape according to URI encoding rules		
	}

	/**Decodes the escaped ('%') characters in the character iterator
		according to the URI encoding rules in
		<a href="http://www.ietf.org/rfc/rfc2396.txt">RFC 2396</a>,
		"Uniform Resource Identifiers (URI): Generic Syntax".
	@param uri The data to URI-decode.
	@return A string containing the unescaped data.
	@see URIConstants#ESCAPE_CHAR
	*/
	public static String decode(final String uri)
	{
		return unescapeHex(uri, ESCAPE_CHAR, 2);	//unescape according to URI encoding rules
	}

		//variables for fixing a JDK URI.resolve() bug
	private final static String EXPECTED_URI_PREFIX="file:////";
	private final static String RESULT_URI_PREFIX="file:/";

	/**Changes a URI from one base to another.
	For example, <code>http://example.com/base1/test.txt</code> changed to base
		<code>http://example.com/base2/level2/</code> yields
		<code>http://example.com/base2/level2/test.txt</code>.
	<p>This method contains a workaround for the JDK 5.0 bug that chops off the
		first few forward slashes for Windows network names.</p>
	@param uri The URI the base of which to change.
	@param oldBaseURI The current base URI.
	@param newBaseURI The base URI of the new URI to construct.
	@return A new URI constructed by relativizing the URI to the old base URI and
		resolving the resulting URI agains the new base URI.
	@see URI#relativize(URI)
	@see URI#resolve(URI)
	@exception IllegalArgumentException Thrown if <var>oldBaseURI</code> is not
		a base URI of <var>uri</var>.
	*/
	public static URI changeBase(final URI uri, final URI oldBaseURI, final URI newBaseURI)
	{
//G***del Debug.trace("changing base of ", uri, "from", oldBaseURI, "to", newBaseURI);
		final URI relativeURI=oldBaseURI.relativize(uri);	//get a URI relative to the old base URI
		if(relativeURI.isAbsolute())	//if we couldn't relativize the the URI to the old base URI and come up with a relative URI
		{
			throw new IllegalArgumentException(oldBaseURI.toString()+" is not a base URI of "+uri);
		}
		URI newURI=newBaseURI.resolve(relativeURI);	//resolve the relative URI to the new base URI
//G***del Debug.trace("new URI:", newURI);
		final String newBaseURIString=newBaseURI.toString();	//get the string of the new base URI
//G***del; maybe not needed		final StringBuilder newURIStringBuilder=new StringBuilder(newURI.toString());	//get the string of the new URI
		final String newURIString=newURI.toString();	//get the string version of the new URI
/*G***del
			//if the old URI ended with '/' but the new URI doesn't (this can occur when the new URI references a directory
		if(endsWith(uri.toString(), PATH_SEPARATOR) && !endsWith(newURIStringBuilder, PATH_SEPARATOR))
		{
			
		}
*/
			//check for the JDK 5.0 bug that chops off the first few forward slashes for Windows network names
		if(!newURIString.startsWith(newBaseURIString))	//if the new URI doesn't start with the new base URI we were expecting
		{
			if(newBaseURIString.startsWith(EXPECTED_URI_PREFIX) && newURIString.startsWith(RESULT_URI_PREFIX))				
			{
				final String fixedURIString=EXPECTED_URI_PREFIX+newURIString.substring(RESULT_URI_PREFIX.length());	//replace the incorrect beginning section
				newURI=URI.create(fixedURIString);	//return create a URI that goes back to the new base URI we expected
			}
			else	//if this is a different bug than we expected
			{
				throw new AssertionError(newURIString+" does not begin with expected new base URI "+newBaseURIString);
			}
		}
		return newURI;	//return the new URI with the changed base
	}

	/**Characters that can appear in a URI path with no escape sequences.*/
//G***del	protected final static String COMPRESS_CHARS=ALPHA_CHARS+DIGIT_CHARS;	//length 49

	/**Compresses a URI into a shorter string representation.
	The resulting string consists only of URI <code>xalpha</code> characters with no escape sequences.
	@param uri The URI to compress.
	@return A compressed string representation of the URI.
	*/
/*G***fix
	public static String compress(final URI uri)
	{
		final int INPUT_CHAR_WIDTH=6;	//no URI character can be more than six bits wide 
		final int COMPRESS_BASE=NORMAL_CHARS.length();	//this is the numbering base we'll use for compression 
		final String uriString=uri.toString();
		final StringBuilder stringBuilder=new StringBuilder();
		int accumulator=0;	//we'll accumulate bits here
		int bit=0;	//this is how many bits we have to shift an incoming character
		for(int i=uriString.length()-1; i>=0; --i)
		{
			final char character=uriString.charAt(i);	//get the next character to compress
			accumulator|=character<<bit;	//combine our value with the character's value
			bit+=INPUT_CHAR_WIDTH;	//we'll have to shift the next character up above the bits we just added
			while(accumulator>COMPRESS_BASE)	//while the value is more than our conversion base
			{
				final index
				
			}
		}
	}
*/

	/**URI alphabetic and digit characters.*/
	private final static String COMPRESS_NORMAL_CHARS=ALPHA_CHARS+DIGIT_CHARS;	//"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" length 62

	/**Characters that will be compressed.*/
	private final static String OTHER_CHARS=SAFE_CHARS+EXTRA_CHARS+ESCAPE_CHAR+RESERVED_CHARS;	//"$-_@.&!*\"'(),%=;/#?: " length 21

	/**Characters that can appear in a URI path with no escape sequences.*/
	private final static String COMPRESS_ENCODE_CHARS="-_()@";	//length 5

	/**Compresses a URI into a shorter string representation.
	@param uri The URI to compress.
	@return A compressed string representation of the URI.
	*/
	public static String safeEncode(final URI uri)
	{
		final int ENCODE_BASE=COMPRESS_ENCODE_CHARS.length();	//this is the base into which we'll encode certain characters
		final String uriString=uri.toString();
		final StringBuilder stringBuilder=new StringBuilder();
		for(int i=0; i<uriString.length(); ++i)	//look at each URI character
		{
			final char character=uriString.charAt(i);	//get the next character
			if(COMPRESS_NORMAL_CHARS.indexOf(character)>=0)	//if this is a normal character
			{
				stringBuilder.append(character);	//add the character normally
			}
			else	//if this is a character to be encoded
			{
				final int index=OTHER_CHARS.indexOf(character);	//get the character's index within our set of special characters
				assert index>=0 : "Invalid URI character: "+character;	//if the character came in the URI object, it should always be valid
				final int high=index/ENCODE_BASE;	//get the high bits of our encoding
				final int low=index%ENCODE_BASE;	//get the high bits of our encoding
				stringBuilder.append(COMPRESS_ENCODE_CHARS.charAt(high));	//add a character to represent our high bits
				stringBuilder.append(COMPRESS_ENCODE_CHARS.charAt(low));	//add a character to represent our low bits
			}
		}
		return stringBuilder.toString();	//return our encoded URI string
	}

	/**Compresses a URI into a shorter string representation.
	@param string The alphanumeric string.
	@return An uncompressed URI from the alphanumeric string.
	@exception SyntaxException Thrown if the given string is not correctly encoded. 
	*/
	public static URI safeDecode(final String string) throws SyntaxException
	{
		final int ENCODE_BASE=COMPRESS_ENCODE_CHARS.length();	//this is the base into which we'll encode certain characters TODO maybe place this outside the method
		final StringBuilder stringBuilder=new StringBuilder();
		for(int i=0; i<string.length(); ++i)	//look at each character
		{
			final char character=string.charAt(i);	//get the next character
			if(COMPRESS_NORMAL_CHARS.indexOf(character)>=0)	//if this is a normal character
			{
				stringBuilder.append(character);	//add the character normally
			}
			else	//if this is a character to be encoded
			{
				final int high=COMPRESS_ENCODE_CHARS.indexOf(character);	//get the high bits
				if(high<0)	//if the high character wasn't recognized
				{
					throw new SyntaxException(string, "Invalid character.");	//indicate that an unexpected character was encountered					
				}
				if(i==string.length()-1)	//if there are no more characters
				{
					throw new SyntaxException(string, "Incomplete encoding sequence.");	//indicate that the encoding character was not present.
				}
				final int low=COMPRESS_ENCODE_CHARS.indexOf(string.charAt(++i));	//go to the next character and get its index
				if(low<0)	//if the low character wasn't recognized
				{
					throw new SyntaxException(string, "Invalid character.");	//indicate that an unexpected character was encountered					
				}
				final int index=high*ENCODE_BASE+low;	//get the index of the original character
				if(index>=OTHER_CHARS.length())	//if the resulting sequence does not match one of our original characters
				{
					throw new SyntaxException(string, "Invalid encoding sequence.");	//indicate that the encoding resulted in an invalid sequence					
				}
				stringBuilder.append(OTHER_CHARS.charAt(index));	//add the encoded character to our string builder
			}
		}
		return URI.create(stringBuilder.toString());	//return the original URI
	}

}
