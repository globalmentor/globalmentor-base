package com.garretwilson.net;

import java.io.*;
import java.net.*;
import com.garretwilson.io.*;
import com.garretwilson.lang.CharSequenceUtilities;
import com.garretwilson.lang.IntegerUtilities;
import com.garretwilson.util.Debug;

/**Various URI manipulating functions for working with URIs as defined in
	in <a href="http://www.ietf.org/rfc/rfc2396.txt">RFC 2396</a>,
	"Uniform Resource Identifiers (URI): Generic Syntax".
@see java.net.URI
*/
public class URIUtilities implements URIConstants, URIInputStreamable
{

	/**Default constructor.*/
	public URIUtilities()
	{
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

	/**Retrieves a <code>File</code> representing the file of the URI.
	@param uri The URI for which to return a file.
	@return A <code>File</code> object representing the file path of the URI
		with no query string.
	*/
	public static File getFile(final URI uri)
	{
		return new File(uri.getPath()); //create a new File from the URI path
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
	@see MediaType#getMediaType
	*/
	public static MediaType getMediaType(final URI uri)
	{
		return MediaType.getMediaType(FileUtilities.getExtension(getFile(uri))); //return the media type based on the extension of the URI filename
	}

	/**Creates a URI from a URL.
	@param url The URL to convert to a URI. The URL should already be properly
		encoded.
	@return The URI form of the URL.
	@exception URISyntaxException Thrown if the URL could not be converted to a URI.
	*/ 
	public static URI createURI(final URL url) throws URISyntaxException
	{
		return new URI(url.toString());	//assuming the URL is already escaped, create a new URI from the string representation of the URL
		/*The following does not work, because it will escape whatever information it gets, doubly-escaping an escaped URL. 
		 *	return new URI(url.getProtocol(), url.getUserInfo(), url.getHost(), url.getPort(), url.getPath(), url.getQuery(), url.getRef()); 
		 */
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
	public static URI createURI(final Object contextObject, final String string) throws URISyntaxException
	{
		if(contextObject instanceof URI)	//if the context is a URI
		{
			try
			{
//TODO if the string contains illegal URI characters, such as spaces, this won't work
//TODO also check to see if the string is null.
				return ((URI)contextObject).resolve(string);	//convert the URL to a URI and use it as a context
			}
			catch(IllegalArgumentException illegalArgumentException)	//if the given string isn't syntactically correct
			{
					//throw a new URI syntax exception from the illegal argument exception
				throw (URISyntaxException)new URISyntaxException(string, illegalArgumentException.getMessage()).initCause(illegalArgumentException);
			}
		}
		else if(contextObject instanceof URL)	//if the context is a URL
		{
			return createURI(createURI((URL)contextObject), string);	//convert the URL to a URI and use it as a context
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

	/**Returns an input stream from given URI by establishing a connection to
		the requested URI.
		<p>This implementation only supports URIs that are URLs.</p>
		<p>This method fulfills the requirements of <code>URIInputStreamable</code>.</p>
	@param uri A complete URL to a file.
	@return An input stream to the contents of the file represented by the given URI.
	@exception IOException Thrown if an I/O error occurred.
	@see URIInputStreamable
	*/
	public InputStream getInputStream(final URI uri) throws IOException
	{
Debug.trace("getting input stream from URI: ", uri);
		return URLUtilities.getInputStream(uri.toURL());	//open a connection to the URI (converted to a URL) and return an input stream to that connection
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

	/**Returns a URI constructed from the string.
	If the URI is not syntactically correct, a runtime exception
	will be thrown, created from the <code>URISyntaxException</code>.
	<p>This method should normally only be used when the format
	of the string is known to be a syntactically correct URI.</p>
	@param string The string from which to construct a URI.
	*/
/*G***del when converted to URI.create	
	public static URI toURI(final String string)
	{
		try
		{
			return new URI(string);	//create and return a new URI
		}
		catch (URISyntaxException e)
		{
			throw new RuntimeException(e);	//throw a runtime exception
		}	
	}
*/

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
			return uri!=null ? uri.toURL() : null;	//convert the URI to a URL, if we have a URI	
		}
		catch (MalformedURLException e)	//if there was an error converting to a URL
		{
			return null;	//show that we couldn't create a valid URL from the given URI
		}
	}

	/**Returns a URI constructed from the given parts, any of
	which can be <code>null</code>.
	If the URI is not syntactically correct, an
	<code>IllegalArgumentException</code>	will be thrown, created from the
	<code>URISyntaxException</code>.
	<p>This method should normally only be used when the format
	of the string is known to be a syntactically correct URI.</p>
	@param scheme The name of the URI scheme.
	@param ssp The scheme-specific part.
	@param fragment The fragment at the end of the URI.
	@exception IllegalArgumentException Thrown if the a URI cannot be constructed
		from the given strings.
	@see URI#create
	*/
	public static URI create(final String scheme, final String ssp, final String fragment) throws IllegalArgumentException	//G***why don't we want to use the URISyntaxException here?
	{
		try
		{
			return new URI(scheme, ssp, fragment);	//create and return a new URI
		}
		catch(URISyntaxException uriSyntaxException)
		{
			final IllegalArgumentException illegalArgumentException=new IllegalArgumentException();	//create a new illegal argument exception
			illegalArgumentException.initCause(uriSyntaxException);	//show what caused it
			throw illegalArgumentException;	//throw the exception
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
	@see URIConstants#ESCAPE_CHARACTER
	@see URIConstants#RESERVED_CHARACTERS
	*/
/*G***del if not needed
	public static String encode(final String uri)
	{
		return CharSequenceUtilities.escapeHex(uri, RESERVED_CHARACTERS, ESCAPE_CHARACTER, 2);	//escape according to URI encoding rules
	}
*/

	/**Decodes the escaped ('%') characters in the character iterator
		according to the URI encoding rules in
		<a href="http://www.ietf.org/rfc/rfc2396.txt">RFC 2396</a>,
		"Uniform Resource Identifiers (URI): Generic Syntax".
	@param uri The data to URI-decode.
	@return A string containing the unescaped data.
	@see URIConstants#ESCAPE_CHARACTER
	*/
/*G***del if not needed
	public static String decode(final String uri)
	{
		return CharSequenceUtilities.unescapeHex(uri, ESCAPE_CHARACTER, 2);	//unescape according to URI encoding rules
	}
*/
}
