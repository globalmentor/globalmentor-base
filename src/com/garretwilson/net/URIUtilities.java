package com.garretwilson.net;

import java.io.*;
import java.net.*;
import com.garretwilson.io.*;
import java.net.*;
import java.net.URI;	//G***del when other URI is removed


/**Various URL manipulating functions.
@see java.net.URL
*/
public class URIUtilities implements URIConstants	//G***del InputStreamLocator
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
	@param url The URI for which to return a file name.
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
		if no known media type is associated with this URL's extension.
	@see MediaType#getMediaType
	*/
	public static MediaType getMediaType(final URI uri)
	{
		return MediaType.getMediaType(FileUtilities.getExtension(getFile(uri))); //return the media type based on the extension of the URI filename
	}

	/**Creates a URL from the given filename relative to the given context object.
		This correctly creates URLs when the filename is a fragment beginning with "#".
	@param contextObject The source context, such as a URL or File, or <code>null</code> if
		the filename should not be referenced from any object.
	@param filename The name of the file, either relative or absolute, or a URL
		fragment beginning with "#".
	@return A URL constructed from the filename and context object, or
		<code>null</code> if no URL could be constructed.
	@exception MalformedURLException Thrown if the filename is not a valid filename or URL name.
	@see File
	@see URL
	*/
/*G***fix	
	public static URL createURL(Object contextObject, final String filename) throws MalformedURLException
	{
		URL url=null;	//we'll use this variable to store the new URL we create
		try
		{
			if(contextObject!=null)	//if we know where we're getting its data from
			{
				if(contextObject instanceof URL)	//if the data is coming from a URL
				{
//G***del Debug.trace("Context object is a URL: ", contextObject);	//G***del
//G***del Debug.trace("filename: ", filename);	//G***del
					final URL contextURL=(URL)contextObject;	//cast the contect object to a URL
					url=new URL(contextURL, filename);	//create a new URL from the old one
//G***del Debug.trace("new url: ", url);  //G***del
					//Since in Java 1.2 new URL("file://...", "#...") causes the filename
					//	to be lost, create a workaround.
					//This workaround is modified from code in
					//	javax.swing.text.html.HTMLEditorKit.java version 1.96 02/02/00 by
					// 	Timothy Prinzing.

					if(filename!=null && FILE_PROTOCOL.equals(url.getProtocol())  //if there is an href, it starts with "#", and the document base is a file
						&& filename.length()>0 && filename.charAt(0)==FRAGMENT_SEPARATOR_CHAR)
					{
				    final String baseFile=contextURL.getFile();	//get the context base URL as a file string
				    final String newFile=url.getFile();	//get a string from the URL we created
				    if(baseFile!=null && newFile!=null && !newFile.startsWith(baseFile))	//if the URL doesn't already start with the base URL
							url=new URL(contextURL, baseFile+filename);	//append the href to the base URL
					}
//G***del System.out.println("created new URL: "+newURL);	//G***del
				}
				//G***check for an instance of File here
			}
//G***del		InputStream inputStream;	//we don't know where our input stream is coming from, yet
			if(url==null)	//if we haven't found a URL, yet
				url=new URL(filename);	//try to create one directly from the filename they give us
		}
		catch(MalformedURLException e)	//if the location isn't a valid URL
		{
//G***del System.out.println(filename+" must be a file.");	//G***del
			url=new File(filename).toURL();	//create a file object and convert that to a URL
			//G***check for MalformedURLException
		}
		return url;	//return the URL we created
		//G***do something if we can't read from the URL
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

	/**Returns an input stream from given URL by establishing a connection to
		the requested URL.
		This method fulfills the requirements of <code>InputStreamLocator</code>.
	@param url A complete URL to a file.
	@return An input stream to the contents of the file represented by the given URL.
	@exception IOException Thrown if an I/O error occurred.
	@see InputStreamLocator
	*/
	public InputStream getInputStream(final URL url) throws IOException
	{
		return url.openConnection().getInputStream();	//open a connection to the URL and return an input stream to that connection
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
	public static URI create(final String scheme, final String ssp, final String fragment) throws IllegalArgumentException
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

}
