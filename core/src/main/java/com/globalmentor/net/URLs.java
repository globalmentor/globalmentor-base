/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.net;

import java.io.*;
import java.net.*;
import java.nio.charset.Charset;

import com.globalmentor.io.*;

import static com.globalmentor.net.URIs.*;

/**
 * Various URL manipulating functions.
 * @author Garret Wilson
 * @see URL
 */
public class URLs {

	/**
	 * Retrieves a <code>File</code> representing the file of the URL.
	 * @param url The URL for which to return a file.
	 * @return A <code>File</code> object representing the file path of the URL with no query string.
	 */
	public static File getFile(final URL url) {
		return new File(url.getPath()); //create a new File from the URL path
	}

	/**
	 * Retrieves the file name of the URL.
	 * @param url The URL for which to return a file name.
	 * @return The name of the file in the URL.
	 * @see #getFile(URL)
	 */
	public static String getFileName(final URL url) {
		return getFile(url).getName(); //return the name of the file we construct from the URL
	}

	/**
	 * Creates a URL from the given filename relative to the given context object. This correctly creates URLs when the filename is a fragment beginning with "#".
	 * @param contextObject The source context, such as a URL or File, or <code>null</code> if the filename should not be referenced from any object.
	 * @param filename The name of the file, either relative or absolute, or a URL fragment beginning with "#".
	 * @return A URL constructed from the filename and context object, or <code>null</code> if no URL could be constructed.
	 * @throws MalformedURLException Thrown if the filename is not a valid filename or URL name.
	 * @see File
	 * @see URL
	 * @deprecated to be removed.
	 */
	@Deprecated
	public static URL createURL(Object contextObject, final String filename) throws MalformedURLException {
		URL url = null; //we'll use this variable to store the new URL we create
		try {
			if(contextObject != null) { //if we know where we're getting its data from
				if(contextObject instanceof URL) { //if the data is coming from a URL
					final URL contextURL = (URL)contextObject; //cast the contect object to a URL
					url = new URL(contextURL, filename); //create a new URL from the old one
					//Since in Java 1.2 new URL("file://...", "#...") causes the filename
					//	to be lost, create a workaround.
					//This workaround is modified from code in
					//	javax.swing.text.html.HTMLEditorKit.java version 1.96 02/02/00 by
					// 	Timothy Prinzing.

					if(filename != null && FILE_SCHEME.equals(url.getProtocol()) //if there is an href, it starts with "#", and the document base is a file
							&& filename.length() > 0 && filename.charAt(0) == FRAGMENT_SEPARATOR) {
						final String baseFile = contextURL.getFile(); //get the context base URL as a file string
						final String newFile = url.getFile(); //get a string from the URL we created
						if(baseFile != null && newFile != null && !newFile.startsWith(baseFile)) //if the URL doesn't already start with the base URL
							url = new URL(contextURL, baseFile + filename); //append the href to the base URL
					}
				}
				//TODO check for an instance of File here
			}
			if(url == null) //if we haven't found a URL, yet
				url = new URL(filename); //try to create one directly from the filename they give us
		} catch(MalformedURLException e) { //if the location isn't a valid URL
			url = new File(filename).toURL(); //create a file object and convert that to a URL
			//TODO check for MalformedURLException
		}
		return url; //return the URL we created
		//TODO do something if we can't read from the URL
	}

	/**
	 * Returns a URL representing the directory of the given file URL. (It is assumed that the given URL represents a file.)
	 * @param url The URL of a file.
	 * @return A URL of the file's directory, ending with '/'.
	 * @throws MalformedURLException Thrown if a directory URL cannot be created.
	 * @deprecated to be removed in favor of working with {@link URI} for path resolution.
	 */
	@Deprecated
	public static URL getDirectoryURL(final URL url) throws MalformedURLException {
		return new URL(url, "."); //create a new URL from the directory of the URL TODO use a constant here
	}

	/**
	 * Returns an input stream from given URL by establishing a connection to the requested URL.
	 * <p>
	 * This method knows how to follow HTTP redirects.
	 * </p>
	 * <p>
	 * This method fulfills the requirements of <code>InputStreamLocator</code>.
	 * </p>
	 * @param url A complete URL to a file.
	 * @return An input stream to the contents of the file represented by the given URL.
	 * @throws IOException Thrown if an I/O error occurred.
	 */
	public static InputStream getInputStream(final URL url) throws IOException {
		return url.openConnection().getInputStream(); //open a connection to the URL and return an input stream to that connection
		/*TODO fix; but this doesn't report back the new URL
				final URLConnection urlConnection=page.openConnection();	//open a connection to the URL
				if(urlConnection instanceof HttpURLConnection) {	//if this is an HTTP URL connection
					HttpURLConnection hconn = (HttpURLConnection) conn;
			    hconn.setInstanceFollowRedirects(false);
			    int response = hconn.getResponseCode();
			    boolean redirect = (response >= 300 && response <= 399);
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
		*/
	}

	/**
	 * Returns a relative path to the URL from the given context URL. This version requires the file to be on the same branch of the context path (e.g.
	 * "http://abc.de/a/c/d.html" is not on the same branch of "http://abc.de/a/b").
	 * @param contextURL The reference URL to use in making the relative path.
	 * @param url The URL for which a relative path should be returned, in relation to the context URL.
	 * @return A relative path to the URL in relation to the context URL.
	 * @throws MalformedURLException Thrown if a relative URL cannot be determined from the context URL.
	 * @deprecated to be removed in favor of working with {@link URI} for path resolution.
	 */
	@Deprecated
	public static String getRelativePath(final URL contextURL, final URL url) throws MalformedURLException {
		//TODO fix this to work with the new URI relativize
		final URL directoryURL = getDirectoryURL(contextURL); //get the directory URL of the context URL
		final String directoryURLPath = directoryURL.getPath(); //get the path of the context URL
		final String urlPath = url.getPath(); //get the path of the URL
		if(urlPath.startsWith(directoryURLPath)) { //if the directory URL path is at the beginning of the URL path
			final String relativePath = urlPath.substring(directoryURLPath.length()); //get everything after the directory URL
			return relativePath; //return the relative path
		}
		throw new MalformedURLException("Cannot create relative path for " + url + " from context " + contextURL); //show that we couldn't determine a relative path
	}

	/**
	 * Reads an object from a URL using the given I/O support.
	 * @param <T> The type of the object to be read.
	 * @param url The URL from which to read.
	 * @param io The I/O support for reading the object.
	 * @return The object read from the URL.
	 * @throws NullPointerException if the given URL and/or I/O support is <code>null</code>.
	 * @throws IllegalArgumentException if the given URL not formatted strictly according to to RFC 2396 and cannot be converted to a URI.
	 * @throws IOException if there is an error reading the data.
	 * @deprecated to be removed or refactored along with the {@link IO} read and write methods.
	 */
	@Deprecated
	public static <T> T read(final URL url, final IO<T> io) throws IOException {
		try (final InputStream bufferedInputStream = new BufferedInputStream(url.openStream())) { //create a buffered input stream to the resource
			return io.read(bufferedInputStream, toURI(url)); //read the object
		}
	}

	/**
	 * Loads the contents of a URL into an array of bytes.
	 * @param url The URL from which to read.
	 * @return An array of bytes from the URL.
	 * @throws IOException Thrown if there is an error loading the bytes.
	 * @see InputStreams#readBytes(InputStream)
	 */
	public static byte[] readBytes(final URL url) throws IOException {
		try (final InputStream urlInputStream = url.openConnection().getInputStream()) { //create an input stream to the URL
			return InputStreams.readBytes(urlInputStream); //convert the URL to an array of bytes
		}
	}

	/**
	 * Loads the contents of a URL into a string.
	 * @param url The URL from which to read.
	 * @param charset The charset used to store the string.
	 * @return A string containing the contents of the URL.
	 * @throws IOException if there is an error loading the bytes.
	 */
	public static String readString(final URL url, final Charset charset) throws IOException {
		final byte[] bytes = readBytes(url); //load the contents of the URL
		return new String(bytes, charset); //convert the bytes into a string, using the given encoding
	}

	/**
	 * Converts a URL to a URI.
	 * <p>
	 * This method functions similar to {@link URL#toURI()}, except that for those few URLs which are not conforming URIs will result in a
	 * {@link IllegalArgumentException} being thrown. In addition, the resulting URI is made to truly conform to RFC 3986 by making sure any non-ASCII characters
	 * are encoded.
	 * </p>
	 * @param url The URI to convert to a URI.
	 * @return The URL as a URI.
	 * @throws IllegalArgumentException if the given URL not formatted strictly according to to RFC 2396 and cannot be converted to a URI.
	 * @see URL#toURI()
	 * @see URIs#canonicalize(URI)
	 */
	public static URI toURI(final URL url) {
		try {
			return URIs.canonicalize(url.toURI());
		} catch(final URISyntaxException uriSyntaxException) {
			throw new IllegalArgumentException(uriSyntaxException);
		}
	}

	/**
	 * Stores the contents of a URL in an output stream.
	 * @param url The URL to copy.
	 * @param outputStream The destination of the URL contents.
	 * @throws IOException Thrown if there is an error copying the URL.
	 */
	public static void write(final URL url, final OutputStream outputStream) throws IOException {
		try (final InputStream bufferedInputStream = new BufferedInputStream(url.openConnection().getInputStream())) { //created a buffered input stream to the URL
			IOStreams.copy(bufferedInputStream, outputStream); //copy the contents of the input stream to the output stream
		}
	}

}
