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
			bufferedInputStream.transferTo(outputStream); //copy the contents of the input stream to the output stream
		}
	}

}
