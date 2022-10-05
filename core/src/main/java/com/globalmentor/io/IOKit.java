/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.io;

import java.io.*;
import java.net.URI;

/**
 * Provides reading and writing functionality for an object.
 * @author Garret Wilson
 * @deprecated
 */
@Deprecated
public interface IOKit<T> extends URIAccessible {

	/**
	 * Loads an object from a given URI.
	 * @param uri The URI that identifies the object to be loaded.
	 * @return The object containing the data of the resouce represented by the URI.
	 * @throws IOException Thrown if there is an error reading the object.
	 */
	public T load(final URI uri) throws IOException;

	/**
	 * Loads an object from an input stream.
	 * @param inputStream The input stream from which to read the data.
	 * @param baseURI The base URI of the content, or <code>null</code> if no base URI is available.
	 * @return The object loaded from the given input stream.
	 * @throws IOException Thrown if there is an error reading the data.
	 */
	public T load(final InputStream inputStream, final URI baseURI) throws IOException;

	/**
	 * Saves an object to a given URI.
	 * @param object The object the data of which will be saved at the given URI.
	 * @param uri The URI at which the object should be saved.
	 * @throws IOException Thrown if there is an error writing the object.
	 */
	public void save(final T object, final URI uri) throws IOException;

	/**
	 * Saves an object to an output stream.
	 * @param object The object the data of which will be written to the given output stream.
	 * @param outputStream The output stream to which to write the object content.
	 * @throws IOException Thrown if there is an error writing the object.
	 */
	public void save(final T object, final OutputStream outputStream) throws IOException;

}
