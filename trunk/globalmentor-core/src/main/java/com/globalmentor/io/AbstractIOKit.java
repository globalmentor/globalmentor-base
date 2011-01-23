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

/**Abstract implementation of reading and writing functionality for an object.
@author Garret Wilson
@deprecated
*/
public abstract class AbstractIOKit<T> extends DefaultURIAccessible implements IOKit<T>
{

	/**Default constructor.*/
	public AbstractIOKit()
	{
		this(null, null);
	}

	/**URI input stream locator constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	*/
	public AbstractIOKit(final URIInputStreamable uriInputStreamable)
	{
		this(uriInputStreamable, null);
	}

	/**URI output stream locator constructor.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public AbstractIOKit(final URIOutputStreamable uriOutputStreamable)
	{
		this(null, uriOutputStreamable);
	}

	/**Full constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public AbstractIOKit(final URIInputStreamable uriInputStreamable, final URIOutputStreamable uriOutputStreamable)
	{
		super(uriInputStreamable, uriOutputStreamable);	//construct the parent class
	}

	/**Loads an object from given URI.
	<p>This version opens a stream and delegates to <code>load(InputStream, URI)</code>.</p>
	@param uri The URI that identifies the resource to be loaded.
	@return An object containing the data of the resouce represented by the URI.
	@exception IOException Thrown if there is an error reading the object.
	*/
	public T load(final URI uri) throws IOException
	{
			//get an input stream to the resource
		final InputStream inputStream=getInputStream(uri);
		try
		{
			return load(inputStream, uri);	//load the object from the input stream
		}
		finally
		{
			inputStream.close();	//always close the input stream
		}
	}

	/**Saves an object to a given URI.
	<p>This version opens a stream and delegates to <code>save(T, OutputStream)</code>.</p>
	@param object The object the data of which will be saved at the given URI.
	@param uri The URI at which the object should be saved.
	@exception IOException Thrown if there is an error writing the object.
	*/
	public void save(final T object, final URI uri) throws IOException
	{
		final OutputStream outputStream=getOutputStream(uri);	//get an output stream to this URI
		try
		{
			save(object, outputStream);	//write to the output stream
		}
		finally
		{
			outputStream.close();	//always close the output stream
		}
	}

}
