/*
 * Copyright © 1996-2009 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.model;

import java.io.*;
import java.net.URI;

import com.globalmentor.io.*;

/**Absract implementation of loading information into a view or
	saving information from a view.
@author GarretWilson
@see Modifiable
*/
public abstract class AbstractViewIOKit<M> extends DefaultURIAccessible implements ViewIOKit<M>
{

	/**Default constructor.*/
	public AbstractViewIOKit()
	{
		this(null, null);
	}

	/**URI input stream locator constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	*/
	public AbstractViewIOKit(final URIInputStreamable uriInputStreamable)
	{
		this(uriInputStreamable, null);
	}

	/**URI output stream locator constructor.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public AbstractViewIOKit(final URIOutputStreamable uriOutputStreamable)
	{
		this(null, uriOutputStreamable);
	}

	/**Full constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public AbstractViewIOKit(final URIInputStreamable uriInputStreamable, final URIOutputStreamable uriOutputStreamable)
	{
		super(uriInputStreamable, uriOutputStreamable);	//construct the parent class
	}

	/**Loads data into a view from a given URI.
	<p>This version opens a stream and delegates to <code>load(View, InputStream, URI)</code>.</p>
		@param view The view into which the data should be loaded.
	@param uri The URI that identifies the resource to be loaded.
	@throws IOException Thrown if there is an error reading the data.
	*/
	public void load(final ModelView<M> view, final URI uri) throws IOException
	{
			//get an input stream to the resource
		final InputStream inputStream=getInputStream(uri);
		try
		{
			load(view, inputStream, uri);	//load the model from the input stream
		}
		finally
		{
			inputStream.close();	//always close the input stream
		}
	}

	/**Saves a view to a given URI.
	<p>If saving is successful and the view is <code>Modifiable</code>, the
		view's modified status is set to <code>false</code>.</p> 
	<p>A calling program should first call the view's <code>verify()</code>
		method to ensure the data is valid and that the model reflects the
		currently entered data.</p>
	<p>This version opens a stream and delegates to <code>save(View, OutputStream)</code>.</p>
	@param view The view the data of which will be saved at the given URI.
	@param uri The URI at which the view data should be saved.
	@throws IOException Thrown if there is an error writing the data.
	@see com.globalmentor.model.Modifiable
	@see Verifiable#verify()
	*/
	public void save(final ModelView<M> view, final URI uri) throws IOException
	{
		final OutputStream outputStream=getOutputStream(uri);	//get an output stream to this URI
		try
		{
			save(view, outputStream);	//write to the output stream
		}
		finally
		{
			outputStream.close();	//always close the output stream
		}
	}

}
