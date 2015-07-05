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

import com.globalmentor.io.URIAccessible;

/**
 * Represents an implementation for loading information into a view or saving information from a view.
 * @author Garret Wilson
 * @see Modifiable
 */
public interface ViewIOKit<M> extends URIAccessible {

	/**
	 * Loads data into a view from a given URI.
	 * @param view The view into which the data should be loaded.
	 * @param uri The URI that identifies the resource to be loaded.
	 * @throws IOException Thrown if there is an error reading the data.
	 */
	public void load(final ModelView<M> view, final URI uri) throws IOException;

	/**
	 * Loads data into a view from an input stream.
	 * @param view The view into which the data should be loaded.
	 * @param inputStream The input stream from which to read the data.
	 * @param baseURI The base URI of the content, or <code>null</code> if no base URI is available.
	 * @throws IOException Thrown if there is an error reading the data.
	 */
	public void load(final ModelView<M> view, final InputStream inputStream, final URI baseURI) throws IOException;

	/**
	 * Saves a view to a given URI.
	 * <p>
	 * If saving is successful and the view is <code>Modifiable</code>, the view's modified status is set to <code>false</code>.
	 * </p>
	 * <p>
	 * A calling program should first call the view's <code>verify()</code> method to ensure the data is valid and that the model reflects the currently entered
	 * data.
	 * </p>
	 * @param view The view the data of which will be saved at the given URI.
	 * @param uri The URI at which the view data should be saved.
	 * @throws IOException Thrown if there is an error writing the data.
	 * @see com.globalmentor.model.Modifiable
	 * @see Verifiable#verify()
	 */
	public void save(final ModelView<M> view, final URI uri) throws IOException;

	/**
	 * Saves a view to an output stream.
	 * <p>
	 * If saving is successful and the view is <code>Modifiable</code>, the view's modified status is set to <code>false</code>.
	 * </p>
	 * <p>
	 * A calling program should first call the view's <code>verify()</code> method to ensure the data is valid and that the model reflects the currently entered
	 * data.
	 * </p>
	 * @param view The view the data of which will be written to the given output stream.
	 * @param outputStream The output stream to which to write the model content.
	 * @throws IOException Thrown if there is an error writing the data.
	 * @see com.globalmentor.model.Modifiable
	 * @see Verifiable#verify()
	 */
	public void save(final ModelView<M> view, final OutputStream outputStream) throws IOException;

}
