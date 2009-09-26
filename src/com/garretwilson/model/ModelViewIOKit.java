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

package com.garretwilson.model;

import java.io.*;
import java.net.URI;

import com.globalmentor.io.IOKit;
import com.globalmentor.model.Modifiable;

/**An implementation for loading information into a view or
	saving information from a view using a model's I/O kit.
@author GarretWilson
@see ModelIOKit
*/
public class ModelViewIOKit<M> extends AbstractViewIOKit<M>
{

	/**The implementation for loading and saving the model.*/
	private final IOKit<M> modelIOKit;

		/**@return The implementation for loading and saving the model.*/
		protected IOKit<M> getModelIOKit() {return modelIOKit;}

	/**Constructs a model/view kit from a model's input/output implementation.
	@param modelIOKit The implementation for loading and saving a model.
	*/
	public ModelViewIOKit(final IOKit<M> modelIOKit)
	{
		super(modelIOKit, modelIOKit);	//construct the parent class, using the model I/O kit to retrieve both input and output streams
		this.modelIOKit=modelIOKit;	//save the model I/O kit
	}

	/**Loads data into a view from an input stream.
	@param view The view into which the data should be loaded.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the content, or <code>null</code> if no base
		URI is available.
	@throws IOException Thrown if there is an error reading the data.
	*/ 
	public void load(final ModelView<M> view, final InputStream inputStream, final URI baseURI) throws IOException
	{
		view.setModel(getModelIOKit().load(inputStream, baseURI));	//load the model and put it in the view
	}

	/**Saves a view to an output stream.
	<p>If saving is successful and the view is <code>Modifiable</code>, the
		view's modified status is set to <code>false</code>.</p> 
	<p>A calling program should first call the view's <code>verify()</code>
		method to ensure the data is valid and that the model reflects the
		currently entered data.</p>
	@param view The view the data of which will be written to the given output stream.
	@param outputStream The output stream to which to write the model content.
	@throws IOException Thrown if there is an error writing the data.
	@see Modifiable
	@see Verifiable#verify()
	*/
	public void save(final ModelView<M> view, final OutputStream outputStream) throws IOException
	{
//TODO shouldn't we verify the view here?		if(view.verify())	//verify the view to make sure we have the latest data; if it verifies TODO fix the XML panel, which never allows saving if the source doesn't verify
		
		getModelIOKit().save(view.getModel(), outputStream);	//get the model from the view and save it
		if(view instanceof Modifiable)	//if the view is modifiable
		{
			((Modifiable)view).setModified(false);	//show that the view has not been modified, now that we've saved its contents
		}		
	}

}
