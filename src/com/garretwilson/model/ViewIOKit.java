package com.garretwilson.model;

import java.io.*;
import java.net.URI;
import com.garretwilson.io.URIAccessible;

/**Represents an implementation for loading information into a view or
	saving information from a view.
@author GarretWilson
@see com.garretwilson.util.Modifiable
*/
public interface ViewIOKit<M> extends URIAccessible
{

	/**Loads data into a view from a given URI.
	@param view The view into which the data should be loaded.
	@param uri The URI that identifies the resource to be loaded.
	@exception IOException Thrown if there is an error reading the data.
	*/
	public void load(final ModelView<M> view, final URI uri) throws IOException;

	/**Loads data into a view from an input stream.
	@param view The view into which the data should be loaded.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the content, or <code>null</code> if no base
		URI is available.
	@throws IOException Thrown if there is an error reading the data.
	*/ 
	public void load(final ModelView<M> view, final InputStream inputStream, final URI baseURI) throws IOException;

	/**Saves a view to a given URI.
	<p>If saving is successful and the view is <code>Modifiable</code>, the
		view's modified status is set to <code>false</code>.</p> 
	<p>A calling program should first call the view's <code>verify()</code>
		method to ensure the data is valid and that the model reflects the
		currently entered data.</p>
	@param view The view the data of which will be saved at the given URI.
	@param uri The URI at which the view data should be saved.
	@exception IOException Thrown if there is an error writing the data.
	@see com.garretwilson.util.Modifiable
	@see Verifiable#verify()
	*/
	public void save(final ModelView<M> view, final URI uri) throws IOException;

	/**Saves a view to an output stream.
	<p>If saving is successful and the view is <code>Modifiable</code>, the
		view's modified status is set to <code>false</code>.</p> 
	<p>A calling program should first call the view's <code>verify()</code>
		method to ensure the data is valid and that the model reflects the
		currently entered data.</p>
	@param view The view the data of which will be written to the given output stream.
	@param outputStream The output stream to which to write the model content.
	@throws IOException Thrown if there is an error writing the data.
	@see com.garretwilson.util.Modifiable
	@see Verifiable#verify()
	*/
	public void save(final ModelView<M> view, final OutputStream outputStream) throws IOException;

}
