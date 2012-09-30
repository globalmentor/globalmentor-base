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

package org.urframework.maqro;

import java.io.*;
import java.net.*;

import com.globalmentor.io.*;
import com.globalmentor.net.ResourceModel;
import com.globalmentor.rdf.*;
import com.globalmentor.text.xml.*;

import static org.urframework.maqro.MAQRO.*;

import org.w3c.dom.Document;

/**Class for loading and saving activities.
@author Garret Wilson
@see ActivityModel
@deprecated
*/
public class ActivityModelIOKit extends AbstractIOKit<ResourceModel<Activity>>	//TODO del class
{

	/**Default constructor.*/
	public ActivityModelIOKit()
	{
		this(null, null);
	}

	/**URI input stream locator constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	*/
	public ActivityModelIOKit(final URIInputStreamable uriInputStreamable)
	{
		this(uriInputStreamable, null);
	}

	/**URI output stream locator constructor.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public ActivityModelIOKit(final URIOutputStreamable uriOutputStreamable)
	{
		this(null, uriOutputStreamable);
	}

	/**Full constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public ActivityModelIOKit(final URIInputStreamable uriInputStreamable, final URIOutputStreamable uriOutputStreamable)
	{
		super(uriInputStreamable, uriOutputStreamable);	//construct the parent class
	}

	/**Loads a model from an input stream.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the content, or <code>null</code> if no base
		URI is available.
	@throws IOException Thrown if there is an error reading the data.
	*/ 
	public ResourceModel<Activity> load(final InputStream inputStream, final URI baseURI) throws IOException
	{
		try
		{
			final RDF rdf=new RDF();  //create a new RDF data model
//TODO fix			rdf.registerResourceFactory(MAQRO_NAMESPACE_URI, new MAQRO());  //register a factory for MAQRO resource classes
			final Document document=XML.parse(inputStream, baseURI, true, new URIInputStreamableXMLEntityResolver(this));	//create an XML processor using the correct input streams locator and parse the activity file
			document.normalize(); //normalize the package description document
			final RDFXMLProcessor rdfProcessor=new RDFXMLProcessor(rdf);	//create a new RDF processor
			rdfProcessor.processRDF(document, baseURI);  //parse the RDF from the document
				//get an activity from the data model
			final Activity activity=(Activity)RDFResources.getResourceByType(rdf, MAQRO_NAMESPACE_URI, ACTIVITY_CLASS_NAME);
			if(activity==null)	//if there is no activity
			{
				throw new IOException("No activity found.");	//G***i18n
			}
			return new ResourceModel<Activity>(activity, baseURI, this);	//create and return an activity model from the activity TODO we should really pass back the underlying stream accessors rather than using the IO kit
		}
		catch(URISyntaxException uriSyntaxException)	//if any of the URIs were incorrect
		{
			throw (IOException)new IOException(uriSyntaxException.getMessage()).initCause(uriSyntaxException);	//convert the exception into an IO exception
		}
	}
	
	/**Saves a model to an output stream.
	@param model The model the data of which will be written to the given output stream.
	@param outputStream The output stream to which to write the model content.
	@throws IOException Thrown if there is an error writing the model.
	*/
	public void save(final ResourceModel<Activity> model, final OutputStream outputStream) throws IOException
	{
		write(model.getResource(), outputStream);	//write the activity to the output stream
	}

	/**Writes an activity to an output stream.
	@param activity The activity which will be written to the given output stream.
	@param outputStream The output stream to which to write the activity.
	@throws IOException Thrown if there is an error writing the activity.
	*/
	public static void write(final Activity activity, final OutputStream outputStream) throws IOException
	{
/*TODO fix
			//create an XML document containing the activity TODO see about using a common RDFXMLifier
		final Document document=new RDFXMLGenerator().createDocument(activity, new XMLDOMImplementation());	//TODO get the XMLDOMImplementation from some common source
		final XMLSerializer xmlSerializer=new XMLSerializer(true);  //create a formatted serializer
		xmlSerializer.serialize(document, outputStream);	//serialize the document to the output stream
*/
	}
}
