package com.garretwilson.rdf.maqro;

import java.io.*;
import java.net.*;
import com.garretwilson.io.*;
import com.garretwilson.model.*;
import com.garretwilson.rdf.*;
import static com.garretwilson.rdf.maqro.MAQROConstants.*;
import com.garretwilson.text.xml.XMLDOMImplementation;
import com.garretwilson.text.xml.XMLProcessor;
import com.garretwilson.text.xml.XMLSerializer;
import org.w3c.dom.Document;

/**Class for loading and saving activities.
@author Garret Wilson
@see ActivityModel
*/
public class ActivityModelIOKit extends AbstractIOKit<ResourceModel<Activity>>
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
			final RDF rdf=new RDF(baseURI);  //create a new RDF data model, showing the base URI
			rdf.registerResourceFactory(MAQRO_NAMESPACE_URI, new MAQROUtilities());  //register a factory for MAQRO resource classes

			final XMLProcessor xmlProcessor=new XMLProcessor(this);	//create an XML processor using the correct input stream locator
			final Document document=xmlProcessor.parseDocument(inputStream, baseURI);	//parse the activity file
			document.normalize(); //normalize the package description document
			final RDFXMLProcessor rdfProcessor=new RDFXMLProcessor(rdf);	//create a new RDF processor
			rdfProcessor.process(document, baseURI);  //parse the RDF from the document
				//get an activity from the data model
			final Activity activity=(Activity)RDFUtilities.getResourceByType(rdf, MAQRO_NAMESPACE_URI, ACTIVITY_CLASS_NAME);
			if(activity==null)	//if there is no activity
			{
				throw new IOException("No activity found.");	//G***i18n
			}
			return new ResourceModel<Activity>(activity, baseURI, this);	//create and return an activity model from the activity
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
			//create an XML document containing the activity TODO see about using a common RDFXMLifier
		final Document document=new RDFXMLifier().createDocument(activity, new XMLDOMImplementation());	//TODO get the XMLDOMImplementation from some common source
		final XMLSerializer xmlSerializer=new XMLSerializer(true);  //create a formatted serializer
		xmlSerializer.serialize(document, outputStream);	//serialize the document to the output stream
	}
}
