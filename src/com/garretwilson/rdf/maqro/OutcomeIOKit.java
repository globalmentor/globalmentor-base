package com.garretwilson.rdf.maqro;

import java.io.*;
import java.net.*;

import com.garretwilson.io.*;
import com.garretwilson.rdf.*;
import static com.garretwilson.rdf.maqro.MAQROConstants.*;
import com.garretwilson.text.xml.XMLDOMImplementation;
import com.garretwilson.text.xml.XMLProcessor;
import com.garretwilson.text.xml.XMLSerializer;
import org.w3c.dom.Document;

/**Class for loading and saving outcomes.
@author Garret Wilson
*/
public class OutcomeIOKit extends AbstractIOKit<Outcome>
{

	/**Default constructor.*/
	public OutcomeIOKit()
	{
		this(null, null);
	}

	/**URI input stream locator constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	*/
	public OutcomeIOKit(final URIInputStreamable uriInputStreamable)
	{
		this(uriInputStreamable, null);
	}

	/**URI output stream locator constructor.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public OutcomeIOKit(final URIOutputStreamable uriOutputStreamable)
	{
		this(null, uriOutputStreamable);
	}

	/**Full constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public OutcomeIOKit(final URIInputStreamable uriInputStreamable, final URIOutputStreamable uriOutputStreamable)
	{
		super(uriInputStreamable, uriOutputStreamable);	//construct the parent class
	}

	/**Loads an outcome from an input stream.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the content, or <code>null</code> if no base
		URI is available.
	@throws IOException Thrown if there is an error reading the data.
	*/ 
	public Outcome load(final InputStream inputStream, final URI baseURI) throws IOException
	{
		try
		{
			final RDF rdf=new RDF(baseURI);  //create a new RDF data model, showing the base URI
			rdf.registerResourceFactory(MAQRO_NAMESPACE_URI, new MAQROUtilities());  //register a factory for MAQRO resource classes
			final XMLProcessor xmlProcessor=new XMLProcessor(this);	//create an XML processor using the correct input stream locator	TODO get the XML processor in a more general way
			final Document document=xmlProcessor.parseDocument(inputStream, baseURI);	//parse the file
			document.normalize(); //normalize the package description document
			final RDFXMLProcessor rdfProcessor=new RDFXMLProcessor(rdf);	//create a new RDF processor
			rdfProcessor.process(document, baseURI);  //parse the RDF from the document
				//get the outcomes from the data model
			for(final RDFResource outcomeResource:RDFUtilities.getResourcesByType(rdf, MAQRO_NAMESPACE_URI, OUTCOME_CLASS_NAME))
			{
				if(outcomeResource instanceof Outcome)	//if this is an outcome
				{
					final Outcome outcome=(Outcome)outcomeResource;	//cast the resource to an outcome
					if(outcome.getInteraction()!=null)	//if this outcome is for an interaction TODO fix better; make sure this is for the assessment
					{
						return outcome;	//return the outcome we found
					}
				}
			}
			throw new IOException("No outcome found.");	//G***i18n
		}
		catch(URISyntaxException uriSyntaxException)	//if any of the URIs were incorrect
		{
			throw (IOException)new IOException(uriSyntaxException.getMessage()).initCause(uriSyntaxException);	//convert the exception into an IO exception
		}
	}
	
	/**Saves an outcome to an output stream.
	@param outcome The outcome which will be written to the given output stream.
	@param outputStream The output stream to which to write the outcome content.
	@throws IOException Thrown if there is an error writing the data.
	*/
	public void save(final Outcome outcome, final OutputStream outputStream) throws IOException
	{
		//create an XML document containing the activity TODO see about using a common RDFXMLifier
		final Document document=new RDFXMLifier().createDocument(outcome, new XMLDOMImplementation());	//TODO get the XMLDOMImplementation from some common source
		final XMLSerializer xmlSerializer=new XMLSerializer(true);  //create a formatted serializer
		xmlSerializer.serialize(document, outputStream);	//serialize the document to the output stream
	}

}
