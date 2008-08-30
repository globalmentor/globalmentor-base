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

package com.globalmentor.urf.maqro;

import java.io.*;
import java.net.*;
import java.util.*;

import static com.globalmentor.urf.maqro.MAQRO.*;

import com.globalmentor.io.*;
import com.globalmentor.rdf.*;
import com.globalmentor.text.xml.XMLDOMImplementation;
import com.globalmentor.text.xml.XMLProcessor;
import com.globalmentor.text.xml.XMLSerializer;

import org.w3c.dom.Document;

/**Class for loading and saving outcomes.
@author Garret Wilson
@deprecated
*/
public class OutcomeIOKit extends AbstractIOKit<Outcome>	//TODO del class
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
	@return The outcome loaded from the input stream.
	@throws IOException Thrown if there is an error reading the data.
	*/ 
	public Outcome load(final InputStream inputStream, final URI baseURI) throws IOException
	{
		try
		{
			final RDF rdf=new RDF();  //create a new RDF data model
//TODO fix			rdf.registerResourceFactory(MAQRO_NAMESPACE_URI, new MAQRO());  //register a factory for MAQRO resource classes
			final XMLProcessor xmlProcessor=new XMLProcessor(this);	//create an XML processor using the correct input stream locator	TODO get the XML processor in a more general way
			final Document document=xmlProcessor.parseDocument(inputStream, baseURI);	//parse the file
			document.normalize(); //normalize the package description document
			final RDFXMLProcessor rdfProcessor=new RDFXMLProcessor(rdf);	//create a new RDF processor
			rdfProcessor.processRDF(document, baseURI);  //parse the RDF from the document
			final Map<RDFResource, Set<RDFResource>> referenceMap=rdf.getReferences();	//get all references to resources
				//get the outcomes from the data model, and find the root outcome
			for(final RDFResource outcomeResource:RDFResources.getResourcesByType(rdf, MAQRO_NAMESPACE_URI, OUTCOME_CLASS_NAME))
			{
				if(outcomeResource instanceof Outcome)	//if this is an outcome
				{
					final Set<RDFResource> referenceSet=referenceMap.get(outcomeResource);	//find out how many refereces there are to this outcome
					if(referenceSet==null || referenceSet.size()==0)	//if there are no references to this outcome
					{
						return (Outcome)outcomeResource;	//return the outcome
					}
				}
			}
			throw new IOException("No outcome found.");	//TODO i18n
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
/*TODO fix
		//create an XML document containing the activity TODO see about using a common RDFXMLifier
		final Document document=new RDFXMLGenerator().createDocument(outcome, new XMLDOMImplementation());	//TODO get the XMLDOMImplementation from some common source
		final XMLSerializer xmlSerializer=new XMLSerializer(true);  //create a formatted serializer
		xmlSerializer.serialize(document, outputStream);	//serialize the document to the output stream
*/
	}

}
