package com.garretwilson.rdf;

import java.io.*;
import java.net.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

import javax.xml.parsers.*;

import static com.garretwilson.lang.Objects.*;

import com.garretwilson.text.xml.XMLSerializer;
import com.garretwilson.text.xml.XMLUtilities;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**Base functionality for loading and saving information stored in RDF+XML.
@param <T> The type to read and write.
@author Garret Wilson
*/
public abstract class AbstractRDFXMLIO<T> implements RDFIO<T>
{

	/**The class representing the type of object being loaded and saved.*/
	private final Class<T> objectClass;

		/**@return The class representing the type of object being loaded and saved.*/
		public Class<T> getObjectClass() {return objectClass;}

	/**A map of resource factories, keyed to namespace URI.*/
	private final Map<URI, RDFResourceFactory> resourceFactoryMap=new ConcurrentHashMap<URI, RDFResourceFactory>();

		/**Registers a resource factory to be used to create resources with a type from the specified namespace. If a resource factory is already registered for this namespace, it will be replaced.
		@param typeNamespaceURI The namespace of the resource type for which this factory should be used to create objects.
		@param factory The resource factory that will be used to create resources of types from this namespace.
		*/
		public void registerResourceFactory(final URI typeNamespaceURI, final RDFResourceFactory factory)
		{
			resourceFactoryMap.put(typeNamespaceURI, factory);
		}

		/**Removes the resource factory being used to create resources with a type from the specified namespace. If there is no resource factory registered for this namespace, no action will be taken.
		@param typeNamespaceURI The namespace of the resource type for which this factory should be used to create objects.
		*/
		public void unregisterResourceFactory(final URI typeNamespaceURI)
		{
			resourceFactoryMap.remove(typeNamespaceURI);
		}

	/**The map of XML serialization prefixes, keyed by namespace URIs.*/
	private final Map<URI, String> namespaceURIPrefixMap=new HashMap<URI, String>();

		/**Registers the given XML serialization prefix to be used with the given namespace URI.
		If a prefix is already registered with the given namespace, it is replaced with this prefix.
		@param namespaceURI The namespace URI.
		@param prefix The XML serialization prefix to use with the given namespace.
		*/
		public void registerNamespacePrefix(final URI namespaceURI, final String prefix)
		{
			namespaceURIPrefixMap.put(namespaceURI, prefix);	//store the prefix in the map, keyed to the URI
		}

		/**Unregisters the XML serialization prefix for the given namespace URI.
		If no prefix is registered for the given namespace, no action occurs.
		@param namespaceURI The namespace URI.
		*/
		public void unregisterNamespacePrefix(final String namespaceURI, final String prefix)
		{
			namespaceURIPrefixMap.remove(namespaceURI);	//remove whatever prefix is registered with this namespace, if any
		}

	/**Class constructor.
	@param objectClass The class representing the type of object being loaded and saved.
	@exception NullPointerException if the given class is <code>null</code>.
	*/
	public AbstractRDFXMLIO(final Class<T> objectClass)
	{
		this.objectClass=checkInstance(objectClass, "Object class must be provided.");
	}

	/**Creates an RDF instance for use in reading RDF data.
	This version creates a default RDF instance and then registers known resource factories.
	@param baseURI The base URI of the RDF data model, or <code>null</code> if unknown.
	@return An RDF instance appropriate for populating with data read from some source.
	*/
	protected RDF createRDF(final URI baseURI)
	{
		final RDF rdf=new RDF(baseURI);  //create a new RDF data model, indicating the base URI
		for(final Map.Entry<URI, RDFResourceFactory> resourceFactoryEntry:resourceFactoryMap.entrySet())	//for each registered resource factory
		{
			rdf.registerResourceFactory(resourceFactoryEntry.getKey(), resourceFactoryEntry.getValue());  //register the factory with the RDF instance				
		}
		return rdf;	//return the RDF with registered resource factories
	}

	/**Creates a document builder appropriate for parsing XML storing RDF.
	@return A new namespace-aware document builder.
	@exception ParserConfigurationException if a document builder cannot be created which satisfies the configuration requested.
	*/
	protected DocumentBuilder createDocumentBuilder() throws ParserConfigurationException
	{
		return XMLUtilities.createDocumentBuilder(true);	//create a document builder that understands namespaces
	}

	/**Reads a resource from an input stream.
	This version delegates to {@link #read(RDF, InputStream, URI)} using {@link #createRDF(URI)} to create a new RDF instance.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The resource read from the input stream.
	@exception NullPointerException if the given input stream is <code>null</code>.
	@exception IOException if there is an error reading the data.
	@exception ClassCastException if no appropriate resource factory was installed, and the loaded resource is not of the correct Java class.
	*/ 
	public final T read(final InputStream inputStream, final URI baseURI) throws IOException
	{
		return read(createRDF(baseURI), inputStream, baseURI);	//create a new RDF data model, showing the base URI, and read and return the object
	}

	/**Reads a resource from an input stream using an existing RDF instance.
	@param rdf The RDF instance to use in creating new resources.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The resource read from the input stream.
	@exception NullPointerException if the given RDF instance and/or input stream is <code>null</code>.
	@exception IOException if there is an error reading the data.
	@exception ClassCastException if no appropriate resource factory was installed, and the loaded resource is not of the correct Java class.
	*/ 
	public abstract T read(final RDF rdf, final InputStream inputStream, final URI baseURI) throws IOException;

	/**Reads RDF data from an input stream.
	@param rdf The RDF instance to use in creating new resources.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The RDF instance representing the data read.
	@exception IOException if there is an error reading the data.
	*/ 
	protected RDF readRDF(final RDF rdf, final InputStream inputStream, final URI baseURI) throws IOException
	{
		try
		{
			final DocumentBuilder documentBuilder=createDocumentBuilder();	//create a new namespace-aware document builder
			final Document document=documentBuilder.parse(inputStream);	//parse the input stream
			document.normalize(); //normalize the document
			final RDFXMLProcessor rdfProcessor=new RDFXMLProcessor(rdf);	//create a new RDF processor
			rdfProcessor.processRDF(document, baseURI);  //parse the RDF from the document
			return rdf;	//return the RDF we processed
		}
		catch(final ParserConfigurationException parserConfigurationException)	//if we can't find an XML parser
		{
			throw (IOException)new IOException(parserConfigurationException.getMessage()).initCause(parserConfigurationException);	//convert the exception into an IO exception
		}
		catch(final SAXException saxException)
		{
			throw (IOException)new IOException(saxException.getMessage()).initCause(saxException);	//convert the exception into an IO exception
		}
		catch(URISyntaxException uriSyntaxException)	//if any of the URIs were incorrect
		{
			throw (IOException)new IOException(uriSyntaxException.getMessage()).initCause(uriSyntaxException);	//convert the exception into an IO exception
		}
	}

	/**Generates an XML document representing the given RDF resource.
	@param resource The RDF resource to represent as XML.
	@return An XML document representing the given RDF resource.
	@exception ParserConfigurationException if a document builder cannot be created which satisfies the configuration requested.
	*/
/*TODO fix if needed
	protected Document generateXML(final RDFResource resource) throws ParserConfigurationException
	{
		final DocumentBuilder documentBuilder=createDocumentBuilder();	//create a new document builder appropriate for writing RDF
			//create an XML document containing the resource TODO see about using a common RDFXMLifier
		return new RDFXMLGenerator().createDocument(resource, documentBuilder.getDOMImplementation());		
	}
*/

	/**Writes an RDF resource to an output stream.
	@param outputStream The output stream to which to write the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@param resource The resource to write to the given output stream.
	@throws IOException Thrown if there is an error writing the data.
	*/
	protected void writeRDFResource(final OutputStream outputStream, final URI baseURI, final RDFResource resource) throws IOException
	{
		try
		{
			final DocumentBuilder documentBuilder=createDocumentBuilder();	//create a new document builder appropriate for writing RDF
			final RDFXMLGenerator rdfXMLGenerator=new RDFXMLGenerator(baseURI);	//create a new RDF XML generator
			for(Map.Entry<URI, String> namespaceURIPrefixEntry:namespaceURIPrefixMap.entrySet())	//for each entry in our namespace URI prefix map
			{
				rdfXMLGenerator.registerNamespacePrefix(namespaceURIPrefixEntry.getKey(), namespaceURIPrefixEntry.getValue());	//transfer the namespace/prefix pair to the generator
			}
			final Document document=rdfXMLGenerator.createDocument(resource, documentBuilder.getDOMImplementation());	//create an XML document containing the resource
			final XMLSerializer xmlSerializer=new XMLSerializer(true);  //create a formatted serializer
//TODO del if not needed			xmlSerializer.setNamespacesDocumentElementDeclarations(true);	//declare namespaces on the document element
			xmlSerializer.serialize(document, outputStream);	//serialize the document to the output stream
		}
		catch(final ParserConfigurationException parserConfigurationException)	//if we can't find an XML parser
		{
			throw (IOException)new IOException(parserConfigurationException.getMessage()).initCause(parserConfigurationException);	//convert the exception into an IO exception
		}
	}

}
