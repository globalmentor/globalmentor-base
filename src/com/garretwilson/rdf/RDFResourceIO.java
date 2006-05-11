package com.garretwilson.rdf;

import java.io.*;
import java.net.*;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.xml.parsers.*;

import com.garretwilson.io.IO;

import static com.garretwilson.lang.ClassUtilities.*;
import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.rdf.RDFUtilities.*;
import static com.garretwilson.text.xml.XMLUtilities.createDocumentBuilder;

import com.garretwilson.text.xml.XMLSerializer;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**Class for loading and saving a particular type of RDF resource.
Custom classes can be added using {@link #registerResourceFactory(URI, RDFResourceFactory)}.
@param <T> The type to read and write.
@author Garret Wilson
*/
public class RDFResourceIO<T extends RDFResource> implements IO<T>
{

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

	/**The class representing the type of resource expected from the RDF instance.*/
	private final Class<T> resourceClass;

		/**@return The class representing the type of resource expected from the RDF instance.*/
		public Class<T> getResourceClass() {return resourceClass;}

	/**The namespace of the RDF resource supported.*/
	private final URI resourceNamespaceURI;

		/**@return The namespace of the RDF resource supported.*/
		public URI getResourceNamespaceURI() {return resourceNamespaceURI;}

	/**The class name of the RDF resource supported.*/
	private final String resourceClassName;

		/**@return The class name of the RDF resource supported.*/
		public String getResourceClassName() {return resourceClassName;}

	/**Class and namespace constructor.
	A {@link DefaultRDFResourceFactory} will be registered with the given namespace, configured to create Java instances from the package of the resource class.
	The class name will be set to the local name of the given resource class.
	@param resourceClass The class representing the type of resource expected from the RDF instance.
	@param resourceNamespaceURI The namespace of the RDF resource supported.
	@exception NullPointerException if the given class and/or namespace URI is <code>null</code>.
	*/
	public RDFResourceIO(final Class<T> resourceClass, final URI resourceNamespaceURI)
	{
		this(resourceClass, resourceNamespaceURI, getLocalName(resourceClass), new DefaultRDFResourceFactory(resourceClass.getPackage()));	//construct the class with a default resource factory
	}

	/**Class, namespace, and class name constructor.
	@param resourceClass The class representing the type of resource expected from the RDF instance.
	@param resourceNamespaceURI The namespace of the RDF resource supported.
	@param resourceClassName The class name of the RDF resource supported.
	@exception NullPointerException if the given class, namespace URI, and/or class name is <code>null</code>.
	*/
	public RDFResourceIO(final Class<T> resourceClass, final URI resourceNamespaceURI, final String resourceClassName)
	{
		this.resourceClass=checkInstance(resourceClass, "Resource class must be provided.");
		this.resourceNamespaceURI=checkInstance(resourceNamespaceURI, "Resource namespace URI must be provided.");
		this.resourceClassName=checkInstance(resourceClassName, "Resource class name must be provided.");
	}

	/**Type constructor with resource factory.
	@param resourceClass The class representing the type of resource expected from the RDF instance.
	@param resourceNamespaceURI The namespace of the RDF resource supported.
	@param resourceClassName The class name of the RDF resource supported.
	@param resouceFactory The resource factory to register with the given namespace.
	@exception NullPointerException if the given class, namespace URI, class name, and/or resource factory is <code>null</code>.
	*/
	public RDFResourceIO(final Class<T> resourceClass, final URI resourceNamespaceURI, final String resourceClassName, final RDFResourceFactory resourceFactory)
	{
		this(resourceClass, resourceNamespaceURI, resourceClassName);	//construct the class
		registerResourceFactory(resourceNamespaceURI, checkInstance(resourceFactory, "Resource factory cannot be null."));  //register the factory for the given namespace
	}

	/**Reads a resource from an input stream.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The resource read from the input stream.
	@exception IOException if there is an error reading the data.
	@exception ClassCastException if no appropriate resource factory was installed, and the loaded resource is not of the correct Java class.
	*/ 
	public T read(final InputStream inputStream, final URI baseURI) throws IOException
	{
		try
		{
			final RDF rdf=new RDF(baseURI);  //create a new RDF data model, showing the base URI
			for(final Map.Entry<URI, RDFResourceFactory> resourceFactoryEntry:resourceFactoryMap.entrySet())	//for each registered resource factory
			{
				rdf.registerResourceFactory(resourceFactoryEntry.getKey(), resourceFactoryEntry.getValue());  //register the factory with the RDF instance				
			}
			final DocumentBuilder documentBuilder=createDocumentBuilder(true);	//create a new namespace-aware document builder
			final Document document=documentBuilder.parse(inputStream);	//parse the input stream
			document.normalize(); //normalize the document
			final RDFXMLProcessor rdfProcessor=new RDFXMLProcessor(rdf);	//create a new RDF processor
			rdfProcessor.processRDF(document, baseURI);  //parse the RDF from the document
			final RDFResource resource=getResourceByType(rdf, getResourceNamespaceURI(), getResourceClassName());	//load the correct resource
			if(resource==null)	//if there is no resource
			{
				throw new IOException("No resource found in namespace "+getResourceNamespaceURI()+" with class name "+getResourceClassName()+".");	//G***i18n
			}
			return getResourceClass().cast(resource);	//cast the resource to the correct type and return it
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
	
	/**Writes a resource to an output stream.
	@param outputStream The output stream to which to write the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@param resource The resource to write to the given output stream.
	@throws IOException Thrown if there is an error writing the data.
	*/
	public void write(final OutputStream outputStream, final URI baseURI, final T resource) throws IOException
	{
		try
		{
			final DocumentBuilder documentBuilder=createDocumentBuilder(true);	//create a new namespace-aware document builder
			//create an XML document containing the resource TODO see about using a common RDFXMLifier
			final Document document=new RDFXMLifier().createDocument(resource, documentBuilder.getDOMImplementation());	//TODO get the XMLDOMImplementation from some common source
			final XMLSerializer xmlSerializer=new XMLSerializer(true);  //create a formatted serializer
			xmlSerializer.serialize(document, outputStream);	//serialize the document to the output stream
		}
		catch(final ParserConfigurationException parserConfigurationException)	//if we can't find an XML parser
		{
			throw (IOException)new IOException(parserConfigurationException.getMessage()).initCause(parserConfigurationException);	//convert the exception into an IO exception
		}
	}

}
