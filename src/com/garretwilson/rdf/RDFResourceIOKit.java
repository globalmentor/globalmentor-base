package com.garretwilson.rdf;

import java.io.*;
import java.net.*;
import java.util.*;
import com.garretwilson.io.*;
import com.garretwilson.model.*;
import com.garretwilson.text.xml.XMLDOMImplementation;
import com.garretwilson.text.xml.XMLProcessor;
import com.garretwilson.text.xml.XMLSerializer;
import org.w3c.dom.Document;

/**Class for loading and saving an RDF resource from an RDF+XML serialization.
@author Garret Wilson
@see RDFResource
*/
public class RDFResourceIOKit<R extends RDFResource> extends AbstractIOKit<R>
{

	/**The map of prefixes, keyed by namespace URIs.*/
	protected final Map<URI, String> namespacePrefixMap=new HashMap<URI, String>();

		/**@return The map of prefixes, keyed by namespace URI.*/
		protected Map<URI, String> getNamespacePrefixMap() {return namespacePrefixMap;}
		
		/**Registers the given prefix to be used with the given namespace URI.
			If a prefix is already registered with the given namespace, it is
			replaced with this prefix.
		<p>Registered namespace prefixes will be registered with the created RDF
			XMLifier, and also added to the document element of the XML document
			to be serialized.</p> 
		@param namespaceURI The XML namespace.
		@param prefix The serialization prefix to use with the given namespace.
		@see #getRDFXMLifier()
		@see #store()
		*/
		public void registerNamespacePrefix(final URI namespaceURI, final String prefix)
		{
			namespacePrefixMap.put(namespaceURI, prefix);	//store the prefix in the map, keyed to the URI
		}

		/**Unregisters the prefix for the given namespace URI.
			If no prefix is registered for the given namespace, no action occurs.
		@param namespaceURI The XML namespace.
		*/
		public void unregisterNamespacePrefix(final URI namespaceURI, final String prefix)
		{
			namespacePrefixMap.remove(namespaceURI);	//remove whatever prefix is registered with this namespace, if any
		}

	/**A map of resource factories, keyed to namespace URI.*/
	private final Map<URI, RDFResourceFactory> resourceFactoryMap=new HashMap<URI, RDFResourceFactory>();

		/**Registers a resource factory to be used to create resources with a type
			from the specified namespace. If a resource factory is already registered
			for this namespace, it will be replaced.
		<p>Registered resource factories will be registered with the created RDF
			data model.</p> 
		@param typeNamespaceURI The namespace of the resource type for which this
			factory should be used to create objects.
		@param factory The resource factory that will be used to create resources
			of types from this this namespace.
		*/
		public void registerResourceFactory(final URI typeNamespaceURI, final RDFResourceFactory factory)
		{
			resourceFactoryMap.put(typeNamespaceURI, factory);
		}

		/**Removes the resource factory being used to create resources with a type
			from the specified namespace. If there is no resource factory registered
			for this namespace, no action will be taken.
		@param typeNamespaceURI The namespace of the resource type for which this
			factory should be used to create objects.
		*/
		public void unregisterResourceFactory(final URI typeNamespaceURI)
		{
			resourceFactoryMap.remove(typeNamespaceURI);
		}

	/**The namespace of the resource type to serialize.*/
	private final URI namespaceURI;

		/**@return The namespace of the resource type to serialize.*/
		protected URI getNamespaceURI() {return namespaceURI;}

	/**The local name of the resource type to serialize.*/
	private final String className;

		/**@return The local name of the resource type to serialize.*/
		protected String getClassName() {return className;}

	/**The factory to be registered with the resource
		namespace for creating resources, or <code>null</code> if no resource
		factory be registered with the namespace
	*/
	private final RDFResourceFactory resourceFactory;

		/**@return The factory to be registered with the resource
			namespace for creating resources, or <code>null</code> if no resource
			factory be registered with the namespace
		*/
		protected RDFResourceFactory getResourceFactory() {return resourceFactory;}

	/**Constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	@param namespaceURI The namespace of the resource type to serialize.
	@param className The local name of the resource type to serialize.
	*/
	public RDFResourceIOKit(final URIInputStreamable uriInputStreamable, final URIOutputStreamable uriOutputStreamable, final URI namespaceURI, final String className) 
	{
		this(uriInputStreamable, uriOutputStreamable, namespaceURI, className, null);	//construct the class with no resource factory
	}

	/**Constructor with optional resource factory.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	@param namespaceURI The namespace of the resource type to serialize.
	@param className The local name of the resource type to serialize.
	@param resourceFactory The factory to be registered with the resource
		namespace for creating resources, or <code>null</code> if no resource
		factory be registered with the namespace.
	*/
	public RDFResourceIOKit(final URIInputStreamable uriInputStreamable, final URIOutputStreamable uriOutputStreamable, final URI namespaceURI, final String className, final RDFResourceFactory resourceFactory) 
	{
		super(uriInputStreamable, uriOutputStreamable);	//construct the parent class
		this.namespaceURI=namespaceURI;
		this.className=className;
		this.resourceFactory=resourceFactory;
		if(resourceFactory!=null)	//if we were given a resource factory
		{
			registerResourceFactory(namespaceURI, resourceFactory);  //register a factory for the namespace
		}
	}

	/**@return An XML serializer appropriately configured for storing the RDF XML.
	<p>Registered namespace prefixes are registered with the XMLifier.</p>
	@see RDFXMLGenerator#registerNamespacePrefix
	*/
	protected RDFXMLGenerator getRDFXMLifier()
	{
		final RDFXMLGenerator rdfXMLifier=new RDFXMLGenerator();	//create an object to convert the RDF data model to an XML data model
		for(final Map.Entry<URI, String> namespacePrefixEntry:namespacePrefixMap.entrySet())	//look through all namespace URIs and prefixes
		{
			final URI namespaceURI=namespacePrefixEntry.getKey();	//get the namespace URI
			final String namespacePrefix=namespacePrefixEntry.getValue();	//get the namespace prefix
			rdfXMLifier.registerNamespacePrefix(namespaceURI.toString(), namespacePrefix);	//register this prefix for the this namespace
		}
		return rdfXMLifier;	//return the XMLifier we created
	}
	
	/**@return An XML serializer appropriately configured for storing the RDF XML.*/
	protected XMLSerializer getXMLSerializer()
	{
		return new XMLSerializer(true);	//create a formatted XML serializer
	}

	/**@return An XML processor appropriately configured for parsing XML.*/
	protected XMLProcessor getXMLProcessor()
	{
		return new XMLProcessor(this);	//create an XML processor using the correct input stream locator
	}
	
	/**Loads an RDF resource from an input stream.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the content, or <code>null</code> if no base
		URI is available.
	@return The RDF resource loaded from the input stream.
	@throws IOException Thrown if there is an error reading the data.
	*/ 
	public R load(final InputStream inputStream, final URI baseURI) throws IOException
	{
		try
		{
			final RDF rdf=new RDF(baseURI);  //create a new RDF data model, showing the base URI
			final Iterator resourceFactoryEntryIterator=resourceFactoryMap.entrySet().iterator();	//get an iterator to look through all resource factories
			while(resourceFactoryEntryIterator.hasNext())	//while there are more resource factories
			{
				final Map.Entry resourceFactoryEntry=(Map.Entry)resourceFactoryEntryIterator.next();	//get the next entry
				final URI typeNamespaceURI=(URI)resourceFactoryEntry.getKey();	//get the type namespace URI
				final RDFResourceFactory resourceFactory=(RDFResourceFactory)resourceFactoryEntry.getValue();	//get the resource factory associated with this type
				rdf.registerResourceFactory(typeNamespaceURI, resourceFactory);	//register this resource factory for this type namespace
			}
			final XMLProcessor xmlProcessor=getXMLProcessor();	//get the XML processor
			final Document document=xmlProcessor.parseDocument(inputStream, baseURI);	//parse the activity file
			document.normalize(); //normalize the package description document
			final RDFXMLProcessor rdfProcessor=new RDFXMLProcessor(rdf);	//create a new RDF processor
			rdfProcessor.processRDF(document, baseURI);  //parse the RDF from the document
				//get the designated resource from the data model
			final RDFResource resource=RDFUtilities.getResourceByType(rdf, getNamespaceURI(), getClassName());
			if(resource==null)	//if there is no such type of resource
			{
				throw new IOException("No resource found of type "+RDFUtilities.createReferenceURI(getNamespaceURI(), getClassName()));	//G***i18n
			}
			return (R)resource;	//return the resource we read TODO make sure the resource is of the correct type somehow 
		}
		catch(URISyntaxException uriSyntaxException)	//if any of the URIs were incorrect
		{
			throw (IOException)new IOException(uriSyntaxException.getMessage()).initCause(uriSyntaxException);	//convert the exception into an IO exception
		}
	}
	
	/**Saves a model to an output stream.
	@param resource The resource which will be written to the given output stream.
	@param outputStream The output stream to which to write the model content.
	@throws IOException Thrown if there is an error writing the model.
	*/
	public void save(final R resource, final OutputStream outputStream) throws IOException
	{
			//create an XML document containing the resource
		final Document document=getRDFXMLifier().createDocument((RDFResource)resource, new XMLDOMImplementation());	//TODO get the XMLDOMImplementation from some common source
		getXMLSerializer().serialize(document, outputStream);	//serialize the document to the output stream
	}

}
